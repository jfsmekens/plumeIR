"""
Written by @jfsmekens - Last updated 28/05/2022

This module contains functions for spectral analysis
Defines two object classes:

    Analyser - an object with functions to perform the spectral analysis
        makeReference           generate OD cross-sections to be used by forward model
        fowardTransmission      forward model for transmission cases (IR source: lamp, sun, moon, lava)
        forwardEmission         forward model for emission cases (no IR source, cold sky background)
        preProcess              contains all the steps to go from raw single beam spectrum to ready to fit
                                (trim, offset, calibrate, calculate difference with clear spectrum if necessary)
        fitSpectrum             optimisation algorithm 
    
    FitResult- an object to hold the fit results
    
"""
import os
import logging

from scipy.optimize import curve_fit
from scipy.interpolate import griddata
import copy
from sklearn.metrics import r2_score, mean_squared_error
from scipy.ndimage import uniform_filter1d

from atmosphere import *
from customXSC import *
from spectra import read_spectrum
from initialise import readConfig, readStandard
from constants import plot_colors, pretty_names, apod_types, atm_temps, gas_temps
from parameters import Retrieval

gas_temps = np.linspace(gas_temps[0], gas_temps[1], 21)   # Range of temperatures for the gas
atm_temps = np.linspace(atm_temps[0], atm_temps[1], 21)   # Range of temperatures for the gas

logger = logging.getLogger(__name__)
continuum_gases = ['H2O']
# ======================================================================================================================
#                                               Planck function
# ======================================================================================================================
def planck(wn, temp, norm=False):
    """
    This function computes the blackbody radiant energy curve using the Planck function for a given temperature

    :param wn: X-axis                                   [cm^-1]
    :param T: Temperature                                   [K]

    :return: Blackbody emission curve                       [mW / (m2 sr cm-1)]
    """
    # Define constants for radiance  [in mW / (m2 sr cm-1)]
    c1 = 1.191042E-5  # mW / (m-2 sr cm-4)
    c2 = 1.4387752  # K cm

    # Create theoretical BB radiance functions
    rad = c1 * wn ** 3 / (np.exp(c2 * wn / temp) - 1)

    if norm:
        return rad / rad.max()
    else:
        return rad

# ======================================================================================================================
#                                    Inverse Planck function
# ======================================================================================================================
def inversePlanck(wn, rad):
    """
    This function computes the blackbody equivalent brightness temperature using the Planck function for a given temperature

    :param wn: X-axis                                   [cm^-1]
    :param rad: Radiance                                    [mW / (m2 sr cm-1)]

    :return: Blackbody emission curve                       [K]
    """
    # Define constants for radiance  [in mW / (m2 sr cm-1)]
    c1 = 1.191042E-5  # mW / (m-2 sr cm-4)
    c2 = 1.4387752  # K cm

    # Create theoretical BB radiance functions
    bbt = c2 * wn / np.log(c1 * wn ** 3 / rad + 1)

    return bbt

# ======================================================================================================================
#                                    Instrument Line Shape function
# ======================================================================================================================

def makeILS(fov=0.03, max_opd=1.8, nper_wn=25, wn=1000, apod_type='NB medium'):

    # Populate kernel array
    out = {}
    out['apod_type'] = apod_type
    out['fov'] = fov
    out['wn'] = wn

    # Relabel variables
    L = max_opd
    total_opd = nper_wn    # Total length of 1-sided interferometer is the resolution
    apod_types = ['Boxcar', 'Uniform', 'Triangular', 'Blackman-Harris', 'Happ-Genzel', 'Hamming', 'Lorenz', 'Gaussian',
                  'NB weak', 'NB medium', 'NB strong', 'Cosine']
    apod_type = apod_type.lower()

    # ----- Build grid based on max OPD -----
    n = int(L * nper_wn)
    if n % 2 == 0:
        n = n + 1
    L_grid = np.linspace(0, L, n)
    n_tot = int(total_opd * nper_wn)
    filler = np.zeros(n_tot - n)

    # ----- Build apodization functions -----
    # Boxcar function (no apodization, sampling function should be perfect sinc)
    if 'boxcar' in apod_type or 'uniform' in apod_type:
        apod = np.ones(n)
    # Triangular function
    elif 'triang' in apod_type:
        apod = 1 - np.abs(L_grid) / L
    # Norton-Beer functions (NB)
    elif 'nb' in apod_type or 'norton' in apod_type or 'beer' in apod_type:
        # 'NB weak'
        if 'weak' in apod_type:
            c = [0.348093, -0.087577, 0.703484, 0.0]
        # NB strong
        elif 'strong' in apod_type:
            c = [0.045335, 0.0, 0.554883, 0.399782]
        # NB medium (default if nothing is specified)
        else:
            c = [0.152442, -0.136176, 0.983734, 0]
        # # Now build
        apod = np.zeros(n)
        for i in range(4):
            apod = apod + c[i] * (1 - (L_grid / L) ** 2) ** i

    # Hamming function (also known as Happ-Genzel function)
    elif 'hamming' in apod_type or 'happ' in apod_type or 'genzel' in apod_type:
        apod = 0.54 + 0.46 * np.cos(np.pi * L_grid / L)
    # Blackman-Harris function
    elif 'blackman' in apod_type or 'harris' in apod_type:
        apod = 0.42 + 0.5 * np.cos(np.pi * L_grid / L) + 0.08 * np.cos(2 * np.pi * L_grid / L)
    # Cosine function
    elif 'cos' in apod_type:
        apod = np.cos(np.pi * L_grid / (2 * L))
    # Lorenz function
    elif 'lorenz' in apod_type:
        apod = np.exp(-np.abs(L_grid) / L)
    elif 'gauss' in apod_type:
        apod = np.exp(-(2.24 * L_grid / L) ** 2)

    else:
        ValueError('Invalid keyword for "apod_type". Please choose from (case insensitive) % s' % apod_types)

    # ----- Take Fourier Transform and get sampling kernel -----
    kernel = np.fft.fft(np.concatenate((apod, filler))) .real    # Add zeros to simulate infinite length and take FT
    middle = n_tot // 2
    kernel = np.concatenate((kernel[middle:], kernel[0:middle]))  # Split in the middle and mirror around wn_grid
    offset = kernel[0:middle // 2].mean(axis=0)
    kernel = kernel - offset        # Remove offset in spectrum

    # Create the wavenumber grid centred on zero
    wn_grid = np.linspace(-total_opd / 2, total_opd / 2, n_tot)

    # Build boxcar filter for FOV effect
    fov_width = wn * (fov/2) ** 2 / 2  # in [cm^-1]
    n_box = int(np.ceil(fov_width * nper_wn))
    if n_box < 0 or n_box > middle:
        n_box = 5
    boxcar = np.ones(n_box) * 1 / n_box
    kernel = np.convolve(kernel, boxcar, 'same')

    # Apply shift due to FOV effect
    shift = fov_width / 2
    shift_wn_grid = wn_grid - shift
    kernel = np.interp(wn_grid, shift_wn_grid, kernel)
    kernel = kernel / kernel.sum(axis=0)

    # Only keep the first 5 limbs
    idx = np.abs(wn_grid) <= 5 / L
    wn_grid = wn_grid[idx]
    kernel = kernel[idx]

    # Populate kernel array
    out['wn_grid'] = wn_grid[0:-1]
    out['kernel'] = kernel[1:]
    out['L_grid'] = L_grid
    out['apod'] = apod
    out['fov_width'] = fov_width

    return out


# ======================================================================================================================
# ======================================== Spectral Analyser Object ====================================================
# ======================================================================================================================

class Analyser(object):
    """
    """

# ======================================================================================================================
#                                               Initialise Object
# ======================================================================================================================
    def __init__(self, params, geometry, fit_window, retrieval=None, model_padding=50, model_spacing=0.04, master=False,
                 name='FIT', data_dir=None, subtract_self=False, fit_size_aero=False, force_ref=False):
        """Initialise the model for the Analyser"""

        # Fit name
        self.name = name
        self.master = master

        # ---------------------------------------------------------------------
        # Define fit parameters
        # ---------------------------------------------------------------------

        # Assign geometry object
        self.geometry = geometry
        self.type = self.geometry.type
        self.subtract_self = subtract_self
        self.fit_size_aero = fit_size_aero
        self.data_dir = data_dir

        # Assign fit parameters and initial guess for fit
        self.params = params.make_copy()
        self.p0 = self.params.fittedValuesList()
        self.bounds = self.params.bounds()

        # If retrieval is supplied use this to extract parameters
        if retrieval is None:
            self.retrieval = Retrieval(type=self.type)
        else:
            self.retrieval = retrieval

        # Are we imposing SCDs from an existing fit?
        if self.retrieval.force_param is not None:
            if isinstance(self.retrieval.force_param, str):
                self.force_param = [self.retrieval.force_param]
            else:
                self.force_param = self.retrieval.force_param
            for p in self.force_param:
                if p in self.params:
                    self.params[p].vary = False
            logger.info('Values for %s imposed from fit: %s' % (self.force_param, self.retrieval.df_dir))
        else:
            self.force_param = []

        # Are we using SCDs from an existing fit as initial parameters?
        if self.retrieval.seed_param is not None:
            if isinstance(self.retrieval.seed_param, str):
                self.seed_param = [self.retrieval.seed_param]
            else:
                self.seed_param = self.retrieval.seed_param
            for p in self.seed_param:
                if p in self.params:
                    self.params[p].vary = True
            logger.info('Using values fot %s as apriori guesses from fit: %s' % (self.seed_param, self.retrieval.df_dir))
        else:
            self.seed_param = []

        # Find the dataframe with the previous fit results and check that it can be used
        self.from_df = self.force_param + self.seed_param
        if len(self.from_df) > 0:
            flist = glob(self.retrieval.df_dir + '/*fitResults*_ALL.csv')
            if len(flist) == 0:
                raise ValueError('Could not find a suitable fitResult file in directory %s'
                                 % self.retrieval.df_dir)
            elif len(flist) == 1:
                df = pd.read_csv(flist[0])
                if all([('%s_val' % x in df.columns for x in self.from_df) or (y in df.columns for y in self.from_df)]):
                    self.df_dir = df
                else:
                    raise IOError('Supplied fitResult does not contain all parameters')
            else:
                raise ValueError('Found multiple (%i) matches for fitResult file in directory %s'
                                 % (len(flist), self.retrieval.df_dir))

        # Generate model wavenumber grid
        self.model_padding = model_padding
        self.model_spacing = model_spacing
        self.nper_wn = 1 / self.model_spacing
        self.fit_window = fit_window
        self.wn_start = self.fit_window[0] - self.model_padding
        self.wn_stop = self.fit_window[1] + self.model_padding
        self.model_grid = np.arange(self.wn_start, self.wn_stop + self.model_spacing, step=self.model_spacing)

        # Get ILS parameters
        self.max_opd = retrieval.max_opd
        self.apod_type = retrieval.apod_type

        # Update info about aerosols
        self.use_Babs_aero = retrieval.use_Babs_aero
        self.fit_size_aero = retrieval.fit_size_aero

        # ---------------------------------------------------------------------
        # Generate calibration and clear sky for emission
        # ---------------------------------------------------------------------
        if self.type == 'layer':
            self.subtract_self = retrieval.subtract_self
            self.no_gas = retrieval.no_gas
            self.use_source_E = retrieval.use_source_E
            self.E_file = retrieval.E_file
            self.use_residual = retrieval.use_residual
            self.res_dir = retrieval.res_dir
        elif self.type == 'emission':
            self.fit_difference = retrieval.fit_difference
            self.bb_drift = retrieval.bb_drift
            self.clear_drift = retrieval.clear_drift
            self.use_bbt = retrieval.use_bbt
            self.H2O_scaling = retrieval.H2O_scaling
            self.makeCalibration()
            if self.fit_difference:
                self.clear_drift = retrieval.clear_drift
                self.makeClear()

        if self.type == 'layer':
            # ---------------------------------------------------------------------
            # Extract self emission background
            # ---------------------------------------------------------------------
            if self.subtract_self:
                self.makeSelfEmission()

            # ---------------------------------------------------------------------
            # Extract "no gas" spectrum
            # ---------------------------------------------------------------------
            if self.no_gas:
                self.makeNoGas()

            # ---------------------------------------------------------------------
            # Make Emissivity spectrum
            # ---------------------------------------------------------------------
            if self.use_source_E:
                self.makeSourceEmissivity()
            else:
                logger.info('Source assumed to be black/gray body with uniform emissivity')
                self.E_model = 1

            # ---------------------------------------------------------------------
            # Generate average residual from previous fit
            # ---------------------------------------------------------------------
            if self.use_residual:
                self.makeResidual()
            else:
                self.res_model = 1

        # ---------------------------------------------------------------------
        # Generate reference cross sections
        # ---------------------------------------------------------------------
        self.makeReference(force=force_ref)


    def set(self, name=None, data_dir=None, fit_difference=None, fit_bbt=None):
        """ Update the attributes of an Analyser (which do not affect the spectral grid or reference) """
        if name is not None:
            self.name = name

        if data_dir is not None:
            self.data_dir = data_dir

        if fit_difference is not None:
            self.fit_difference = fit_difference

        if fit_bbt is not None:
            self.fit_bbt = fit_bbt


# ======================================================================================================================
#                                               Generate Reference
# ======================================================================================================================
    def makeReference(self, force=False):
        """
        """

        # Read in Standard quantities for cross-sections
        std = readStandard()
        scalings = np.linspace(0, 4, 21)    # 0-4 scalings for continuum gases


        # Create empty structure to store reference
        ref = {}
        ref['wn'] = self.model_grid

        logger.info('########## Reference cross-sections for %s - type %s ##########'
                    % (self.name.upper(), self.type.upper()))

        # ---------------------------------------------------------------------
        # 1: Homogeneous layer geometry
        # ---------------------------------------------------------------------
        if self.type == 'layer':

            # Generate header for looking up references
            header = "Pathlength: %.3f\n" \
                     "Plume thickness: %.3f\n" \
                     "Atmospheric temperature: %i K\n" \
                     "Atmospheric pressure: %i mbar\n" \
                     "Plume temperature: %i K\n" \
                     "Gas Temp range: %i-%i K\n" \
                     "Atm Temp range: %i-%i K\n" \
                     "Plume pressure: %i mbar\n" \
                     "Fit window: %i - %i cm^-1\n" \
                     "Spectral resolution: %.3f cm^-1\n" \
                     "Atmospheric gases: %s\n" \
                     "Plume gases: %s\n" \
                     "Plume aerosols: %s\n" \
                     % (self.geometry.pathlength,
                        self.geometry.plume_thickness,
                        self.geometry.atm_temp,
                        self.geometry.atm_pres,
                        self.geometry.plume_temp,
                        gas_temps.min(), gas_temps.max(),
                        atm_temps.min(), atm_temps.max(),
                        self.geometry.plume_pres,
                        self.wn_start, self.wn_stop,
                        self.model_spacing,
                        self.params.atmGasList(),
                        self.params.plumeGasList(),
                        self.params.plumeAeroList())

            # Compare headers to see if any of the saved references are a match
            old_refs = glob(self.data_dir + '/layerReference*.pkl')
            match = False
            matches = []
            if len(old_refs) == 0:
                match = False
            else:
                for path in old_refs:
                    with open(path, 'rb') as f:
                        old = pickle.load(f)
                    if 'header' in old.keys():
                        if old['header'] == header:
                            matches.append(True)
                        else:
                            matches.append(False)
                    else:
                        matches.append(False)

            # Load the most recent matching reference
            if any(matches):
                match = True
                match_idx = [i for i, val in enumerate(matches) if val]
                logger.info('Found %i matching references' % len(match_idx))
                if len(match_idx) == 1:
                    logger.info('Loading: %s' % old_refs[match_idx[0]])
                    with open(old_refs[match_idx[0]], 'rb') as f:
                        ref = pickle.load(f)
                else:
                    logger.info('Loading most recent: %s' % old_refs[match_idx[-1]])
                    with open(old_refs[match_idx[-1]], 'rb') as f:
                        ref = pickle.load(f)

            # If no match is found, proceed with creating one
            if not match or force:

                # Create empty structures
                ref['atm'] = {}
                ref['plume'] = {}
                ref['atm']['ref_column'] = {}
                ref['plume']['ref_column'] = {}

                # ----- 1: Atmospheric gases -----
                if self.retrieval.fit_atm_temp:
                    logger.info('Atmospheric gases %s in range %i - %i K...' % (self.params.atmGasList(), atm_temps.min(), atm_temps.max()))
                else:
                    logger.info('Atmospheric gases %s at %i K...' % (self.params.atmGasList(), self.geometry.atm_temp))
                ref['atm']['OD_calib'] = {}

                # Get the list of gases and create a progress bar
                atm_gases = self.params.atmGasList()
                n_gas = len(atm_gases)
                n_continuum = len([gas for gas in atm_gases if gas in continuum_gases])
                if self.retrieval.fit_atm_temp:
                    pbar = tqdm(total=n_continuum * len(scalings) + (n_gas - n_continuum) * len(atm_temps))  # Progress bar
                else:
                    pbar = tqdm(total=n_continuum * len(scalings) + n_gas - n_continuum)

                # Loop over gases
                for i, gas in enumerate(atm_gases):

                    pbar.set_description('%s' % gas)    # Update pbar description

                    # Assign reference column amount and find concentration based on pathlength
                    ref_conc = std[gas]['conc']
                    ref_column = ppm2scd(ref_conc, self.geometry.pathlength,
                                                            temp=self.geometry.atm_temp, pres=self.geometry.atm_pres)
                    ref['atm']['ref_column'][gas] = ref_column

                    # If gas is a continuum gas, use scalings to create a calibration polynomial
                    if gas in continuum_gases:
                        ODs = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                        for j, sc in enumerate(scalings):
                            pbar.set_description('%s: %.1f scaling' % (gas, sc))  # Change pbar message
                            # Generate OD cross-section
                            ODs[:, j] = makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc * sc,
                                                   species=gas)['Bext'] * self.geometry.pathlength * 1e3
                            if sc == 1.0:
                                ref['atm'][gas] = makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc,
                                                             species=gas)['Bext'] * self.geometry.pathlength * 1e3
                            pbar.update()
                        ref['atm']['OD_calib'][gas] = np.polyfit(scalings, ODs.T, 10)

                    # Otherwise create a temperature calibration
                    else:
                        ODs = np.empty([len(self.model_grid), len(atm_temps)], dtype=float)
                        for j, t in enumerate(atm_temps):
                            pbar.set_description('%s @ %i K' % (gas, t))  # Change pbar message
                            conc = scd2ppm(ref_column, self.geometry.pathlength, temp=t,
                                           pres=self.geometry.atm_pres)
                            # Generate OD cross-section
                            ODs[:, j] = makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=conc,
                                                   species=gas, temp=t, pres=self.geometry.plume_pres)[
                                            'Bext'] * self.geometry.pathlength * 1e3
                            pbar.update()

                        ref['atm']['OD_calib'][gas] = np.polyfit(atm_temps, ODs.T, 10)
                        ref['atm'][gas] = makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc,
                                                       species=gas, temp=self.geometry.plume_temp,
                                                       pres=self.geometry.plume_pres)[
                                                'Bext'] * self.geometry.pathlength * 1e3
                    pbar.set_description('Done!')
                pbar.close()
                logger.info('...Done!')

                # ----- 2: Plume gases -----
                if self.retrieval.fit_gas_temp:
                    logger.info('Plume gases %s in range %i - %i K...' % (self.params.plumeGasList(), gas_temps.min(), gas_temps.max()))
                else:
                    logger.info('Plume gases %s at %i K...' % (self.params.plumeGasList(), self.geometry.plume_temp))
                ref['plume']['OD_calib'] = {}

                # Get the list of gases and create a progress bar
                plume_gases = self.params.plumeGasList()
                n_gas = len(plume_gases)
                n_continuum = len([gas for gas in plume_gases if gas in continuum_gases])
                if self.retrieval.fit_gas_temp:
                    pbar = tqdm(total=n_continuum * len(scalings) + (n_gas - n_continuum) * len(gas_temps))  # Progress bar
                else:
                    pbar = tqdm(total=n_continuum * len(scalings) + n_gas - n_continuum)

                # Loop over gases
                for i, gas in enumerate(plume_gases):

                    # If gas is both atm and plume, remove suffix
                    pbar.set_description('%s' % gas)

                    # Assign reference concentration and reference SCD based on plume_thickness
                    ref_conc = std[gas]['conc']
                    ref_column = ppm2scd(ref_conc, self.geometry.plume_thickness,
                                                              temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)
                    ref['plume']['ref_column'][gas] = ref_column

                    # If gas is a continuum gas, use scalings to create a calibration polynomial
                    if gas in continuum_gases:
                        ODs = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                        for j, sc in enumerate(scalings):
                            pbar.set_description('%s: %.1f scaling' % (gas, sc))  # Change pbar message
                            # Generate OD cross-section
                            ODs[:, j] = makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc * sc,
                                                           species=gas, temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)['Bext'] * self.geometry.plume_thickness * 1e3
                            if sc == 1.0:
                                ref['plume'][gas] = makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc,
                                                             species=gas, temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)['Bext'] * self.geometry.plume_thickness * 1e3
                            pbar.update()

                        ref['plume']['OD_calib'][gas] = np.polyfit(scalings, ODs.T, 10)

                    # Otherwise create a temperature calibration
                    else:
                        ODs = np.empty([len(self.model_grid), len(gas_temps)], dtype=float)
                        for j, t in enumerate(gas_temps):
                            pbar.set_description('%s @ %i K' % (gas, t))  # Change pbar message
                            conc = scd2ppm(ref_column, self.geometry.plume_thickness, temp=t, pres=self.geometry.plume_pres)
                            # Generate OD cross-section
                            ODs[:, j] = makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=conc,
                                                   species=gas, temp=t, pres=self.geometry.plume_pres)['Bext'] * self.geometry.plume_thickness * 1e3
                            pbar.update()

                        ref['plume']['OD_calib'][gas] = np.polyfit(gas_temps, ODs.T, 10)
                        ref['plume'][gas] = makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc,
                                                       species=gas, temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)['Bext'] * self.geometry.plume_thickness * 1e3

                    pbar.update()
                    pbar.set_description('Done!')
                pbar.close()
                logger.info('...Done!')

                # --- 3: Plume aerosols ---
                logger.info('Plume particulates %s ...' % self.params.plumeAeroList())

                # Loop over particulates
                for aero in self.params.plumeAeroList():

                    # Assign reference column amount
                    ref['plume']['ref_column'][aero] = std[aero]['conc'] * self.geometry.plume_thickness * 1e3
                    ref['plume'][aero] = {}
                    comp = std[aero]['comp']

                    # Get size parameters from standard file
                    size_log = np.linspace(np.log10(std[aero]['vmin']), np.log10(std[aero]['vmax']), 11)
                    sigma = std[aero]['sigma']
                    mass_conc = std[aero]['conc']

                    # Create empty arrays
                    Bext = np.empty([11, len(self.model_grid)], dtype=float)
                    Babs = np.empty([11, len(self.model_grid)], dtype=float)

                    for i, size in enumerate(size_log):
                        # When using a PSD
                        if sigma is None:
                            xsec = makeAerosolXSC(self.wn_start, self.wn_stop, self.nper_wn, 10 ** size,
                                               mass_conc, species=aero, comp=comp)
                        # When using a single size
                        else:
                            xsec = makeAerosolXSC(self.wn_start, self.wn_stop, self.nper_wn, 10 ** size,
                                               mass_conc, psd=True, sigma=sigma, species=aero, comp=comp)
                        # Store Bext and Babs in arrays
                        Bext[i, :] = xsec['Bext'] * self.geometry.plume_thickness * 1e3
                        Babs[i, :] = xsec['Babs'] * self.geometry.plume_thickness * 1e3

                    # Create polynomial calibration
                    ref['plume'][aero]['Bext'] = np.polyfit(size_log, Bext, 10)
                    ref['plume'][aero]['Babs'] = np.polyfit(size_log, Babs, 10)

                logger.info('...Done!')

                # Save a copy to avoid lengthy reruns
                ref['header'] = header
                outname = self.data_dir + 'layerReference_%s.pkl' % datetime.strftime(datetime.now(), '%Y%m%dT%H%M%S')
                with open(outname, 'wb') as f:
                    pickle.dump(ref, f)
                logger.info('Reference saved in local data directory: %s' % outname)

        # ---------------------------------------------------------------------
        # 2: Solar occultation geometry
        # ---------------------------------------------------------------------
        elif self.type == 'solar' or self.type == 'lunar':

            # Look for solar spectrum (coming soon!)

            # Create empty structures
            ref['atm'] = {}
            ref['plume'] = {}
            ref['atm']['ref_column'] = {}
            ref['plume']['ref_column'] = {}

            # Copy atm profile to analyser level
            self.atm = self.geometry.atm

            # Generate header for looking up references
            header = "Standard atmosphere: %s\n" \
                     "Sounding: %s\n" \
                     "Observer Height: %.3f km\n" \
                     "Plume Height: %.3f km\n" \
                     "Plume Thickness: %.3f km\n" \
                     "Viewing Angle: %.1f deg\n" \
                     "Fit window: %i - %i cm^-1\n" \
                     "Spectral resolution: %.3f cm^-1\n" \
                     "Atmospheric gases: %s\n" \
                     "Plume gases: %s\n" \
                     "Plume aerosols: %s\n" \
                     % (self.geometry.atm_path,
                        self.geometry.sounding,
                        self.geometry.obs_height,
                        self.geometry.plume_height,
                        self.geometry.plume_thickness,
                        self.geometry.elev,
                        self.wn_start, self.wn_stop,
                        self.model_spacing,
                        self.params.atmGasList(),
                        self.params.plumeGasList(),
                        self.params.plumeAeroList())

            # Compare headers to see if any of the saved references are a match
            old_refs = glob(self.data_dir + '/solarReference*.pkl')
            match = False
            matches = []
            if len(old_refs) == 0:
                match = False
            else:
                for path in old_refs:
                    with open(path, 'rb') as f:
                        old = pickle.load(f)
                    if 'header' in old.keys():
                        if old['header'] == header:
                            matches.append(True)
                        else:
                            matches.append(False)
                    else:
                        matches.append(False)

            # Load most recent matching reference
            if any(matches):
                match = True
                match_idx = [i for i, val in enumerate(matches) if val]
                logger.info('Found %i matching references' % len(match_idx))
                if len(match_idx) == 1:
                    logger.info('Loading: %s' % old_refs[match_idx[0]])
                    with open(old_refs[match_idx[0]], 'rb') as f:
                        ref = pickle.load(f)
                else:
                    logger.info('Loading most recent: %s' % old_refs[match_idx[-1]])
                    with open(old_refs[match_idx[-1]], 'rb') as f:
                        ref = pickle.load(f)

            # If no match is found, proceed with creating a new one
            if not match or force:

                # --- 1: Atmospheric gases ---
                logger.info('Background atmosphere: from observer height (%.2f km) to edge of atmosphere ...'
                            % self.geometry.obs_height)
                atm_dist, dist = sliceAtm(self.geometry.atm_path, self.geometry.obs_height,
                                          self.atm['HGT'].max(), outpath=True)

                ref['atm']['OD_calib'] = {}

                # Look up list of gases and create a progress bar
                atm_gases = self.params.atmGasList()
                n_gas = len(atm_gases)
                n_continuum = len([gas for gas in atm_gases if gas in continuum_gases])
                pbar = tqdm(total=n_continuum * len(scalings) + n_gas - n_continuum)  # Progress bar

                # Loop over gases
                for gas in atm_gases:

                    # Assign total column as reference
                    pbar.set_description('%s' % gas)
                    ref['atm']['ref_column'][gas] = ppm2scd(atm_dist[gas], atm_dist['HGT'] * 1e3,
                                                            temp=atm_dist['TEM'],
                                                            pres=atm_dist['PRE'])

                    # If gas is a continuum gas
                    if gas in continuum_gases:

                        ODs = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                        for j, sc in enumerate(scalings):
                            pbar.set_description('%s: %.1f scaling' % (gas, sc))  # Change pbar message

                            # Scale atmosphere for that gas
                            atm_scaled, scaled = scaleAtm(dist, sc, which=gas, outpath=True)

                            # Write RFM driver and run
                            writeRFMdrv(self.wn_start, self.wn_stop, self.nper_wn, layer=False, atm=scaled,
                                        height=self.geometry.obs_height, elev=self.geometry.elev, gas=gas)
                            run('./RFM/source/rfm', stdout=DEVNULL)

                            # Generate OD cross-section
                            ODs[:, j] = readRFM('./RFM/output/opt%05d.out' % (self.geometry.elev * 1e3))['data']
                            if sc == 1.0:
                                ref['atm'][gas] = readRFM('./RFM/output/opt%05d.out' % (self.geometry.elev * 1e3))['data']
                            pbar.update()

                        ref['atm']['OD_calib'][gas] = np.polyfit(scalings, ODs.T, 10)

                    # Otherwise just create a cross-section based onn a reference quantity
                    else:

                        # Write RFM driver and run
                        writeRFMdrv(self.wn_start, self.wn_stop, self.nper_wn, layer=False, atm=dist,
                                    height=self.geometry.obs_height, elev=self.geometry.elev, gas=gas)
                        run('./RFM/source/rfm', stdout=DEVNULL)

                        # Read Optical Depth
                        ref['atm'][gas] = readRFM('./RFM/output/opt%05d.out' % (self.geometry.elev * 1e3))['data']
                        pbar.update()
                    pbar.set_description('Done!')
                pbar.close()
                logger.info('...Done!')

                # ----- 2: Plume gases -----
                logger.info('Plume gases %s at %i K...' % (self.params.plumeGasList(), self.geometry.plume_temp))
                ref['plume']['OD_calib'] = {}

                # Create list of gases and generate progress bar
                plume_gases = self.params.plumeGasList()
                n_gas = len(plume_gases)
                n_continuum = len([gas for gas in plume_gases if gas in continuum_gases])
                pbar = tqdm(total=n_continuum * len(scalings) + n_gas - n_continuum)  # Progress bar

                # Loop over gases
                for i, gas in enumerate(plume_gases):

                    # If gas is both atm and plume, remove suffix
                    pbar.set_description('%s' % gas)

                    # Assign reference concentration and reference SCD based on pathlength
                    ref_conc = std[gas]['conc']
                    ref['plume']['ref_column'][gas] = ppm2scd(ref_conc, self.geometry.pathlength,
                                                              temp=self.geometry.plume_temp,
                                                              pres=self.geometry.plume_pres)

                    # If gas is a continuum gas, use scalings to create a calibration polynomial
                    if gas in continuum_gases:

                        ODs = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                        for j, sc in enumerate(scalings):
                            pbar.set_description('%s: %.1f scaling' % (gas, sc))  # Change pbar message
                            # Generate OD cross-section
                            ODs[:, j] = \
                                makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc * sc,
                                           species=gas, temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)['Bext'] * self.geometry.pathlength * 1e3
                            if sc == 1.0:
                                ref['atm'][gas] = \
                                    makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc,
                                               species=gas, temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)['Bext'] * self.geometry.pathlength * 1e3
                            pbar.update()

                        ref['plume']['OD_calib'][gas] = np.polyfit(scalings, ODs.T, 10)

                    # Otherwise create a cross-section for a reference quantity
                    else:
                        pbar.set_description('%s' % gas)
                        ref['plume'][gas] = \
                            makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc,
                                       species=gas, temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)['Bext'] * self.geometry.pathlength * 1e3
                        pbar.update()
                    pbar.set_description('Done!')
                pbar.close()
                logger.info('...Done!')

                # --- 3: Plume aerosols ---
                logger.info('Plume aerosols %s ...' % self.params.plumeAeroList())
                for aero in self.params.plumeAeroList():

                    # Assign reference column amount
                    ref['plume'][aero] = {}
                    comp = std[aero]['comp']
                    ref['plume']['ref_column'][aero] = std[aero]['conc'] * self.geometry.pathlength * 1e3

                    # Get size parameters from standard file
                    size_log = np.linspace(np.log10(std[aero]['vmin']), np.log10(std[aero]['vmax']), 11)
                    sigma = std[aero]['sigma']
                    mass_conc = std[aero]['conc']

                    # Create empty arrays
                    Bext = np.empty([11, len(self.model_grid)], dtype=float)
                    Babs = np.empty([11, len(self.model_grid)], dtype=float)

                    for i, size in enumerate(size_log):
                        # When size is a PSD
                        if sigma is None:
                            xsec = makeAerosolXSC(self.wn_start, self.wn_stop, self.nper_wn, 10 ** size,
                                               mass_conc, species=aero, comp=comp)

                        # When size is a single size
                        else:
                            xsec = makeAerosolXSC(self.wn_start, self.wn_stop, self.nper_wn, 10 ** size,
                                               mass_conc, psd=True, sigma=sigma, species=aero, comp=comp)

                        # Store Bext and Babs in arrays
                        Bext[i, :] = xsec['Bext'] * self.geometry.pathlength * 1e3
                        Babs[i, :] = xsec['Babs'] * self.geometry.pathlength * 1e3

                    # Make calibration polynomials
                    ref['plume'][aero]['Bext'] = np.polyfit(size_log, Bext, 10)
                    ref['plume'][aero]['Babs'] = np.polyfit(size_log, Babs, 10)

                logger.info('...Done!')

                # Save a copy to avoid lengthy reruns
                ref['header'] = header
                outname = self.data_dir + 'solarReference_%s.pkl' % datetime.strftime(datetime.now(), '%Y%m%dT%H%M%S')
                with open(outname, 'wb') as f:
                    pickle.dump(ref, f)

        # ---------------------------------------------------------------------
        # 3: Emission geometry
        # ---------------------------------------------------------------------
        elif self.type == 'emission':

            # Copy atm profile to Analyser level
            self.atm = self.geometry.atm

            # Generate header for looking up references
            header = "Layer Model: %i\n" \
                     "Standard atmosphere: %s\n" \
                     "Sounding: %s\n" \
                     "Observer Height: %.3f km\n" \
                     "Plume Height: %.3f km\n" \
                     "Plume Thickness: %.3f km\n" \
                     "Viewing Angle: %.1f deg\n" \
                     "Fit window: %i - %i cm^-1\n" \
                     "Spectral resolution: %.3f cm^-1\n" \
                     "Atmospheric gases: %s\n" \
                     "Plume gases: %s\n" \
                     "Plume aerosols: %s\n" \
                     % (self.geometry.layer_model,
                        self.geometry.atm_path,
                        self.geometry.sounding,
                        self.geometry.obs_height,
                        self.geometry.plume_height,
                        self.geometry.plume_thickness,
                        self.geometry.elev,
                        self.wn_start, self.wn_stop,
                        self.model_spacing,
                        self.params.atmGasList(),
                        self.params.plumeGasList(),
                        self.params.plumeAeroList())

            # Compare headers to see if any of the saved references are a match
            old_refs = glob(self.data_dir + '/emissionReference*.pkl')
            old_refs.sort()
            match = False
            matches = []
            if len(old_refs) == 0:
                match = False
            else:
                for path in old_refs:
                    with open(path, 'rb') as f:
                        old = pickle.load(f)
                    if 'header' in old.keys():
                        if old['header'] == header:
                            matches.append(True)
                        else:
                            matches.append(False)
                    else:
                        matches.append(False)

            # Load most recent matching reference
            if any(matches):
                match = True
                match_idx = [i for i, val in enumerate(matches) if val]
                logger.info('Found %i matching references' % len(match_idx))
                if len(match_idx) == 1:
                    logger.info('Loading: %s' % old_refs[match_idx[0]])
                    with open(old_refs[match_idx[0]], 'rb') as f:
                        ref = pickle.load(f)
                else:
                    logger.info('Loading most recent: %s' % old_refs[match_idx[-1]])
                    with open(old_refs[match_idx[-1]], 'rb') as f:
                        ref = pickle.load(f)

            atm_gases = " ".join(self.params.atmGasList())

            if not match or force:

                # Create empty structures
                ref['atm'] = {}
                ref['plume'] = {}
                ref['atm']['ref_column'] = {}
                ref['plume']['ref_column'] = {}
                ref['atm_prox'] = {}
                ref['atm_prox']['ref_column'] = {}
                if self.geometry.layer_model == 2:
                    ref['atm_slice'] = {}
                    ref['atm_slice']['ref_column'] = {}
                ref['atm_dist'] = {}
                ref['atm_dist']['ref_column'] = {}

                # --- 1a: Atmospheric gases in proximal atmosphere ---
                logger.info('Background atmosphere: from observer (%.2f km) to plume height (%.2f km) ...'
                            % (self.geometry.obs_height, self.geometry.h1))
                atm_prox, prox = sliceAtm(self.geometry.atm_path, self.geometry.obs_height, self.geometry.h1, outpath=True)

                # Assign total column as reference
                for gas in atm_gases.split():
                    ref['atm_prox']['ref_column'][gas] = ppm2scd(atm_prox[gas], atm_prox['HGT'],
                                                                 temp=atm_prox['TEM'], pres=atm_prox['PRE'],
                                                                 elev=self.geometry.elev)
                pbar = tqdm(total=len(scalings))
                L = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                T = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                gas = 'H2O'
                for j, sc in enumerate(scalings):
                    pbar.set_description('%s: %.1f scaling' % (gas, sc))  # Change pbar message
                    # Write RFM driver and run
                    atm_scaled, scaled = scaleAtm(prox, sc, which=gas, outpath=True)
                    writeRFMdrv(self.wn_start, self.wn_stop, self.nper_wn, layer=False, atm=scaled,
                                height=self.geometry.obs_height, elev=self.geometry.elev, gas=atm_gases)
                    run('./RFM/source/rfm', stdout=DEVNULL)
                    # Read Optical Depth and Radiance
                    L[:, j] = readRFM('./RFM/output/rad%05d.out' % (self.geometry.elev * 1e3))['data'] / 100
                    T[:, j] = readRFM('./RFM/output/tra%05d.out' % (self.geometry.elev * 1e3))['data']

                    if sc == 1.0:
                        ref['L_prox'] = L[:, j]
                        ref['T_prox'] = T[:, j]
                    pbar.update()
                pbar.set_description('Done!')
                pbar.close()

                # Make polynomial calibration
                ref['atm_prox']['L_calib'] = np.polyfit(scalings, L.T, 10)
                ref['atm_prox']['T_calib'] = np.polyfit(scalings, T.T, 10)

                if self.geometry.layer_model == 2:

                    # --- 1b: Atmospheric gases in plume slice ---
                    logger.info('Background atmosphere: within plume slice (%.2f - %.2f km) ...' % (self.geometry.h1, self.geometry.h2))
                    atm_slice, slice = sliceAtm(self.geometry.atm_path, self.geometry.h1, self.geometry.h2, outpath=True)

                    # Assign total column as reference
                    for gas in atm_gases.split():
                        ref['atm_slice']['ref_column'][gas] = ppm2scd(atm_slice[gas], atm_slice['HGT'], temp=atm_slice['TEM'],
                                                                     pres=atm_slice['PRE'], elev=self.geometry.elev)
                    pbar = tqdm(total=len(scalings))
                    L = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                    T = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                    gas = 'H2O'
                    for j, sc in enumerate(scalings):
                        pbar.set_description('%s: %.1f scaling' % (gas, sc))  # Change pbar message
                        # Write RFM driver and run
                        atm_scaled, scaled = scaleAtm(slice, sc, which=gas, outpath=True)
                        writeRFMdrv(self.wn_start, self.wn_stop, self.nper_wn, layer=False, atm=scaled,
                                    height=self.geometry.h1, elev=self.geometry.elev, gas=atm_gases)
                        run('./RFM/source/rfm', stdout=DEVNULL)
                        # Read Optical Depth and Radiance
                        L[:, j] = readRFM('./RFM/output/rad%05d.out' % (self.geometry.elev * 1e3))['data'] / 100
                        T[:, j] = readRFM('./RFM/output/tra%05d.out' % (self.geometry.elev * 1e3))['data']

                        if sc == 1.0:
                            ref['L_slice'] = L[:, j]
                            ref['T_slice'] = T[:, j]
                        pbar.update()
                    pbar.set_description('Done!')
                    pbar.close()

                    # Make polynomial calibration
                    ref['atm_slice']['L_calib'] = np.polyfit(scalings, L.T, 10)
                    ref['atm_slice']['T_calib'] = np.polyfit(scalings, T.T, 10)

                # --- 1c: Atmospheric gases in distal atmosphere ---
                logger.info('Background atmosphere: from plume height (%.2f km) to edge of atmosphere ...' % self.geometry.h2)
                atm_dist, dist = sliceAtm(self.geometry.atm_path, self.geometry.h2, self.atm['HGT'].max(), outpath=True)

                # Assign total column as reference
                for gas in atm_gases.split():
                    ref['atm_dist']['ref_column'][gas] = ppm2scd(atm_dist[gas], atm_dist['HGT'], temp=atm_dist['TEM'],
                                                                 pres=atm_dist['PRE'], elev=self.geometry.elev)
                pbar = tqdm(total=len(scalings))
                L = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                T = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                gas = 'H2O'
                for j, sc in enumerate(scalings):
                    pbar.set_description('%s: %.1f scaling' % (gas, sc))  # Change pbar message
                    # Write RFM driver and run
                    atm_scaled, scaled = scaleAtm(dist, sc, which=gas, outpath=True)
                    writeRFMdrv(self.wn_start, self.wn_stop, self.nper_wn, layer=False, atm=scaled,
                                height=self.geometry.h2, elev=self.geometry.elev, gas=atm_gases)
                    run('./RFM/source/rfm', stdout=DEVNULL)
                    # Read Optical Depth and Radiance
                    L[:, j] = readRFM('./RFM/output/rad%05d.out' % (self.geometry.elev * 1e3))['data'] / 100
                    T[:, j] = readRFM('./RFM/output/tra%05d.out' % (self.geometry.elev * 1e3))['data']

                    if sc == 1.0:
                        ref['L_dist'] = L[:, j]
                        ref['T_dist'] = T[:, j]
                    pbar.update()
                pbar.set_description('Done!')
                pbar.close()

                # Make polynomial calibration
                ref['atm_dist']['L_calib'] = np.polyfit(scalings, L.T, 10)
                ref['atm_dist']['T_calib'] = np.polyfit(scalings, T.T, 10)

                # --- 2a: Plume layer: Atm gases ---
                logger.info('Atmospheric gases in plume: %s at %i K...' % (self.params.atmGasList(), self.geometry.atm_temp))
                ref['atm']['OD_calib'] = {}
                if 'H2O' in self.params.atmGasList():
                    nH2O = 1
                else:
                    nH2O = 0
                pbar = tqdm(total=len(scalings) * nH2O + len(self.params.atmGasList()) - nH2O)  # Progress bar
                for i, gas in enumerate(self.params.atmGasList()):

                    # Assign reference column amount and find concentration based on pathlength
                    ref_conc = np.interp(self.geometry.plume_height, atm_slice['HGT'], atm_slice[gas])
                    ref['atm']['ref_column'][gas] = ppm2scd(ref_conc, self.geometry.pathlength,
                                                            temp=self.geometry.atm_temp, pres=self.geometry.atm_pres,
                                                            elev=self.geometry.elev)

                    if gas in continuum_gases:
                        ODs = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                        for j, sc in enumerate(scalings):
                            pbar.set_description('%s: %.1f scaling' % (gas, sc))  # Change pbar message
                            # Generate OD cross-section
                            ODs[:, j] = makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc * sc,
                                        species=gas, temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)['Bext'] * self.geometry.pathlength * 1e3 \

                            if sc == 1.0:
                                ref['atm'][gas] = ODs[:, j]

                            pbar.update()
                        ref['atm']['OD_calib'][gas] = np.polyfit(scalings, ODs.T, 10)

                    else:
                        pbar.set_description('%s' % gas)
                        ref['atm'][gas] = makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc,
                                                     species=gas, temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)['Bext'] * self.geometry.pathlength * 1e3

                        pbar.update()
                pbar.set_description('Done!')
                pbar.close()
                logger.info('...Done!')

                # ----- 2b: Plume gases -----
                logger.info('Plume gases %s at %i K...' % (self.params.plumeGasList(), self.geometry.plume_temp))
                ref['plume']['OD_calib'] = {}
                if 'H2O' in self.params.plumeGasList():
                    nH2O = 1
                else:
                    nH2O = 0
                pbar = tqdm(total=len(scalings) * nH2O + len(self.params.plumeGasList()) - nH2O)  # Progress bar
                for i, gas in enumerate(self.params.plumeGasList()):

                    # Assign reference concentration and reference SCD based on pathlength
                    ref_conc = std[gas]['conc']
                    ref['plume']['ref_column'][gas] = ppm2scd(ref_conc, self.geometry.pathlength,
                                                              temp=self.geometry.plume_temp, pres=self.geometry.plume_pres,
                                                              elev=self.geometry.elev)

                    if gas in continuum_gases:

                        ODs = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                        for j, sc in enumerate(scalings):
                            pbar.set_description('%s: %.1f scaling' % (gas, sc))  # Change pbar message
                            # Generate OD cross-section
                            ODs[:, j] = \
                            makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc * sc,
                                       species=gas, temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)['Bext'] * self.geometry.pathlength * 1e3

                            if sc == 1.0:
                                ref['atm'][gas] = ODs[:, j]
                            pbar.update()

                        ref['plume']['OD_calib'][gas] = np.polyfit(scalings, ODs.T, 10)

                    else:
                        pbar.set_description('%s' % gas)
                        ref['plume'][gas] = \
                        makeGasXSC(self.wn_start, self.wn_stop, self.nper_wn, conc=ref_conc,
                                   species=gas, temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)['Bext'] * self.geometry.pathlength * 1e3

                        pbar.update()
                    pbar.set_description('Done!')
                pbar.close()
                logger.info('...Done!')

                # --- 2c: Plume aerosols ---
                logger.info('Plume aerosols...')
                for aero in self.params.plumeAeroList():

                    # Assign reference column amount
                    ref['plume'][aero] = {}
                    ref['plume']['ref_column'][aero] = std[aero]['conc'] * self.geometry.pathlength * 1e3

                    # Get size parameters from standard file
                    size_log = np.linspace(np.log10(std[aero]['vmin']), np.log10(std[aero]['vmax']), 11)
                    Bext = np.empty([11, len(self.model_grid)], dtype=float)
                    Babs = np.empty([11, len(self.model_grid)], dtype=float)
                    sigma = std[aero]['sigma']
                    mass_conc = std[aero]['conc']
                    comp = std[aero]['comp']

                    for i, size in enumerate(size_log):
                        # When size is a PSD
                        if sigma is None:
                            xsec = makeAerosolXSC(self.wn_start, self.wn_stop, self.nper_wn, 10 ** size,
                                                  mass_conc, species=aero, comp=comp)

                        # When size is a single size
                        else:
                            xsec = makeAerosolXSC(self.wn_start, self.wn_stop, self.nper_wn, 10 ** size,
                                                  mass_conc, psd=True, sigma=sigma, species=aero, comp=comp)

                        # Store Bext and Babs in arrays
                        Bext[i, :] = xsec['Bext'] * self.geometry.pathlength * 1e3
                        Babs[i, :] = xsec['Babs'] * self.geometry.pathlength * 1e3

                    # Make calibration polynomials
                    ref['plume'][aero]['Bext'] = np.polyfit(size_log, Bext, 10)
                    ref['plume'][aero]['Babs'] = np.polyfit(size_log, Babs, 10)

                logger.info('...Done!')

                # Save a copy to avoid lengthy reruns
                ref['header'] = header
                outname = self.data_dir + 'emissionReference_%s.pkl' % datetime.strftime(datetime.now(), '%Y%m%dT%H%M%S')
                with open(outname, 'wb') as f:
                    pickle.dump(ref, f)

        # Assign Reference dictionary to Analyser
        self.reference = ref

        # Compute theoretical clear sky on model grid
        if self.type == 'emission' and self.fit_difference:
            L_prox = self.reference['L_prox']
            T_prox = self.reference['T_prox']
            L_prox = planck(self.model_grid, self.geometry.obs_temp) * (1 - T_prox)
            L_dist = self.reference['L_dist']
            if self.geometry.layer_model == 1:
                self.L_clear_model = L_dist * T_prox + L_prox
            elif self.geometry.layer_model == 2:
                L_slice = self.reference['L_slice']
                T_slice = self.reference['T_slice']
                self.L_clear_model = L_dist * T_slice * T_prox + L_slice * T_prox + L_prox

        # Update gas parameters with reference column amount
        for name, p in self.params.items():
            if name in self.params.atmGasList():
                p.set(ref_column=ref['atm']['ref_column'][name])
            if name in self.params.plumeGasList():
                p.set(ref_column=ref['plume']['ref_column'][name])
            if name in self.params.plumeAeroList():
                p.set(ref_column=ref['plume']['ref_column'][name])

        # Print the list of gases in reference
        # self.prettyPrint()


# ======================================================================================================================
#                                        Plot Reference
# ======================================================================================================================
    def plotReference(self, save=False, outdir='./plots/', title=True, umax=True):

        # Assign local variables
        std = readStandard()
        ref = self.reference
        wn = self.model_grid
        padding = self.model_padding
        n_atm = len(self.params.atmGasList())
        n_plume = len(self.params.plumeGasList()) + len(self.params.plumeAeroList())
        depth = 0.75
        if wn.max() - wn.min() > 5 * padding:
            pad = 2
        else:
            pad = 1
        if self.type == 'emission':
            atmT = 'Transmittance'
            plumeT = 'Extinction'
        else:
            atmT = 'Transmittance'
            plumeT = 'Transmittance'

        idx = [i for i, w in enumerate(wn) if wn.min() + pad * padding <= w <= wn.max() - pad * padding]

        # Open figure
        fig, ax1 = plt.subplots(figsize=[10, 6])
        ax2 = plt.twinx(ax1)
        if len(self.params.plumeAeroList()) > 0:
            ax3 = plt.twinx(ax1)
            thirdcolor = 'tab:grey'
            tkw = dict(size=4, width=1.5)
            ax3.tick_params(axis='y', colors=thirdcolor, **tkw)
            ax3.spines['right'].set_position(("axes", 1.1))
            ax3.spines['right'].set_fc(thirdcolor)
            ax3.spines['right'].set_ec(thirdcolor)
            ax3.set_ylabel('Test', fontdict={'color': thirdcolor})

        # Add title
        if title:
            fig.suptitle('%s - %s' % (self.name, self.type.upper()))

        for gas in self.params.atmGasList():
            if gas == 'H2O':
                sc = 1.0
            else:
                sc = - np.log(1 - depth) / ref['atm'][gas][idx].max()
            T = np.exp(-ref['atm'][gas] * sc)
            ax1.plot(wn, T, c=plot_colors[gas], zorder=0, alpha=0.5, label='%s' % pretty_names[gas])

        lns = []
        for gas in self.params.plumeGasList():
            if gas == 'H2O':
                sc = 1.0
            else:
                sc = - np.log(1 - depth) / ref['plume'][gas][idx].max()
            if self.type == 'emission':
                A = 1 - np.exp(-ref['plume'][gas] * sc)
                l, = ax2.plot(wn, A, c=plot_colors[gas], zorder=1, alpha=1.0, label='%s' % pretty_names[gas])
                lns.append(l)
            else:
                T = np.exp(-ref['plume'][gas] * sc)
                l, = ax2.plot(wn, T, c=plot_colors[gas], zorder=1, alpha=1.0, label='%s' % pretty_names[gas])
                lns.append(l)

        for aero in self.params.plumeAeroList():
            sc = - np.log(1 - depth) / np.polyval(ref['plume'][aero]['Bext'], 1.0).max()
            if self.type == 'emission':
                A = 1 - np.exp(-np.polyval(ref['plume'][aero]['Bext'], 1.0) * sc)
                l, = ax3.plot(wn, A, c=plot_colors[aero], zorder=1, alpha=1.0, label='%s' % pretty_names[aero])
                lns.append(l)
            else:
                T = np.exp(-np.polyval(ref['plume'][aero]['Bext'], 1.0) * sc)
                l, = ax2.plot(wn, T, c=plot_colors[aero], zorder=1, alpha=1.0, label='%s' % pretty_names[aero])
                lns.append(l)

        # Add micron wavelength axis
        if umax:
            bbox1 = (0., 1.08, 0.5, .102)
            bbox2 = (0.5, 1.08, 0.5, .102)
            def fw(x):
                return 10000 / x
            umax = ax1.secondary_xaxis('top', functions=(fw, fw))
            umax.minorticks_on()
            umax.set_xlabel('Wavelength [$\mu$m]')
        else:
            bbox1 = (0., 1.02, 0.5, .102)
            bbox2 = (0.5, 1.02, 0.5, .102)

        ax1.set(xlabel='Wavenumber [$cm^{-1}$]', ylabel='%s' % atmT, ylim=(0, 1))
        if len(self.params.plumeAeroList()) > 0:
            ax2.set(ylabel='Gas %s' % plumeT, ylim=(0, 1))
            ax3.set(ylabel='Particulate %s' % plumeT)
        else:
            ax2.set(ylabel='%s' % plumeT, ylim=(0, 1))

        ax1.legend(title='Atmosphere (%s - %.2g km)' % (atmT, self.geometry.pathlength), bbox_to_anchor=bbox1,
                  loc='lower left', ncol=n_atm // 2 + n_atm % 2)

        labs = [l.get_label() for l in lns]
        ax2.legend(lns, labs, title='Plume (%s - %.2g km)' % (plumeT, self.geometry.pathlength), bbox_to_anchor=bbox2,
                   loc='lower right', ncol=n_plume // 2 + n_plume % 2)

        plt.tight_layout()
        plt.show(block=False)

        if save:
            plt.savefig(outdir + 'reference.png', dpi=600)
            plt.close()

# ======================================================================================================================
#                                        Generate calibration
# ======================================================================================================================
    def makeCalibration(self):

        logger.info('Generating calibration from BB files')

        # List all files in directory
        flist = glob(self.data_dir + '*')

        # Only keep the potential spectra files
        flist = [f for f in flist if not os.path.isdir(f)]
        invalid_extensions = ['pkl', 'csv', 'txt', 'ini', 'dat', 'xls', 'xlsx', 'doc', 'docx', 'jpg', 'png']
        flist = [f for f in flist if f.split('.')[-1] not in invalid_extensions]

        # Look for clear and blackbody spectra and remove from main list
        bbc_list = [f for f in flist if any([s in f for s in ['bbc', 'BBC']])]
        bbh_list = [f for f in flist if any([s in f for s in ['bbh', 'BBH']])]

        # ---------------------------------------------------------------------
        # Make BB IRF and RI
        # ---------------------------------------------------------------------
        times = []
        bbc = []
        bbh = []
        for i, item in enumerate(bbc_list):
            if i == 0:
                wn = read_spectrum(bbc_list[i]).spectrum[0]
                idx = [i for i, x in enumerate(wn) if self.fit_window[0] <= x <= self.fit_window[1]]
                wn = wn[idx]
                TC = read_spectrum(bbc_list[i]).target_temp
                TH = read_spectrum(bbh_list[i]).target_temp
            bbc.append(read_spectrum(bbc_list[i]).spectrum[1][idx])
            bbh.append(read_spectrum(bbh_list[i]).spectrum[1][idx])
            times.append(read_spectrum(bbc_list[i]).dtime.timestamp())

        times = np.asarray(times)
        bbc = np.asarray(bbc)
        bbh = np.asarray(bbh)

        n = len(times)

        if n > 1 and self.bb_drift:

            logger.info('Found %i sets of blackbodies. Accounting for drift over time' % n)

            idx2 = times.argsort()
            times = times[idx2]
            bbc = bbc[idx2]
            bbh = bbh[idx2]

            # times = np.linspace(times[0], times[-1], n_steps+1)
            deg = min([n-1, 2])
            irf = np.empty([n, len(wn)], dtype=float)
            ri = np.empty([n, len(wn)], dtype=float)
            for i in range(n):
                # Read in sbm spectra
                BBC_sb = bbc[i]
                BBH_sb = bbh[i]

                # Create theoretical BB radiance functions
                planck_C = planck(wn, TC)
                planck_H = planck(wn, TH)

                # Make sure we don't divide by zero anywhere
                quotient = BBH_sb - BBC_sb
                minimum = np.min(np.abs(quotient[np.nonzero(quotient)]))  # Find smallest non-zero number
                quotient = np.where(quotient == 0, minimum, quotient)  # Replace zeros by that number

                # Derive Instrument Response
                ri[i, :] = (BBH_sb * planck_C - BBC_sb * planck_H) / quotient
                irf[i, :] = BBH_sb / (planck_H - ri[i, :])

            self.irf_poly = np.polyfit(times, irf, deg)
            self.ri_poly = np.polyfit(times, ri, deg)

        else:
            # Read in sbm spectra
            BBC_sb = bbc[-1]
            BBH_sb = bbh[-1]

            # Create theoretical BB radiance functions
            planck_C = planck(wn, TC)
            planck_H = planck(wn, TH)

            # Make sure we don't divide by zero anywhere
            quotient = BBH_sb - BBC_sb
            minimum = np.min(np.abs(quotient[np.nonzero(quotient)]))  # Find smallest non-zero number
            quotient = np.where(quotient == 0, minimum, quotient)  # Replace zeros by that number

            # Derive Instrument Response
            self.ri = (BBH_sb * planck_C - BBC_sb * planck_H) / quotient
            self.irf = BBH_sb / (planck_H - self.ri)

        # Append lists of BB names and spectra
        self.bbc_list = bbc_list
        self.bbh_list = bbh_list
        self.bbc = bbc
        self.bbh = bbh

# ======================================================================================================================
#                                        Generate clear sky
# ======================================================================================================================
    def makeClear(self):

        logger.info('Generating clear sky')

        # List all files in directory
        flist = glob(self.data_dir + '*')

        # Only keep the potential spectra files
        flist = [f for f in flist if not os.path.isdir(f)]
        invalid_extensions = ['pkl', 'csv', 'txt', 'ini', 'dat', 'xls', 'xlsx', 'doc', 'docx', 'jpg', 'png']
        flist = [f for f in flist if f.split('.')[-1] not in invalid_extensions]

        # Look for clear and blackbody spectra and remove from main list
        clear_list = [f for f in flist if any([s in f for s in ['clear', 'sky']])]

        # ---------------------------------------------------------------------
        # Making clear spectra
        # ---------------------------------------------------------------------
        wn = read_spectrum(clear_list[0]).spectrum[0]
        idx = [i for i, x in enumerate(wn) if self.fit_window[0] <= x <= self.fit_window[1]]
        wn = wn[idx]

        n = len(clear_list)

        times = np.empty(n, dtype=float)

        if n > 1 and self.clear_drift:

            logger.info('Found %i clear sky spectra. Accounting for drift over time' % n)

            deg = min([n-1, 2])
            clear = np.empty([n, len(wn)], dtype=float)
            for i in range(n):
                f = read_spectrum(clear_list[i])
                times[i] = datetime.timestamp(f.dtime)
                try:
                    irf = np.polyval(self.irf_poly, times[i])
                    ri = np.polyval(self.ri_poly, times[i])
                except:
                    irf = self.irf
                    ri = self.ri
                clear[i, :] = f.spectrum[1][idx] / irf + ri

            self.clear_poly = np.polyfit(times, clear, deg)
            self.clear = clear

        else:
            f = read_spectrum(clear_list[-1])
            timestamp = datetime.timestamp(f.dtime)
            try:
                irf = np.polyval(self.irf_poly, timestamp)
                ri = np.polyval(self.irf_poly, timestamp)
            except:
                irf = self.irf
                ri = self.ri

            self.clear = f.spectrum[1][idx] / irf + ri

        # Append lists sky name
        self.clear_list = clear_list

# ======================================================================================================================
#                                       Generate instrument radiance spectrum
# ======================================================================================================================
    def makeSelfEmission(self):

        logger.info('Generating self emission background')

        # List all files in directory
        flist = glob(self.data_dir + '*')

        # Only keep the potential spectra files
        flist = [f for f in flist if not os.path.isdir(f)]
        invalid_extensions = ['pkl', 'csv', 'txt', 'ini', 'dat', 'xls', 'xlsx', 'doc', 'docx', 'jpg', 'png']
        flist = [f for f in flist if f.split('.')[-1] not in invalid_extensions]

        # Look for self emission spectra
        self_list = [f for f in flist if any([s in f for s in ['self']])]

        # ---------------------------------------------------------------------
        # Making self emission spectra
        # ---------------------------------------------------------------------
        wn = read_spectrum(self_list[0]).spectrum[0]
        idx = [i for i, x in enumerate(wn) if self.fit_window[0] <= x <= self.fit_window[1]]
        wn = wn[idx]
        self.self_emission_x = wn

        n = len(self_list)

        times = np.empty(n, dtype=float)

        if n > 1:

            logger.info('Found %i self emission spectra. Accounting for drift over time' % n)

            deg = min([n - 1, 2])
            self_spectra = np.empty([n, len(wn)], dtype=float)
            for i in range(n):
                f = read_spectrum(self_list[i])
                times[i] = datetime.timestamp(f.dtime)
                self_spectra[i, :] = f.spectrum[1][idx]
            self.self_poly = np.polyfit(times, self_spectra, deg)

        else:
            f = read_spectrum(self_list[-1])
            self.self_emission = f.spectrum[1][idx]

        # Append lists sky name
        self.self_list = self_list

# ======================================================================================================================
#                                 Generate a spectrum with no gas to isolate Plume T
# ======================================================================================================================
    def makeNoGas(self):

        logger.info('Isolating Plume Transmittance')

        # List all files in directory
        flist = glob(self.data_dir + '*')

        # Only keep the potential spectra files
        flist = [f for f in flist if not os.path.isdir(f)]
        invalid_extensions = ['pkl', 'csv', 'txt', 'ini', 'dat', 'xls', 'xlsx', 'doc', 'docx', 'jpg', 'png']
        flist = [f for f in flist if f.split('.')[-1] not in invalid_extensions]

        # Look for spectra with no gas
        nogas_list = [f for f in flist if any([s in f for s in ['nogas']])]

        # ---------------------------------------------------------------------
        # Making self emission spectra
        # ---------------------------------------------------------------------
        wn = read_spectrum(nogas_list[0]).spectrum[0]
        idx = [i for i, x in enumerate(wn) if self.fit_window[0] <= x <= self.fit_window[1]]
        wn = wn[idx]
        self.nogas_x = wn

        n = len(nogas_list)

        times = np.empty(n, dtype=float)

        if n > 1:

            logger.info('Found %i spectra with no gas. Accounting for drift over time' % n)

            deg = min([n - 1, 2])
            nogas_spectra = np.empty([n, len(wn)], dtype=float)
            for i in range(n):
                f = read_spectrum(nogas_list[i])
                times[i] = datetime.timestamp(f.dtime)
                nogas_spectra[i, :] = f.spectrum[1][idx]
            self.nogas_poly = np.polyfit(times, nogas_spectra, deg)

        else:
            f = read_spectrum(nogas_list[-1])
            self.nogas = f.spectrum[1][idx]

        # Append lists sky name
        self.nogas_list = nogas_list

# ======================================================================================================================
#                                       Generate Source Emissivity spectrum
# ======================================================================================================================
    def makeSourceEmissivity(self, smooth=50):
        """."""

        wn = self.model_grid

        # Determine which source we are using
        if self.type == 'layer':

            # Read the emissivity file from THOMPSON ET AL. 2021
            if self.E_file is None:
                fname = './xsec/F_HI2_OUTPUT_Data.csv'
            else:
                fname = self.E_file
            df = pd.read_csv(fname, skiprows=5)
            n = len([i for i, x in enumerate(df['Wavenumber']) if (x - df['Wavenumber'][0]) < smooth])

            # Check that the wn axis is within the bounds of the supplied emissivity file
            if not (wn.min() >= df['Wavenumber'].min() and wn.max() <= df['Wavenumber'].max()):
                raise ValueError(
                    'Emissivity data only available over the range %i-%i cm-1 - Requested range of %i-%i cm-1 not (fully) covered'
                    % (df['Wavenumber'].min(), df['Wavenumber'].max(), wn.min(), wn.max()))

            # Check which temperatures are available
            temp = 1300
            logger.info('Using emissivity spectrum from BASALT source at %i K (THOMPSON ET AL. 2021)' % temp)
            y = df['%i' % temp]
            y = uniform_filter1d(y, size=n)
            self.E_model = griddata(df['Wavenumber'], y, wn, method='cubic')

        else:
            self.E_model = 1
            logger.info('EMISSIVITY OF SUN SOURCE NOT YET AVAILIABLE')

# ======================================================================================================================
#                                       Generate Average Residual
# ======================================================================================================================
    def makeResidual(self, smooth=20):

        fill ={'meas': 'measurement', 'bkg': 'background'}
        if self.res_dir is None:
            raise ValueError('No directory given to calculate reisudal from a previous fit')
        fdir = self.res_dir
        if not fdir.endswith('/'):
            fdir = fdir + '/'
        logger.info('Using average %s residual from previous fit: %s' % (fill[type], fdir))
        files = glob(fdir + 'spectral_fits/*_FIT0.csv')
        files.sort()
        fitResult = pd.read_csv(glob(fdir + 'fitResult*_ALL.csv')[0])
        df = pd.read_csv(files[0])

        nfiles = len(files)
        wn = df['wn']
        nwave = len(wn)
        spacing = wn[1] - wn[0]
        nsmooth = int(smooth / spacing)

        residuals = np.empty([nfiles, nwave], dtype=float)
        r2_thresh = 0.9
        for i, fname in enumerate(tqdm(files, desc='Extracting')):
            df = pd.read_csv(fname)
            res = 1 + (df['meas'] - df['model']) / df['meas']
            r2 = fitResult['MIN R2'][i]
            if r2 > r2_thresh:
                residuals[i, :] = res
            else:
                residuals[i, :] = np.ones(len(wn)) * np.nan

        res = np.nanmean(residuals, axis=0)
        res = uniform_filter1d(res, size=nsmooth)
        res = griddata(wn, res, self.model_grid, method='cubic', fill_value=1.0)
        self.res_model = res



# ======================================================================================================================
#                                               Pre-process Spectrum
# ======================================================================================================================
    def preProcess(self, spectrum, trim_type='cutoff'):
        """

        :param spectrum:
        :param trim_type:
        :return:
        """
        # Unpack spectrum
        new_spectrum = copy.deepcopy(spectrum)
        x, y = spectrum.spectrum

        # Subtract self emission background
        if self.subtract_self:
            try:
                timestamp = datetime.timestamp(spectrum.dtime)
                bkg = np.polyval(self.self_poly, timestamp)
            except:
                bkg = griddata(self.self_emission_x, self.self_emission, x)
            y = y - bkg

        # Isolate plume transmittance with no gas spectrum (Active geometry only)
        if self.type == 'layer' and self.no_gas:
            try:
                timestamp = datetime.timestamp(spectrum.dtime)
                nogas = np.polyval(self.nogas_poly, timestamp) - bkg
            except:
                nogas = griddata(self.nogas_x, self.nogas, x) - bkg
            y = y / nogas
            new_spectrum.spec_type = 'tra'

        # Remove zero and negative values
        if trim_type == 'cutoff':
            neg_values = [i for i, v in enumerate(y) if v <= 0]
            pos_values = [i for i, v in enumerate(y) if v > 0]
            y[neg_values] = min(y[pos_values])

        elif trim_type == 'offset':
            offset = min(y)
            if offset < 0:
             y = y + abs(offset * 1.01)

        # Cut desired wavelength window
        fit_idx = np.where(np.logical_and(x >= self.fit_window[0],
                                          x <= self.fit_window[1]))
        x = x[fit_idx]
        y = y[fit_idx]

        # Calibrate spectrum in emission fits
        if self.type == 'emission':
            try:
                timestamp = datetime.timestamp(spectrum.dtime)
                irf = np.polyval(self.irf_poly, timestamp)
                ri = np.polyval(self.ri_poly, timestamp)
            except:
                irf = self.irf
                ri = self.ri
            y = y / irf + ri
            new_spectrum.spec_type = 'rad'

            # Convert to Brightness Temperature
            if self.use_bbt:
                y = inversePlanck(x, y)
                new_spectrum.spec_type = 'bbt'

            # Calculate difference spectrum
            if self.fit_difference:
                try:
                    timestamp = datetime.timestamp(spectrum.dtime)
                    clear = np.polyval(self.clear_poly, timestamp)
                except:
                    clear = self.clear
                y = (y - clear)
                if self.use_bbt:
                    new_spectrum.spec_type = 'bbtdiff'
                else:
                    new_spectrum.spec_type = 'raddiff'
            else:
                pass

        if self.type == 'layer' and self.use_source_E:
            self.E_meas = griddata(self.model_grid, self.E_model, x)

        if self.type == 'layer' and self.use_residual:
            self.res_meas = griddata(self.model_grid, self.res_model, x)

        new_spectrum.update(spectrum=np.row_stack([x, y]))

        return new_spectrum

# ======================================================================================================================
#                                               Fit Spectrum
# ======================================================================================================================
    def fitSpectrum(self, spectrum, update_params=False, resid_type='Absolute', preProcess=True, use_bounds=True):
        """

        :param spectrum:
        :param update_params:
        :param resid_type:
        :param preProcess:
        :return:
        """

        # Get the right forward
        if self.type == 'emission':
            forwardModel = self.forwardEmission
        else:
            forwardModel = self.forwardTransmission

        # Check if spectrum requires preprocessing
        if preProcess:
            spectrum = self.preProcess(spectrum)

        # Unpack the spectrum
        grid, spec = spectrum.spectrum

        # Use parameters from previous fit?
        if len(self.from_df) > 0:
            i = [i for i, x in enumerate(self.df_dir['File']) if x == spectrum.fname.split('/')[-1]][0]
            for p in self.from_df:
                if p in self.params:
                    if p in possible_gases:
                        value = self.df_dir['%s_val' % p][i]
                    else:
                        value = self.df_dir[p][i]
                    self.params[p].value = value
        if len(self.seed_param) > 0:
            self.p0 = self.params.fittedValuesList()    # Reset initial guess based on updated values

        # Fit the spectrum
        try:
            if use_bounds:
                popt, pcov = curve_fit(forwardModel, grid, spec, p0=self.p0, bounds=self.bounds)
            else:
                popt, pcov = curve_fit(forwardModel, grid, spec, p0=self.p0)

            # Calculate the parameter error
            perr = np.sqrt(np.diag(pcov))

            # Set the success flag
            nerr = 1

        # If the fit fails return nans
        except:
            popt = np.full(len(self.p0), np.nan)
            perr = np.full(len(self.p0), np.nan)
            nerr = 0

        # Put the results into a FitResult object
        fit_result = FitResult(spectrum, self, popt, perr, nerr, forwardModel, resid_type)

        if fit_result.r2 < 0:
            popt = np.full(len(self.p0), np.nan)
            perr = np.full(len(self.p0), np.nan)
            nerr = 0
            fit_result = FitResult(spectrum, self, popt, perr, nerr, forwardModel, resid_type)

        # If the fit was good then update the initial parameters
        if update_params and fit_result.nerr == 1:
            self.p0 = popt
        else:
            self.p0 = self.params.fittedValuesList()

        return fit_result

# ======================================================================================================================
#                                          Transmission Forward Model
# ======================================================================================================================
    def forwardTransmission(self, wn, *p0):
        """

        :param wn:
        :param p0:
        :return:
        """

        # Get dictionary of fitted parameters
        params = self.params
        p = params.valuesDict()

        # Update the fitted parameter values with those supplied to the forward
        i = 0
        for par in params.values():
            if par.vary:
                p[par.name] = p0[i]
                i += 1
            else:
                p[par.name] = par.value

        # Unpack polynomial parameters and construct background polynomial
        poly_object = [p[n] for n in p if 'poly' in n]
        if len(poly_object) == 0:
            bg_poly = 1
        else:
            bg_poly = np.polyval(poly_object, self.model_grid)
        
        # Calculate the gas optical depth spectra for Atmospheric Gases
        OD_atm = 0
        atm_gases = [params[n].name for n in p if params[n].atm_gas]
        for gas in atm_gases:
            if gas in continuum_gases:
                OD_gas = np.polyval(self.reference['atm']['OD_calib'][gas], p[gas])
            else:
                OD_gas = p[gas] * np.polyval(self.reference['atm']['OD_calib'][gas], p['atm_temp'])
            OD_atm = OD_atm + OD_gas

        # Calculate the gas optical depth spectra for Plume Gases
        OD_plume = 0
        OD_plumeE = 0
        plume_gases = [params[n].name for n in p if params[n].plume_gas]
        for gas in plume_gases:
            if gas in continuum_gases:
                OD_gas = np.polyval(self.reference['plume']['OD_calib'][gas], p[gas])
            else:
                OD_gas = p[gas] * np.polyval(self.reference['plume']['OD_calib'][gas], p['gas_temp'])
                OD_gasE = p[gas] * np.polyval(self.reference['plume']['OD_calib'][gas], p['gasE_temp'])
            OD_plume = OD_plume + OD_gas
            OD_plumeE = OD_plumeE + OD_gasE

        # Add the optical depth spectra for Plume Aerosols
        plume_aero = [params[n].name for n in p if params[n].plume_aero]
        OD_aeros = 0
        for aero in plume_aero:
            OD_aero = p[aero] * np.polyval(self.reference['plume'][aero]['Bext'], np.log10(p[aero + '_deff']))
            OD_plume = OD_plume + OD_aero
            OD_aeros = OD_aeros + OD_aero

        # Calculate transmittance (and emissivity) of both layers
        T_atm = np.exp(-OD_atm)
        T_plume = np.exp(-OD_plume)
        E_plume = 1 - np.exp(-OD_plumeE)

        # Compute model spectrum at full resolution
        if self.no_gas:
            F_raw = T_plume * T_atm * bg_poly
            if self.retrieval.model_gas_emission:
                Bg = planck(self.model_grid, p['gasE_temp']) / planck(self.model_grid, p['source_temp'])
                F_raw = F_raw + Bg * E_plume * bg_poly
        else:
            F_raw = self.E_model * self.res_model * T_plume * T_atm * bg_poly
            if self.retrieval.model_gas_emission:
                Bg = planck(self.model_grid, p['gasE_temp']) / planck(self.model_grid, p['source_temp'])
                F_raw = F_raw + Bg * E_plume * self.res_model * T_atm * bg_poly

        # Generate ILS and convolve
        kernel = makeILS(fov=p['fov'], nper_wn=self.nper_wn, wn=(wn.max() - wn.min()) / 2, max_opd=p['max_opd'],
                         apod_type=self.apod_type)['kernel']
        F_conv = np.convolve(F_raw, kernel, 'same')

        # Apply offset
        F_offset = F_conv + p['offset']

        # Apply shift to the model_grid
        shift_model_grid = np.add(self.model_grid, p['nu_shift'])

        # Interpolate model spectrum onto spectrometer grid
        model = griddata(shift_model_grid, F_offset, wn, method='cubic')

        return model


# ======================================================================================================================
#                                          Emission Forward Model
# ======================================================================================================================
    def forwardEmission(self, wn, *p0):
        """

        :param wn:
        :param p0:
        :return:
        """
        # Get dictionary of fitted parameters
        params = self.params
        p = params.valuesDict()

        # Update the fitted parameter values with those supplied to the forward
        i = 0
        for par in params.values():
            if par.vary:
                p[par.name] = p0[i]
                i += 1
            else:
                p[par.name] = par.value

        # ----- 1: Build background polynomial -----
        poly_object = [p[n] for n in p if 'poly' in n]
        if len(poly_object) == 0:
            bg_poly = 1
        else:
            bg_poly = np.polyval(poly_object, self.model_grid)

        # # ----- 2: Calculate L and T for initial atmospheric layers -----
        if self.H2O_scaling == 3:    # Layer 3 (distal)
            L_dist = np.polyval(self.reference['atm_dist']['L_calib'], p['H2O_sc'])
        else:
            L_dist = self.reference['L_dist']
        if self.H2O_scaling >= 2:   # Layer 2 (plume slice)
            T_slice = np.polyval(self.reference['atm_slice']['T_calib'], p['H2O_sc'])
            L_slice = np.polyval(self.reference['atm_slice']['L_calib'], p['H2O_sc'])
        else:
            T_slice = self.reference['T_slice']
            L_slice = self.reference['L_slice']
        if self.H2O_scaling >= 1:   # Layer 1 (proximal)
            T_prox = np.polyval(self.reference['atm_prox']['T_calib'], p['H2O_sc'])
        else:
            T_prox = self.reference['T_prox']
        L_prox = planck(self.model_grid, self.geometry.obs_temp + p['dt_prox']) * (1 - T_prox)

        # ----- 4: Calculate T and L for the Plume layer----
        # First atm gases
        atm_gases = [params[n].name for n in p if params[n].atm_gas]
        OD_plume = 0
        for gas in atm_gases:
            if gas in continuum_gases:
                sign = p[gas] / np.abs(p[gas])
                OD_plume = OD_plume + sign * np.polyval(self.reference['atm']['OD_calib'][gas], np.abs(p[gas]))
            else:
                OD_plume = OD_plume + p[gas] * self.reference['atm'][gas]

        # Next plume gases
        plume_gases = [params[n].name for n in p if params[n].plume_gas]
        for gas in plume_gases:
            if gas in continuum_gases:
                sign = p[gas] / np.abs(p[gas])
                OD_plume = OD_plume + sign * np.polyval(self.reference['plume']['OD_calib'][gas], np.abs(p[gas]))
            else:
                OD_plume = OD_plume + p[gas] * self.reference['plume'][gas]

        # Next plume aerosols
        OD_plume2 = OD_plume.copy()
        plume_aeros = [params[n].name for n in p if params[n].plume_aero]
        for aero in plume_aeros:
                OD_plume = OD_plume + p[aero] * np.polyval(self.reference['plume'][aero]['Bext'], np.log10(p[aero + '_deff']))
                OD_plume2 = OD_plume + p[aero] * np.polyval(self.reference['plume'][aero]['Babs'], np.log10(p[aero + '_deff']))

        T_plume = np.exp(-OD_plume)
        if self.use_Babs_aero:
            T_plume2 = np.exp(-OD_plume2)
            L_plume = planck(self.model_grid, self.geometry.plume_temp + p['dt_plume']) * (1 - T_plume2)
        else:
            L_plume = planck(self.model_grid, self.geometry.plume_temp + p['dt_plume']) * (1 - T_plume)

        # ----- 5: Compute model spectrum at full resolution -----
        L_raw = L_dist * T_plume * T_slice * T_prox + (L_plume + L_slice) * T_prox + L_prox

        # Turn to BT (optional)
        if self.use_bbt:
            L_raw = inversePlanck(self.model_grid, L_raw)

        # Calculate difference if needed
        if self.fit_difference:
            if self.use_bbt:
                L_clear = inversePlanck(self.model_grid, self.L_clear_model)
            else:
                L_clear = self.L_clear_model
            F_raw = (L_raw - L_clear) * bg_poly
        else:
            F_raw = L_raw * bg_poly

        # ----- 6: Generate ILS, shift and offset and project onto spectrometer grid ------
        # ILS
        # kernel = makeILS1(fov=p['fov'], nper_wn=self.nper_wn, wn_start=wn.min())['kernel']
        kernel = makeILS(fov=p['fov'], nper_wn=self.nper_wn, wn=(wn.max() - wn.min()) / 2)['kernel']
        F_conv = np.convolve(F_raw, kernel, 'same')

        # Offset
        F_offset = F_conv + p['offset']

        # Shift
        shift_model_grid = np.add(self.model_grid, p['nu_shift'])

        # Interpolate
        model = griddata(shift_model_grid, F_offset, wn, method='cubic')

        return model


# ======================================================================================================================
# =========================================== Fit Result Object ========================================================
# ======================================================================================================================

class FitResult(object):
    """

    """

# ======================================================================================================================
#                                               Initialise Object
# ======================================================================================================================
    def __init__(self, spectrum, analyser, popt, perr, nerr, forwardModel, resid_type):
        """Initialize"""

        # Get the timestamp from spectrum
        self.dtime = spectrum.dtime

        # Assign the variables
        self.spec_type = spectrum.spec_type
        self.grid, self.spec = spectrum.spectrum
        self.params = analyser.params
        self.geometry = analyser.geometry
        self.popt = popt
        self.perr = perr
        self.nerr = nerr
        self.fwd_model = forwardModel
        self.resid_type = resid_type

        # Add the fit results to each parameter
        n = 0
        for par in self.params.values():

            if par.vary:
                par.set(fit_val=self.popt[n],
                        fit_err=self.perr[n])
                n += 1
            else:
                par.set(fit_val=par.value)
                par.set(fit_err=0)

        # If fit was successful then calculate the residual and error
        if self.nerr:

            # Generate the fit
            self.model = self.fwd_model(self.grid, *self.popt)

            # Calculate the residual
            if self.resid_type == 'Relative':
                self.res = (self.spec - self.model) / self.spec
            elif self.resid_type == 'Absolute':
                self.res = self.spec - self.model
            self.rmse = mean_squared_error(self.spec, self.model)
            self.r2 = r2_score(self.spec, self.model)

            # Replicate the background
            if self.geometry.type == 'emission':
                partial = self.params.fittedValuesList()
                for i, key in enumerate(self.params.poptDict().keys()):
                    if key in ['H2O_sc', 'dt_prox', 'nu_shift', 'fov']:
                        value = popt[i]
                        partial[i] = value
                self.bkg = self.fwd_model(self.grid, *partial)

            else:
                p_object = [self.params[x].fit_val for x in self.params if 'poly' in x]
                self.bg_poly = np.polyval(p_object, self.grid)
                # Emissivity spectrum
                if self.geometry.type == 'layer' and analyser.use_source_E:
                    self.E = analyser.E_meas
                else:
                    self.E = np.ones(len(self.grid))
                # Instrument residual
                if self.geometry.type == 'layer' and analyser.use_residual:
                    self.instr_res = analyser.res_meas
                else:
                    self.instr_res = np.ones(len(self.grid))
                # Offset
                if self.params['offset'].vary:
                    self.offset = np.ones(len(self.grid)) * self.params['offset'].fit_val
                else:
                    self.offset = np.zeros(len(self.grid))
                self.bkg = self.bg_poly * self.E * self.instr_res + self.offset

        # If not then return nans
        else:
            # logger.info('Fit failed!')
            self.model = np.full(len(self.spec), np.nan)
            self.res = np.full(len(self.spec), np.nan)
            self.bkg = np.full(len(self.spec), np.nan)
            self.E = np.full(len(self.spec), np.nan)
            self.instr_res = np.full(len(self.spec), np.nan)
            self.bg_poly = np.full(len(self.spec), np.nan)
            self.offset = np.full(len(self.spec), np.nan)
            self.rmse = np.nan
            self.r2 = np.nan
            # self.T = None






