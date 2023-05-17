"""
Written by @jfsmekens - Last updated 01/04/2022

This module contains functions for spectral analysis
Defines two object classes:

    Analyser - an object with functions to perform the spectral analysis
        makeReference           generate OD cross-sections to be used by forward model
        forwardEmission         forward model for emission cases (no IR source, cold sky background)
        preProcess              contains all the steps to go from raw single beam spectrum to ready to fit
                                (trim, offset, calibrate, calculate difference with clear spectrum if necessary)
        fitSpectrum             optimisation algorithm 
    
    FitResult- an object to hold the fit results
    
"""
import os
import logging

import numpy as np
from scipy.optimize import curve_fit
from scipy.interpolate import griddata
from datetime import datetime
import copy
from tqdm import tqdm
from sklearn.metrics import r2_score, mean_squared_error
import random

from atmosphere import *
from customXSC import *
from spectra import read_spectrum
from initialise import readConfig, readStandard
from constants import plot_colors, pretty_names
from parameters import Retrieval

logger = logging.getLogger(__name__)
continuum_gases = ['H2O']
# ======================================================================================================================
#                                               Planck function
# ======================================================================================================================
def planck(wave_x, temp):
    """
    This function computes the blackbody radiant energy curve using the Planck function for a given temperature

    :param wave_x: X-axis                                   [cm^-1]
    :param T: Temperature                                   [K]

    :return: Blackbody emission curve                       [mW / (m2 sr cm-1)]
    """
    # Define constants for radiance  [in mW / (m2 sr cm-1)]
    c1 = 1.191042E-5  # mW / (m-2 sr cm-4)
    c2 = 1.4387752  # K cm

    # Create theoretical BB radiance functions
    planck = c1 * wave_x ** 3 / (np.exp(c2 * wave_x / temp) - 1)

    return planck

# ======================================================================================================================
#                                    Inverse Planck function
# ======================================================================================================================
def inversePlanck(wave_x, rad):
    """
    This function computes the blackbody radiant energy curve using the Planck function for a given temperature

    :param wave_x: X-axis                                   [cm^-1]
    :param rad: Radiance                                    [mW / (m2 sr cm-1)]

    :return: Blackbody emission curve                       [K]
    """
    # Define constants for radiance  [in mW / (m2 sr cm-1)]
    c1 = 1.191042E-5  # mW / (m-2 sr cm-4)
    c2 = 1.4387752  # K cm

    # Create theoretical BB radiance functions
    planck = c2 * wave_x / np.log(c1 * wave_x ** 3 / rad + 1)

    return planck

# ======================================================================================================================
#                                    Instrument Line Shape function
# ======================================================================================================================
def ils(fov=0.030, ftir_opd=1.0, n_per_wave=25, start_wave=1000, apod_flag=1):
    """
    This function calculates the Instrument Line Shape (ILS) and computes a kernel to convolve high resolution spectra and
    degrade them to instrument resolution

    SYNTAX: out = ils(fov = 0.004, ftir_opd = 1.0, n_per_wave = 25, start_wave = 2000, apod_flag = 1)

    :param fov: Field of View of the instrument
                    Default is 0.004 (Bruker EM27)
    :param ftir_opd: Optical Path Difference of the FTIR instrument                                         [cm]
                    Default is 1.0 (Bruker EM27)
    :param n_per_wave: Resolution of the spectrum to be smoothed                                            [per cm-1]
                    Default is 25
    :param start_wave: Starting wavelength for the spectrum                                                 [cm-1]
                    Default is 600
    :param apod_flag: Which apodization function to use
                    # 0 = boxcar
                    # 1 = triangular
                    # 2 = NB weak
                    # 3 = NB medium
                    # 4 = NB strong
                    Default is 1

    :return: dictionary with the following structure:
                                                                             Units
        ['grid']       The wavenumber grid for the kernel                   [cm^-1]
        ['kernel']      The kernel itself
        ['FOV-width']   The width of the Field of View                      [cm]
        ['shift]        The predicted shift                                 [cm]

    """

    # --------------- DEFINE STARTING PARAMETERS  ---------------------
    # Define the total OPD as [in cm] the sampling frequency
    total_opd = n_per_wave

    # Set the number of points in the interferograms (IGM)
    n_total_igm = int(total_opd * n_per_wave)         # Total IGM
    n_ftir_igm = int(ftir_opd * n_per_wave)           # FTIR IGM
    n_filler_igm = int(n_total_igm - n_ftir_igm)     # Filler (difference between the two)

    # Create the IGM grids (in cm)
    total_igm_grid = np.linspace(0, total_opd, n_ftir_igm)
    ftir_igm_grid = np.linspace(0, ftir_opd, n_ftir_igm)

    # Create empty IGM arrays
    ftir_igm = np.zeros(n_ftir_igm)
    filler_igm = np.zeros(n_filler_igm)

    # ---------------------- GENERATE THE IGMS ------------------------
    # Flag = 1: Triangular
    if apod_flag == 1:
        ftir_igm = (np.arange(n_ftir_igm, dtype=float) / (n_ftir_igm-1))[::-1]

    else:
        # Flag = 0: Boxcar
        if apod_flag == 0:
            c =[1, 0, 0, 0]
        # Flag = 2: NB weak
        if apod_flag == 2:
            c =[0.348093, -0.087577, 0.703484, 0.0]
        # Flag = 3: NB medium
        if apod_flag == 3:
            c =[0.152442, -0.136176, 0.983734, 0]
        # Flag = 4: NB strong
        if apod_flag == 4:
            c =[0.045335, 0.0, 0.554883, 0.399782]
        # Now build
        for i in range(4):
            ftir_igm = ftir_igm + c[i] * ((1 - (ftir_igm_grid / ftir_opd) ** 2)) ** i

    # Fill the rest of the signal with zeros
    igm = np.concatenate((ftir_igm, filler_igm))

    # ---------------------- RECONSTRUCT KERNEL ------------------------
    # Apply Fourier Transform to reconstruct spectrum
    spc = np.fft.fft(igm).real

    # Split the spectrum in the middle and mirror around axis
    middle = n_total_igm // 2           # Find middle point
    spc_right = spc[0:middle]         # Beginning of the signal is the tapering edge (right side of the kernel)
    spc_left = spc[middle:]             # End of the signal is the rising edge (left side of the kernel)
    spc = np.concatenate((spc_left, spc_right))  # Reconstruct spectrum around the middle point

    # Remove offset in spectrum
    offset = spc[0:middle//2].mean(axis=0)
    spc = spc - offset

    # FOV effect is like a boxcar in freq space
    fov_width = start_wave * (1 - np.cos(fov/2))      # in [cm^-1]
    shift = start_wave * (1 - np.cos(fov/2)) * 0.5    # in [cm^-1]
    n_box = int(np.ceil(fov_width * n_per_wave))
    if n_box < 0 or n_box > middle:
        n_box = 5
    boxcar = np.ones(n_box) * 1 / n_box
    spc_fov = np.convolve(spc, boxcar, 'same')
    spc_fov = spc_fov / spc_fov.sum()

    # Create the wavenumber grid centred on zero
    wave = np.linspace(-total_opd / 2, total_opd / 2, n_total_igm)

    # select the middle +/- 5 wavenumbers (minimum window size is then 10 cm^-1)
    start = int(middle - 5 * n_per_wave)
    stop = int(middle + 5 * n_per_wave)
    kernel = spc_fov[start:stop]
    kernel = kernel / kernel.sum(axis=0)
    grid = wave[start:stop]

    # Populate kernel array
    out = {}
    out['grid'] = grid
    out['kernel'] = kernel
    out['fov_width'] = fov_width
    out['shift'] = shift

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
    def __init__(self, params, geometry, fit_window, retrieval=None, model_padding=50, model_spacing=0.04,
                 name='FIT', data_dir=None, subtract_self=False, fit_size_aero=False, force_ref=False):
        """Initialise the model for the Analyser"""

        # Fit name
        self.name = name

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

        # Generate model wavenumber grid
        self.model_padding = model_padding
        self.model_spacing = model_spacing
        self.n_per_wave = 1 / self.model_spacing
        self.fit_window = fit_window
        self.start_wave = self.fit_window[0] - self.model_padding
        self.end_wave = self.fit_window[1] + self.model_padding
        self.model_grid = np.arange(self.start_wave, self.end_wave + self.model_spacing, step=self.model_spacing)

        # If retrieval is supplied use this to extract parameters
        if retrieval is None:
            self.retrieval = Retrieval(type=self.type)

        # Update info about aerosols
        self.use_Babs_aero = retrieval.use_Babs_aero
        self.fit_size_aero = retrieval.fit_size_aero

        # Extract spectrum wavelength grid as well?

        # ---------------------------------------------------------------------
        # Generate calibration and clear sky for emission
        # ---------------------------------------------------------------------
        if self.type == 'layer':
            self.subtract_self = retrieval.subtract_self
            self.no_gas = retrieval.no_gas
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

        # ---------------------------------------------------------------------
        # Extract self emission background
        # ---------------------------------------------------------------------
        if self.subtract_self:
            self.makeSelfEmission()

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
        ref['wave'] = self.model_grid

        logger.info('########## Reference cross-sections for %s - type %s ##########'
                    % (self.name.upper(), self.type.upper()))

       
        # ---------------------------------------------------------------------
        # 3: Emission geometry
        # ---------------------------------------------------------------------
        if self.type == 'emission':

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
                        self.start_wave, self.end_wave,
                        self.model_spacing,
                        self.params.atmGasList(),
                        self.params.plumeGasList(),
                        self.params.plumeAeroList())

            # Compare headers to see if any of the saved references are a match
            old_refs = glob.glob(self.data_dir + '/emissionReference*.pkl')
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
                    writeRFMdrv(self.start_wave, self.end_wave, self.n_per_wave, layer=False, atm=scaled,
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
                        writeRFMdrv(self.start_wave, self.end_wave, self.n_per_wave, layer=False, atm=scaled,
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
                    writeRFMdrv(self.start_wave, self.end_wave, self.n_per_wave, layer=False, atm=scaled,
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
                            ODs[:, j] = makeGasXSC(self.start_wave, self.end_wave, self.n_per_wave, conc=ref_conc * sc,
                                        species=gas)['Bext'] * self.geometry.pathlength * 1e3 \

                            if sc == 1.0:
                                ref['atm'][gas] = ODs[:, j]

                            pbar.update()
                        ref['atm']['OD_calib'][gas] = np.polyfit(scalings, ODs.T, 10)

                    else:
                        pbar.set_description('%s' % gas)
                        ref['atm'][gas] = makeGasXSC(self.start_wave, self.end_wave, self.n_per_wave, conc=ref_conc,
                                                     species=gas)['Bext'] * self.geometry.pathlength * 1e3

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
                            makeGasXSC(self.start_wave, self.end_wave, self.n_per_wave, conc=ref_conc * sc,
                                       species=gas)['Bext'] * self.geometry.pathlength * 1e3

                            if sc == 1.0:
                                ref['atm'][gas] = ODs[:, j]
                            pbar.update()

                        ref['plume']['OD_calib'][gas] = np.polyfit(scalings, ODs.T, 10)

                    else:
                        pbar.set_description('%s' % gas)
                        ref['plume'][gas] = \
                        makeGasXSC(self.start_wave, self.end_wave, self.n_per_wave, conc=ref_conc,
                                   species=gas)['Bext'] * self.geometry.pathlength * 1e3

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
                            xsec = makeAerosolXSC(self.start_wave, self.end_wave, self.n_per_wave, 10 ** size,
                                                  mass_conc, species=aero, comp=comp)

                        # When size is a single size
                        else:
                            xsec = makeAerosolXSC(self.start_wave, self.end_wave, self.n_per_wave, 10 ** size,
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
    def plotReference(self, save=False, outdir='./plots/'):

        # Assign local variables
        std = readStandard()
        ref = self.reference
        wave = self.model_grid
        padding = self.model_padding
        n_atm = len(self.params.atmGasList())
        n_plume = len(self.params.plumeGasList()) + len(self.params.plumeAeroList())
        depth = 0.75
        if wave.max() - wave.min() > 5 * padding:
            pad = 2
        else:
            pad = 1
        if self.type == 'emission':
            atmT = 'Transmittance'
            plumeT = 'Absorbance'
        else:
            atmT = 'Transmittance'
            plumeT = 'Transmittance'

        idx = [i for i, w in enumerate(wave) if wave.min() + pad * padding <= w <= wave.max() - pad * padding]

        # Open figure
        fig, ax = plt.subplots(figsize=[10, 6])
        ax2 = plt.twinx(ax)

        # Add title
        fig.suptitle('%s - %s' % (self.name, self.type.upper()))

        for gas in self.params.atmGasList():
            if gas == 'H2O':
                sc = 1.0
            else:
                sc = - np.log(1 - depth) / ref['atm'][gas][idx].max()
            T = np.exp(-ref['atm'][gas] * sc)
            ax.plot(wave, T, c=plot_colors[gas], zorder=0, alpha=0.5, label='%s' % pretty_names[gas])

        for gas in self.params.plumeGasList():
            if gas == 'H2O':
                sc = 1.0
            else:
                sc = - np.log(1 - depth) / ref['plume'][gas][idx].max()
            if self.type == 'emission':
                A = 1 - np.exp(-ref['plume'][gas] * sc)
                ax2.plot(wave, A, c=plot_colors[gas], zorder=1, alpha=1.0, label='%s' % pretty_names[gas])
            else:
                T = np.exp(-ref['plume'][gas] * sc)
                ax2.plot(wave, T, c=plot_colors[gas], zorder=1, alpha=1.0, label='%s' % pretty_names[gas])

        for aero in self.params.plumeAeroList():
            sc = - np.log(1 - depth) / np.polyval(ref['plume'][aero]['Bext'], 1.0).max()
            if self.type == 'emission':
                A = 1 - np.exp(-np.polyval(ref['plume'][aero]['Bext'], 1.0) * sc)
                ax2.plot(wave, A, c=plot_colors[aero], zorder=1, alpha=1.0, label='%s' % pretty_names[gas])
            else:
                T = np.exp(-np.polyval(ref['plume'][aero]['Bext'], 1.0) * sc)
                ax2.plot(wave, T, c=plot_colors[aero], zorder=1, alpha=1.0, label='%s' % pretty_names[gas])

        ax.set(xlabel='Wavenumber [$cm^{-1}$]', ylabel='%s' % atmT, ylim=(0, 1))
        ax2.set(ylabel='%s' % plumeT, ylim=(0, 1))

        ax.legend(title='Atmosphere (%s - %.2g km)' % (atmT, self.geometry.pathlength), bbox_to_anchor=(0., 1.02, 0.5, .102),
                  loc='lower left', ncol=n_atm // 2 + n_atm % 2)
        ax2.legend(title='Plume (%s - %.2g km)' % (plumeT, self.geometry.pathlength), bbox_to_anchor=(0.5, 1.02, 0.5, .102),
                   loc='lower right', ncol=n_plume // 2 + n_plume % 2)

        # plt.tight_layout(rect=[0, 0, 1, 0.95])
        plt.tight_layout()
        plt.show(block=False)

        if save:
            plt.savefig(outdir + 'reference.png')

# ======================================================================================================================
#                                        Generate calibration
# ======================================================================================================================
    def makeCalibration(self):

        logger.info('Generating calibration from BB files')

        # List all files in directory
        flist = glob.glob(self.data_dir + '*')

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
                wave = read_spectrum(bbc_list[i]).spectrum[0]
                idx = [i for i, x in enumerate(wave) if self.fit_window[0] <= x <= self.fit_window[1]]
                wave = wave[idx]
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
            irf = np.empty([n, len(wave)], dtype=float)
            ri = np.empty([n, len(wave)], dtype=float)
            for i in range(n):
                # Read in sbm spectra
                BBC_sb = bbc[i]
                BBH_sb = bbh[i]

                # Create theoretical BB radiance functions
                planck_C = planck(wave, TC)
                planck_H = planck(wave, TH)

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
            planck_C = planck(wave, TC)
            planck_H = planck(wave, TH)

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
        flist = glob.glob(self.data_dir + '*')

        # Only keep the potential spectra files
        flist = [f for f in flist if not os.path.isdir(f)]
        invalid_extensions = ['pkl', 'csv', 'txt', 'ini', 'dat', 'xls', 'xlsx', 'doc', 'docx', 'jpg', 'png']
        flist = [f for f in flist if f.split('.')[-1] not in invalid_extensions]

        # Look for clear and blackbody spectra and remove from main list
        clear_list = [f for f in flist if any([s in f for s in ['clear', 'sky']])]

        # ---------------------------------------------------------------------
        # Making clear spectra
        # ---------------------------------------------------------------------
        wave = read_spectrum(clear_list[0]).spectrum[0]
        idx = [i for i, x in enumerate(wave) if self.fit_window[0] <= x <= self.fit_window[1]]
        wave = wave[idx]

        n = len(clear_list)

        times = np.empty(n, dtype=float)

        if n > 1 and self.clear_drift:

            logger.info('Found %i clear sky spectra. Accounting for drift over time' % n)

            deg = min([n-1, 2])
            clear = np.empty([n, len(wave)], dtype=float)
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
#                                        Generate self emission background
# ======================================================================================================================
    def makeSelfEmission(self):

        logger.info('Generating self emission background')

        # List all files in directory
        flist = glob.glob(self.data_dir + '*')

        # Only keep the potential spectra files
        flist = [f for f in flist if not os.path.isdir(f)]
        invalid_extensions = ['pkl', 'csv', 'txt', 'ini', 'dat', 'xls', 'xlsx', 'doc', 'docx', 'jpg', 'png']
        flist = [f for f in flist if f.split('.')[-1] not in invalid_extensions]

        # Look for clear and blackbody spectra and remove from main list
        self_list = [f for f in flist if any([s in f for s in ['self']])]

        # ---------------------------------------------------------------------
        # Making self emission spectra
        # ---------------------------------------------------------------------
        wave = read_spectrum(self_list[0]).spectrum[0]
        idx = [i for i, x in enumerate(wave) if self.fit_window[0] <= x <= self.fit_window[1]]
        wave = wave[idx]
        self.self_emission_x = wave

        n = len(self_list)

        times = np.empty(n, dtype=float)

        if n > 1:

            logger.info('Found %i self emission spectra. Accounting for drift over time' % n)

            deg = min([n - 1, 2])
            self_spectra = np.empty([n, len(wave)], dtype=float)
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


    def findEnvelope(self, spectrum, min_T=0.99, margin=1.01):

        # Unpack spectrum
        wave, spec = spectrum.spectrum

        # Generate T reference spectrum with all gases
        x = self.reference['wave']
        y = 0
        for key in self.reference['atm'].keys():
            if key not in ['ref_column', 'OD_calib', 'H2O']:
                y = y + self.reference['atm'][key]
        for key in self.reference['plume'].keys():
            if key not in ['ref_column', 'OD_calib', 'H2O']:
                y = y + self.reference['plume'][key]
        T = np.exp(-y)

        # Project T spectrum onto spectrum grid
        new_T = griddata(x, T, wave)

        # Find indices of T matching threshold (T ~= 1)
        idx = [i for i, val in enumerate(new_T) if val > min_T]

        # What is the degree of the polynomial used in this fit?
        polydeg = len([p for p in self.params if 'poly' in p]) - 1

        # Fit polynomial through unity values only
        poly_object = np.polyfit(wave[idx], spec[idx] * margin, polydeg)

        # Find H2O continuum
        if 'H2O' in self.params.atmGasList():
            T_H2O = np.exp(-self.reference['atm']['H2O'])
        elif 'H2O' in self.params.plumeGasList():
            T_H2O = np.exp(-self.reference['plume']['H2O'])
        else:
            T_H2O = 1
        # Update polynomial parameters to fixed value
        for p in self.params:
            if 'poly' in p:
                i = int(p.replace('poly', ''))
                self.params[p].set(value=poly_object[i], vary=False)

        # Reset p0
        self.p0 = self.params.fittedValuesList()


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

        new_spectrum.update(spectrum=np.row_stack([x, y]))

        return new_spectrum

# ======================================================================================================================
#                                               Fit Spectrum
# ======================================================================================================================
    def fitSpectrum(self, spectrum, update_params=True, resid_type='Absolute', preProcess=True, use_bounds=True,
                    findEnvelope=False):
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

        # Check if spectrum requires preprocessing
        if preProcess:
            spectrum = self.preProcess(spectrum)

        # Fix polynomial envelope
        if findEnvelope:
            self.findEnvelope(spectrum)

        # Unpack the spectrum
        grid, spec = spectrum.spectrum

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
        fit_result = FitResult(spectrum, self.params, self.geometry, popt, perr, nerr,
                               forwardModel, resid_type)

        if fit_result.r2 < 0:
            popt = np.full(len(self.p0), np.nan)
            perr = np.full(len(self.p0), np.nan)
            nerr = 0
            fit_result = FitResult(spectrum, self.params, self.geometry, popt, perr, nerr,
                                   forwardModel, resid_type)

        # If the fit was good then update the initial parameters
        if update_params and fit_result.nerr == 1:
            self.p0 = popt
        else:
            # logger.info('Resetting initial guess parameters')
            self.p0 = self.params.fittedValuesList()

        return fit_result

# ======================================================================================================================
#                                          Emission Forward Model
# ======================================================================================================================
    def forwardEmission(self, wave_x, *p0):
        """

        :param wave_x:
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
        kernel = ils(fov=p['fov'], n_per_wave=self.n_per_wave, start_wave=wave_x.min())['kernel']
        F_conv = np.convolve(F_raw, kernel, 'same')

        # Offset
        F_offset = F_conv + p['offset']

        # Shift
        shift_model_grid = np.add(self.model_grid, p['nu_shift'])

        # Interpolate
        model = griddata(shift_model_grid, F_offset, wave_x, method='cubic')

        return model



# ======================================================================================================================
#                                               Pretty Print
# ======================================================================================================================
    def prettyPrint(self, width=99):

        # Define sections
        atm_gases = self.params.atmGasList()
        plume_gases = self.params.plumeGasList()
        plume_aero = self.params.plumeAeroList()
        all = atm_gases + plume_gases + plume_aero
        cols = ['Parameter', 'Fit?', 'A priori', 'Min', 'Max']
        colwidth = [width // 6 * 2 - 1, width // 6, width // 6, width // 6, width // 6]

        # Title
        msg = '\n'
        msg += f'{"=" * (width + 2)}\n'
        title = "%s MODEL PARAMETERS- %s GEOMETRY" % (self.name, self.geometry.type.upper())
        msg += '|' + f'{title:^{width}}' + '|\n'
        msg += f'{"=" * (width + 2)}\n'

        # ---------------------------------------------------------------------
        # Geometry drawing
        # ---------------------------------------------------------------------
        if self.geometry.type == 'emission':
            msg += '|' + '                  /'.ljust(width, ' ') + '|\n'
            msg += '|' + '                 /'.ljust(width // 2, ' ') + ' ' + 'ATMOSPHERE '.center(width // 2,
                                                                                                  ' ') + '|\n'
            msg += '|' + '                /'.ljust(width // 2,
                                                   ' ') + ' ' + f'{"[%s]" % ", ".join(atm_gases):^{width // 2}}' + '|\n'
        msg += '|' + '               /'.ljust(width // 2 - 10, ' ') + ''.center(21, '_') \
               + ''.rjust(width // 2 - 10, ' ') + '|\n'
        msg += '|' + '              /'.ljust(width // 2 - 11, ' ') + '/' + 'PLUME LAYER'.center(21, ' ') \
               + '\ '.ljust(width // 2 - 10, ' ') + '|\n'
        msg += '|' + '             /'.ljust(width // 2 - 12, ' ') + '/ ' + (
                    'Temp: %i K' % self.geometry.plume_temp).center(21, ' ') \
               + ' \ '.ljust(width // 2 - 10, ' ') + '|\n'
        msg += '|' + '___________ /'.ljust(width // 2 - 13, '_') + '/ ' + (
                    'Pres: %i mb' % self.geometry.plume_pres).center(21, ' ') \
               + '   \_' + (' Height: %.2f km ' % self.geometry.plume_height).center(width // 2 - 14, '_') + '|\n'
        msg += '|' + '           /'.ljust(width // 2 - 13, ' ') + '\ ' + ("[%s]" % ', '.join(plume_gases)).center(21,
                                                                                                                  ' ') \
               + '   /' + (' Thickness: %.2f km ' % self.geometry.plume_thickness).center(width // 2 - 13,
                                                                                              ' ') + '|\n'
        msg += '|' + '          /'.ljust(width // 2 - 12, ' ') + '\ ' + ("[%s]" % ', '.join(plume_aero)).center(21, ' ') \
               + ' / '.ljust(width // 2 - 10, ' ') + '|\n'
        msg += '|' + '         /'.ljust(width // 2 - 11, ' ') + '\_' + ''.center(20, '_') \
               + '/'.ljust(width // 2 - 10, ' ') + '|\n'
        msg += '|' + '      __/' + (' Angle: %.1f deg' % self.geometry.elev).ljust(width - 9, ' ') + '|\n'
        msg += '|' + '     |__|------------------'.ljust(width, ' ') + '|\n'
        msg += '|' + '      /\ '.ljust(width // 2, ' ') + ' ' + \
               ('Observer height: %.2f km' % self.geometry.obs_height).center(width // 2, ' ') + '|\n'

        msg += f'{"=" * (width + 2)}\n'

        # ---------------------------------------------------------------------
        # Fit model parameters
        # ---------------------------------------------------------------------
        msg += '|' + " FIT WINDOW:".ljust(25, ' ') + (
                    "%i - %i cm^-1" % (self.fit_window[0], self.fit_window[1])).ljust(width - 25) + '|\n'
        if len(atm_gases) == 0: atm_gases = ['None']
        msg += '|' + " ATMOSPHERIC GASES:".ljust(25, ' ') + ("%s" % ', '.join(atm_gases)).ljust(width - 25) + '|\n'
        if len(plume_gases) == 0: plume_gases = ['None']
        msg += '|' + " PLUME GASES:".ljust(25, ' ') + ("%s" % ', '.join(plume_gases)).ljust(width - 25) + '|\n'
        if len(plume_aero) == 0: plume_aero = ['None']
        msg += '|' + " PLUME AEROSOLS:".ljust(25, ' ') + ("%s" % ', '.join(plume_aero)).ljust(width - 25) + '|\n'

        msg += '|' + f'{"-" * width}' + '|\n'

        title = ''
        for n, c in enumerate(cols):
            title += f'|{c:^{colwidth[n]}}'
        msg += title + '|\n'

        msg += '|' + f'{"-" * width}' + '|\n'

        for name, p in self.params.items():
            if name not in all:
                cols = [name, f'{p.vary}', f'{p.value}', f'{p.min}', f'{p.max}']
                line = ''
                for n, c in enumerate(cols):
                    line += f'|{c:^{colwidth[n]}}'
                msg += line + '|\n'

        msg += f'{"=" * (width + 2)}\n'

        # Pass to logger for display
        logger.info(msg)


# ======================================================================================================================
# =========================================== Fit Result Object ========================================================
# ======================================================================================================================

class FitResult(object):
    """

    """

# ======================================================================================================================
#                                               Initialise Object
# ======================================================================================================================
    def __init__(self, spectrum, params, geometry, popt, perr, nerr, fwd_model,
                 resid_type):
        """Initialize"""

        # Get the timestamp from spectrum
        self.dtime = spectrum.dtime
        # Make a copy of the parameters
        self.params = params

        # Assign the variables
        self.spec_type = spectrum.spec_type
        self.grid, self.spec = spectrum.spectrum
        self.geometry = geometry
        self.popt = popt
        self.perr = perr
        self.nerr = nerr
        self.fwd_model = fwd_model
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
            if self.resid_type == 'Percentage':
                self.res = (self.spec - self.model) / self.spec * 100
            else:
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

        # If not then return nans
        else:
            # logger.info('Fit failed!')
            self.model = np.full(len(self.spec), np.nan)
            self.res = np.full(len(self.spec), np.nan)
            self.bkg = np.full(len(self.spec), np.nan)
            self.rmse = np.nan
            self.r2 = np.nan



