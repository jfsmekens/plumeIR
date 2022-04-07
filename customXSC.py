"""
Written by @jfsmekens - Last updated 01/04/2022

This module contains functions to create optical depth cross-sections:
    1. For gases by using RFM called outside of python
    2. For particles by using PyMieScatt

"""
import numpy as np
import pandas as pd
import scipy.stats as stats
import pickle
import jcamp
import PyMieScatt
from subprocess import run, DEVNULL
from atmosphere import *
import glob
from tqdm import tqdm
from constants import *

# ---- Small function tool to make gaussian profile ---
def gauss(x, mu, sig):
    """
    This function creates a Gaussian profile with maximum value of 1, spread across the coordinates in x

    :param x: array of heights along which to create the distribution
    :param mu: mean of the Gaussian (mid-point of the plume)
    :param sig: standard deviation of the Gaussian

    :return: Gaussian distribution
    """
    return np.exp(-np.power(x - mu, 2.) / (2 * np.power(sig, 2.)))
# --------------------------------------------------------

# ======================================================================================================================
#                                    Create a Particle Size Distribution
# ======================================================================================================================
def makePSD(mean=1.0, sigma=1.5, n=50, fname=None):
    """
    This function creates a PSD with one of two methods:
    1.  Log-normal distribution using user-defined geometric mean and standard deviation
    2.  Reading a .csv file with the size bins [inn um] and size fraction [in decimal fraction, not %]

    :param mean: Geometric mean of the log-normal distribution. Default is 1.                                  [um]
    :param sigma: Geometric standard deviation for the log-ormal distribution. Default is 1.5        [dimensionless]
    :param n: The number of bins. Default is 50
    :param fname: Pathname to a .csv file containing a size distribution. Must be strictly formatted with columns
                                    size           fraction
                                    [um]          [decimal]
                            1      value            value
                            2      value            value
                            ...    value            value
                            n      value            value

    :return: Dictionary with the following structure
                ['header']          Header information. Formatted to fit with RFM headers
                ['bins']            Center value for size bins                                                  [um]
                ['frac']            Relative size fraction                                           [dimensionless]
                ---------- Statistical Parameters -----------         Extracted from data if PSD is supplied in .csv
                ['mean']            Geometric mean                                                              [um]
                ['sigma']           Geometric standard deviation                                     [dimensionless]

    """
    if fname is None:
        # ---------------------------------------------------------------------
        # 1: Create Log normal distribution
        # ---------------------------------------------------------------------
        # Create regularly spaced X-axis (in log space, +/- 3 sigma)
        x = np.linspace(np.log(mean) - 3 * np.log(sigma), np.log(mean) + 3 * np.log(sigma), n)

        # Log normal distribution is essentially a Gaussian in log space
        frac = gauss(x, np.log(mean), np.log(sigma))
        frac = frac / np.sum(frac)  # Normalise so that sum is 100 %

        # Create output structure
        out = {}
        out['header'] = "! Log normal PSD with mean= %.3f um and sigma= %.3f" % (mean, sigma)
        out['bins'] = np.exp(x)
        out['frac'] = frac
        out['mean'] = mean
        out['sigma'] = sigma

    else:
        # ---------------------------------------------------------------------
        # 2: Get PSD from a CSV file
        # ---------------------------------------------------------------------
        file = pd.read_csv(fname)

        # Separate size bins and fractions
        bins = np.array(file['size'].array)
        frac = np.array(file['fraction'].array)

        # Trim the PSD where fraction is null
        idx = np.where(frac != 0)
        bins = bins[idx]
        frac = frac[idx]

        # Distribute 1e6 particles along this distribution and get stats
        dist = np.empty(0)
        for i, x in enumerate(bins):
            dist = np.append(dist, np.ones(int(frac[i] * 1e6)) * x)
        mean = stats.gmean(dist)
        sigma = stats.gstd(dist)

        # Flip the arrays if they are reversed
        if bins[0] > bins[-1]:
            bins = np.flip(bins)
            frac = np.flip(frac)

        # Populate output structure
        out = {}
        out['header'] = "! PSD from file: %s" % fname
        out['bins'] = bins
        out['frac'] = frac
        out['mean'] = mean
        out['sigma'] = sigma

    return out

# ======================================================================================================================
#                                   Make custom cross-sections for odd gases
# ======================================================================================================================
def makeGasXSC(start_wave, end_wave, nperwave, conc=1.0, temp=298, pres=1013, species='sif4'):
    """
    Use this function to make cross-sections for gases not in the Hitran database. Each gas is manually added in the
        ./xsec/ folder as a JCAMP-JDX file (.jdx) taken from the NIST website. Reference experiment environemental data
        (P, T, pathlength) are hard coded in the function and updated upon user request.

        Current gases: ['SiF4']

    :param start_wave:  Lower bound of the frequency range for x-axis                           [cm^-1]
    :param end_wave:    Higher bound of the frequency range for x-axis                          [cm^-1]
    :param nperwave:    Resolution of the frequency range for x-axis                            [{cm^-1}^-1]
    :param conc:        Concentration for the cross-section                                     [ppm]
    :param temp:        Temperature                                                             [K]
    :param pres:        Pressure                                                                [mbar]
    :param species:     Gas formula (case insensitive)                                          String
    :return: Dictionary with structure:
                ['wave']:           Wavenumber x-axis                                           [cm^-1]
                ['Bext']:           Extinction coefficients                                     [m^-1]
                ['conc']:           Gas concenntration used to calculate cross-section          [ppm]
    """

    # Create wavenumber x-axis
    npts = int((end_wave - start_wave) * nperwave) + 1
    wave = np.linspace(start_wave, end_wave, npts)

    # If no species is given
    if species is None:
        raise ValueError('Please supply a species name from the following: %s' % possible_species)

    elif species in special_species:

        # ---------------------------------------------------------------------
        # Read in spectrum from JDX file
        # ---------------------------------------------------------------------
        # Read the absorbance data (from the NIST webbook in JCAMP-JDX format)
        fname = './xsec/GAS_%s.jdx' % species.lower()
        data = jcamp.JCAMP_reader(fname)

        # Extract spectrum and sample onto wavenumber grid
        if data['yunits'] == 'ABSORBANCE':
            A = np.interp(wave, data['x'], data['y'])
        elif data['yunits'] == 'TRANSMITTANCE':
            A = 1 - np.interp(wave, data['x'], data['y'])
        elif 'micromol/mol' in data['yunits']:
            xsec = np.interp(wave, data['x'], data['y'])

        # ---------------------------------------------------------------------
        # Determine experiment conditions and calculate cross-section
        # ---------------------------------------------------------------------
        if not 'micromol/mol' in data['yunits']:
            R = 8.31446261815324  # R constant [in m^3⋅Pa⋅K^−1⋅mol^−1]

            # Establish conditions in which measurement was made
            if species.lower() == 'sif4':
                pres_ref = 65328  # Partial pressure of target gas from NIST webbook entry (490 mmHg) - [in Pa]
                temp_ref = 293.15  # "Room" temperature for NIST experiments [in K]
                Lref = 0.05  # Standard path length for NIST experiments [in m]
            else:
                raise ValueError('Gas species %s currently not supported. '
                                 'You may add the JDX file for the desired gas in the ./xsec/ folder '
                                 'and update the reference pressure manually in the source code of the makeGasXSC() functionn'
                                 'in ./plumeIR/customXSC.py' % species.upper())

            # Number density of target gas in experiment
            Nref = pres_ref / (R * temp_ref)    # in [mol.m^-3]

            # Derive cross-section
            xsec = A / Lref / Nref  # [in m^2.mol^-1]

        # ---------------------------------------------------------------------
        # Calculate extinction coefficients for desired environment (conc, P, T)
        # ---------------------------------------------------------------------
        # Calculate desired number density
        N = conc * 1e-6 * pres * 1e2 / (R * temp)   # [in mol.m^-3]

        # Get extinction coefficient
        Bext = xsec * N     # [in m^-1]


    elif species in rfm_species:

        # Write RFM driver and run
        writeRFMdrv(start_wave, end_wave, nperwave, layer=True, pathlength=0.1, gas=species, conc={species: conc},
                    temp=temp, pres=pres, out="OPT")
        run('./RFM/source/rfm', stdout=DEVNULL)

        # Read Optical Depth
        Bext = readRFM('./RFM/output/opt00100.out')['data'] / 100

    else:
        raise ValueError('Invalid gas species. Please supply a species name from the following: %s' % possible_species)

    # Make output structure
    out = {}
    out['species'] = species
    out['conc'] = conc
    out['temp'] = temp
    out['pres'] = pres
    out['wave'] = wave
    out['Bext'] = Bext

    return out


# ======================================================================================================================
#                                   Make custom cross-sections for odd gases
# ======================================================================================================================
def makeAerosolXSC(start_wave, end_wave, n_per_wave, size, mass_conc, psd=False, sigma=None, density=None,
                    fname=None, species=None, sio2=None, acid=None, temp=298, save=True):
    """
    This function calculates the Mie scattering properties for aerosols, using complex refractive indices from the ARIA
    database. The function uses the PyMieScatt.AutoMieQ() function to calculate the Mie efficiencies, then derive
    cross-sections and coefficients from there. Size can be supplied as a single number, or as a dictionary containing
    the size distribution (bins and size fraction). The function also writes a XSC file for use within RFM, with the
    extinction coefficient (Bext) in units of [km^-1]. Treat N as a reference concentration.

    Size must be given in microns, and particle concentrations (N) in number per cm^3. PyMieScatt requires input in nm
    and the conversion is done within this function.

    Syntax: out = makeXSC(fname: String, end_wave, n_per_wave, size, N, species='aerosol')

    :param fname: Pathname to the .ri file containing the refractive indices
    :param start_wave: Lower wavenumber
    :param end_wave:
    :param n_per_wave: Desired resolution of the output spectra. Calculations are performed with a resolution of 1 cm,
                        and the output is extrapolated for higher resolutions.
    :param size: The size over which to calculate the Mie efficiencies. This can be one of two options
            1.  Scalar  -   A single particle size diameter             [um]
            2.  PSD     -   A particle size distribution. Expects an ordered dictionary from function makePSD().
                                Will be included in the output dictionary
                                as part of the output structure
                Input must follow a strict formatting with the following
                    ['header]       Metadata information about the PSD, will be included in the XSC file for RFM
                    ['bins']        Center values of the sizes to be considered                                     [um]
                    ['fract']       Relative fraction of the particles within each bin. Sum must be 1
                    ['mean']        The geometric mean of the distribution                                          [um]
                    ['sigma']       The geometric standard deviation of the distribution                  [dimesionless]
    :param N: Particle concentration. If size is a PSD, this reflects the total N, spread across the size range  [cm^-3]
    :param sio2: Composition information for the ash.                                                            [wt. %]
            If given, fname will be ignored and the refractive indices will be computed using the parametrisation
                described in Prata et al. 2018. Must be a scalar between 0-100. Works only between 500 and 2500 cm^-1
    :param species: The label for the aerosol type. Currently supported types are: ['ash', 'h2so4', 'water']
                        Files that don't match will be labeled with generic 'aerosol'

    :return: A dictionary with the following structure:
                    ['header']      Header information, as written to file in RFM .xsc file
                    ['PSD']         The supplied PSD structure                              only if size is a PSD
                    ['N']           Particle number concentration                                           [cm^-3]
                    ['wave']        Wavenumber axis                                                         [cm^-1]
                    ['n']           Real part of refractive index
                    ['k']           Imaginary part of refractive index
                    ['m']           Complex expression of refractive index
                    ['ri']          Complex refractive index
                    ----- Mie Efficiencies -----
                    ['Qext]         Extinction Efficiency                                                   []
                    ['Qsca]         Scattering Efficiency                                                   []
                    ['Qabs]         Absorption Efficiency                                                   []
                    ----- Mie Cross-sections -----
                    ['Cext]         Extinction Cross-section                                                [m^2]
                    ['Csca]         Scattering Cross-section                                                [m^2]
                    ['Cabs]         Absorption Cross-section                                                [m^2]
                    ----- Mie Coefficients -----
                    ['Bext]         Extinction Coefficient                                                  [m^-1]
                    ['Bsca]         Scattering Coefficient                                                  [m^-1]
                    ['Babs]         Absorption Coefficient                                                  [m^-1]

    """
    # ---------------------------------------------------------------------
    # Make sure input makes sense
    # ---------------------------------------------------------------------
    possible_species = ['h2so4', 'ash', 'water']

    # If neither name nor species is given
    if species is None and fname is None:
        raise ValueError('Please supply either a filename or a species name from the following: %s' % possible_species)

    # If species given without a filename, use default
    if species is not None and fname is None:
        if species.lower() == 'h2so4':
            if acid is None:
                acid = 72
            else:
                fnames = glob.glob('./xsec/H2SO4*%iK*Myhre*.ri' % temp)
                acids = [float(f.split('_')[1]) for f in fnames]

                def find_nearest(array, value):
                    array = np.asarray(array)
                    idx = (np.abs(array - value)).argmin()
                    return array[idx]
                acid = find_nearest(acids, acid)

            fname = './xsec/H2SO4_%i_%iK_Myhre_2003.ri' % (acid, temp)
        elif species.lower() == 'ash': fname = './xsec/ASH_etna_Deguine_2020.ri'
        elif species.lower() == 'water': fname = './xsec/H2O_Downing_1975.ri'

    # If filename given without a species, determine which it is
    if species is None and fname is not None:
        if 'ASH' in fname: species = 'ash'
        elif 'H2SO4' in fname: species = 'h2so4'
        elif 'WATER' in fname: species = 'water'
        else: species = 'aerosol'

    # Lower case version of the species for the rest of the script
    species = species.lower()

    if species == 'h2so4':
        acid = float(fname.split('_')[1])
        if 'Myhre' in fname:
            temp = float(fname.split('_')[2].strip('K'))
        else:
            temp = 298

    # ---------------------------------------------------------------------
    # Check type of size information given
    # ---------------------------------------------------------------------
    # If size is just a single diameter
    if not psd:
        size_flag = 0
        size_nm = size * 1e3           # Convert size to nm
        size_SI = size * 1e-6       # For SI units operations
        area_SI = np.pi * (size_SI / 2) ** 2

    # If not then it should be a dictionary with a PSD
    else:
        size_flag = 1
        if sigma is None:
            sigma = 1.5
        size_psd = makePSD(size, sigma)
        bins_nm = size_psd['bins'] * 1e3  # Convert size to nm
        bins_SI = size_psd['bins'] * 1e-6  # For SI units operations
        area_SI = np.pi * (bins_SI / 2) ** 2
        frac = size_psd['frac']

    # ---------------------------------------------------------------------
    # Get the density information depending on species
    # ---------------------------------------------------------------------
    if density is None:
        if species == 'water':
            density = 1000
        elif species == 'h2so4':
            acid_frac = int(fname.split('/')[-1].split('_')[1]) / 100
            density = 1830 * acid_frac
        elif species == 'ash':
            density = 2600

    # Convert from mass concentration [g/m3] to number concentration [m^-3]
    if size_flag == 0:
        particle_mass = 4 / 3 * np.pi * (size_SI / 2) ** 3 * density
        N_SI = mass_conc * 1e-3 / particle_mass
    elif size_flag == 1:
        particle_mass = 4 / 3 * np.pi * (bins_SI / 2) ** 3 * density
        mass_frac = particle_mass * frac
        mass_frac = mass_frac / mass_frac.sum()
        N_SI = np.sum(mass_conc * 1e-3 * mass_frac / particle_mass)

    # ---------------------------------------------------------------------
    # 1: Using SiO2 wt.% to derive refractive indices [Prata et al. 2019]
    # ---------------------------------------------------------------------
    if sio2 is not None:
        # Make sure we are dealing with ash
        if species != 'ash':
            raise ValueError('Only provide sio2 parameter with species type ASH')

        # Make sure the value is within reasonable range
        if not np.isscalar(sio2) or not 30 < sio2 < 100:
            raise ValueError('Please enter sio2 wt %% as a scalar 30-100')

        # Load Parametrisation
        with open('./xsec/Prata_coeff.pkl', 'rb') as f:
            prata = pickle.load(f)

        # Compute n & k
        wave = np.asarray(prata['wn'])
        n = np.asarray(prata['a_i2'] + prata['b_i2'] * sio2)
        k = np.asarray(prata['c_i2'] + prata['d_i2'] * sio2)
        descrip = '# DESCRIPTION: Refractive indices computed from composition using parametrisation' \
                  ' in Prata et al. 2018: SiO2= %i wt %%' % sio2


    # ---------------------------------------------------------------------
    # 2: Read in refractive indices from file
    # ---------------------------------------------------------------------
    else:

        # Open file
        file = open(fname, 'r')
        lines = file.readlines()
        file.close()

        # Extract descriptive information
        descrip = [lines[i].strip('\n') for i in range(len(lines)) if "#DESCRIPTION" in lines[i]][0]

        # Find the start of the data
        start = [i for i in range(len(lines)) if "#FORMAT" in lines[i]][0] + 1

        # Extract X-axis
        wave = np.array([float(x.split()[0]) for x in lines[start:-1]])

        # Extract complex refractive index
        n = np.array([float(x.split()[1]) for x in lines[start:-1]])
        k = np.array([float(x.split()[2]) for x in lines[start:-1]])

    # Flip arrays if they are in order of decreasing wavenumbers
    if wave[0] > wave[-1]:
        wave = np.flip(wave)
        n = np.flip(n)
        k = np.flip(k)

    # Resample along regular x-axis with 1 cm^1 resolution
    npts = int(end_wave - start_wave) + 1
    wave_new = np.linspace(start_wave, end_wave, npts)
    n = np.interp(wave_new, wave, n)
    k = np.interp(wave_new, wave, k)

    # Turn into appropriate units for PyMieScatt
    lambda_nm = 1e7 / wave_new
    m = np.array([np.complex(n[i], k[i]) for i in range(len(wave_new))])

    # ---------------------------------------------------------------------
    # Check if this particular cross-section has been run before
    # ---------------------------------------------------------------------
    # Make header
    header = {}
    header['title'] = '! Extinction cross-section generated with data from ARIA repository'
    header['species'] = species.upper()
    if sio2 is not None:
        header['sio2'] = sio2
    elif acid is not None:
        header['acid'] = acid
    header['fname'] = fname
    header['start_wave'] = start_wave
    header['end_wave'] = end_wave
    header['n_per_wave'] = n_per_wave
    if size_flag == 0:
        header['size'] = size
        header['mass_conc'] = mass_conc
    elif size_flag == 1:
        header['size'] = size
        header['sigma'] = sigma
        header['mass_conc'] = mass_conc
    if sio2 is not None:
        header['sio2'] = sio2
    else:
        header['fname'] = fname

    # List all existing file and compare headers
    xsec_list = glob.glob('./xsec/customXSC*%s*.pkl' % species.upper())
    xsec_exists = []
    for file in xsec_list:
        with open(file, 'rb') as f:
            xsec = pickle.load(f)
            xsec_exists.append(xsec['header'] == header)

    # If it has already been run then just get that one
    if any(xsec_exists):
        xsec_list = [xsec_list[i] for i, val in enumerate(xsec_exists) if val]

        # Only one match
        if len(xsec_list) == 1:
            with open(xsec_list[0], 'rb') as f:
                out = pickle.load(f)

        # Multiple matches, keep the most recent one
        elif len(xsec_list) > 1:
            xsec_list.sort()
            with open(xsec_list[-1], 'rb') as f:
                out = pickle.load(f)


    # If not then proceed with the calculations
    # ---------------------------------------------------------------------
    # Calculate Efficiencies, Cross-sections and Coefficients
    # ---------------------------------------------------------------------
    else:
        # ------ For single particle size ------
        if size_flag == 0:
            Qext = []
            Qsca = []
            Qabs = []

            # Loop over range of wavelengths and get Q
            pbar = tqdm(total=npts)
            pbar.set_description('%s: %.2f um' % (species, size))
            if sio2 is not None:
                pbar.set_postfix_str('%i wt.%% SiO2' % sio2)
            else:
                pbar.set_postfix_str('from file: %s' % fname)

            for i in range(npts):
                run = PyMieScatt.AutoMieQ(m[i], lambda_nm[i], size_nm, asDict=True)
                Qext.append(run['Qext'])
                Qsca.append(run['Qsca'])
                Qabs.append(run['Qabs'])
                pbar.update()
            pbar.close()

            # Convert size to [m] and calculate cross-section (C)
            Cext = np.array(Qext) * area_SI
            Csca = np.array(Qsca) * area_SI
            Cabs = np.array(Qabs) * area_SI

            # Convert N concentration to [m^-3] and calculate coefficients (B)
            Bext = Cext * N_SI
            Bsca = Csca * N_SI
            Babs = Cabs * N_SI

        # ------ For size distribution ------
        if size_flag == 1:
            Qext = []
            Qsca = []
            Qabs = []

            Cext = []
            Csca = []
            Cabs = []

            Bext = []
            Bsca = []
            Babs = []

            # Loop over range of wavelengths
            pbar = tqdm(total=npts * len(bins_nm))
            pbar.set_description('%s: %.2f um' % (species, size))
            if sio2 is not None:
                pbar.set_postfix_str('%i wt.%% SiO2' % sio2)
            else:
                pbar.set_postfix_str('from file: %s' % fname)
            for i in range(npts):
                Qext_x = []
                Qsca_x = []
                Qabs_x = []

                # Loop over range of sizes and get Q
                for x in bins_nm:
                    run = PyMieScatt.AutoMieQ(m[i], lambda_nm[i], x, asDict=True)
                    Qext_x.append(run['Qext'])
                    Qsca_x.append(run['Qsca'])
                    Qabs_x.append(run['Qabs'])
                    pbar.update()

                # Convert size to [m] and calculate cross-section (C)
                Cext_x = np.array(Qext_x) * area_SI
                Csca_x = np.array(Qsca_x) * area_SI
                Cabs_x = np.array(Qabs_x) * area_SI

                # Convert N concentration to [m^-3] and calculate coefficients (B)
                Bext_x = Cext_x * N_SI
                Bsca_x = Csca_x * N_SI
                Babs_x = Cabs_x * N_SI

                # Integrate efficiencies (Q) over the size fraction
                Qext.append(np.sum(Qext_x * frac))
                Qsca.append(np.sum(Qsca_x * frac))
                Qabs.append(np.sum(Qabs_x * frac))

                # Integrate cross sections (C) over the size fraction
                Cext.append(np.sum(Cext_x * frac))
                Csca.append(np.sum(Csca_x * frac))
                Cabs.append(np.sum(Cabs_x * frac))

                # Integrate coefficients (B) over the size fraction
                Bext.append(np.sum(Bext_x * frac))
                Bsca.append(np.sum(Bsca_x * frac))
                Babs.append(np.sum(Babs_x * frac))

        pbar.close()

        # Resample everything using user supplied resolution
        npts = int(n_per_wave * (end_wave - start_wave)) + 1
        wave_final = np.linspace(start_wave, end_wave, npts)
        n_final = np.interp(wave_final, wave_new, n)  # Refractive indices
        k_final = np.interp(wave_final, wave_new, k)
        m_final = np.array([np.complex(n_final[i], k_final[i]) for i in range(len(wave_final))])
        Qext = np.interp(wave_final, wave_new, Qext)  # Efficiencies (Q)
        Qsca = np.interp(wave_final, wave_new, Qsca)
        Qabs = np.interp(wave_final, wave_new, Qabs)
        Cext = np.interp(wave_final, wave_new, Cext)  # Cross-sections (C)
        Csca = np.interp(wave_final, wave_new, Csca)
        Cabs = np.interp(wave_final, wave_new, Cabs)
        Bext = np.interp(wave_final, wave_new, Bext)  # Coefficients (B)
        Bsca = np.interp(wave_final, wave_new, Bsca)
        Babs = np.interp(wave_final, wave_new, Babs)

        # ---------------------------------------------------------------------
        # Create output structure and save file for later use
        # ---------------------------------------------------------------------
        # Populate output structure
        out = {}
        out['header'] = header      # Header
        if size_flag == 1:
            out['PSD'] = size_psd
            out['size_bins'] = size_psd['bins']
            out['N_frac'] = size_psd['frac']
            out['mass_frac'] = mass_frac
        else:
            out['size'] = size
        out['wave'] = wave_final    # Wavenumber axis
        out['n'] = n_final
        out['k'] = k_final
        out['m'] = m_final
        out['mass_conc'] = mass_conc                # Particle concentration
        out['N_SI'] = N_SI
        out['ri'] = m_final         # Complex refractive index
        out['Qext'] = Qext          # Efficiencies (Q)
        out['Qsca'] = Qsca
        out['Qabs'] = Qabs
        out['Cext'] = Cext          # Cross-sections (C)
        out['Csca'] = Csca
        out['Cabs'] = Cabs
        out['Bext'] = Bext          # Coefficients (B)
        out['Bsca'] = Bsca
        out['Babs'] = Babs

        if save:
            # Save pickled file
            # if sigma is not None:
            #             #     size_info = '%.2fum_%.2fsigma' % (size, sigma)
            #             # else:
            #             #     size_info = '%.2fum' % size
            #             # if species == 'ash':
            #             #     if sio2 is not None:
            #             #         outname = './xsec/customXSC_%s_%isio2_%s_%i-%icm_%iK.pkl' % (species.upper(), sio2, size_info, start_wave, end_wave, temp)
            #             #     else:
            #             #         outname = './xsec/customXSC_%s_fromfile_%s_%i-%icm_%iK.pkl' % (species.upper(), size_info, start_wave, end_wave, temp)
            #             # elif species == 'h2so4':
            #             #     outname = './xsec/customXSC_%s_%iacid_%s_%i-%icm_%iK.pkl' % (species.upper(), acid, size_info, start_wave, end_wave, temp)
            #             # else:
            #             #     outname = './xsec/customXSC_%s_%s_%i-%icm_%iK.pkl' % (species.upper(), size_info, start_wave, end_wave, temp)
            outname = './xsec/customXSC_%s_%s.pkl' % (species.upper(), datetime.now().strftime("%y%m%d_%H%M%S"))
            with open(outname, 'wb') as f:
                pickle.dump(out, f)

    return out