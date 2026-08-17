"""
Written by @jfsmekens - Last updated 01/04/2022

This module contains functions to read and define atmospheric profile, and to run RFM using these profiles:

"""

# ----------------- Import Dependencies ------------------
import numpy as np
import matplotlib.pyplot as plt
import warnings
from unitConversion import *
from datetime import datetime
warnings.filterwarnings("ignore")
# --------------------------------------------------------


def readAtm(fname):
    """
    This function reads in a standard atmospheric profile formatted for RFM input

    SYNTAX: out = readAtm(fname)

    :param fname: Pathname of the file to read

    :return:a dictionary with the following structure (some fields may not be included, others may be added):

                                                        Units

        ['header']	String array with header inoformation
        ['HGT']	    Height	                            [km]
        ['PRES']	Atmospheric Pressure	            [mb]
        ['TEMP']	Temperature	                        [K]
        ['H2O']	    Water vapor mixing ratio	        [ppmv]
        ['CO2']	    Carbon dioxide mixing ratio	        [ppmv]
        ['O3']	    Ozone mixing ratio	                [ppmv]
        ['N2O']	    Nitrous oxide mixing ratio	        [ppmv]
        ['CO']	    Carbon monoxide mixing ratio	    [ppmv]
        ['CH4']	    Methane mixing ratio	            [ppmv]
        ['O2']	    Oxygen mixing ratio	                [ppmv]
        ['SO2']	    Sulphur dioxide mixing ratio	    [ppmv]

    """

    # Read in atmospheric profile
    with open(fname, 'r') as file:
        lines = file.readlines()

    # Extract sections and where they are
    sections = [lines[i].split()[0].replace('*', '').upper() for i in range(len(lines)) if "*" in lines[i]]
    indices = [i for i in range(len(lines)) if "*" in lines[i]]

    # Create output structure
    out = {}
    out['header'] = [x.strip('\n') for x in lines[0:indices[0]-1]]

    # Loop through sections and read in data
    for idx, field in enumerate(sections[:-1]):
        start = indices[idx] + 1
        end = indices[idx+1]
        var = np.empty(0)
        for i in range(start, end):
            line = np.asarray([float(x) for x in lines[i].strip().replace(',', '').split()])
            var = np.append(var, line)
        out[field] = var

    return out

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

def writeAtm(atm, outname, header=""):
    """
    This function writes a standard atmospheric profile formatted for RFM input

    SYNTAX: writeAtm(atm, outname, header="")

    :param atm: A dictionary containing the atmospheric profile
    :param outname: Pathname to the text file to be created
    :param header: Optional String with custom header. If present, it will override any existing header section

    :return: -none-
    """
    # Open file
    file = open(outname, 'w')

    # Write header
    if header == "":
        if "header" in atm.keys():
            for x in atm['header']: file.write("%s\n" % x)
        else:
            header = '! RFM atmospheric profile'
            file.write('%s\n' % header)
            atm['header'] = header
    else:
        if isinstance(header, list):
            for x in header: file.write("%s\n" % x)
        else:
            file.write('%s\n' % header)
            atm['header'] = header

    # Write number of levels i profile
    file.write("%s" % len(atm['HGT']))
    file.write("  ! No.Levels in profiles\n\n")

    # Write each field
    for key in [x for x in atm.keys() if x != "header"]:
        file.write("*%s\n" % key)
        line = ['%.5f' % x for x in atm[key]]
        file.write(', '.join(line))
        file.write("\n\n")

    # End of file *END
    file.write("*END")
    file.close()

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

def resampleAtm(fname, outpath=False):
    """
    """
    
    # Read file
    atm = readAtm(fname)
    
    # Define 25 levels vertical grid
    hgt_scale = [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5,
                 5.0, 7.5, 10.0, 12.5, 15.0, 17.5, 20.0,
                 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 75.0, 100.0]

    hgt_scale = [h for h in hgt_scale if atm['HGT'].min() <= h <= atm['HGT'].max()]

    out = {}
    out['HGT'] = hgt_scale

    # Write header
    if "header" in atm.keys():
        header = atm['header']
        header.append('! Resampled onto plumeIR_dev vertical grid')
    else:
        header = '! RFM atmospheric profile'
        header.append('! Resampled onto plumeIR_dev vertical grid')
    out['header'] = header

    # ---------------  1: SCALE ATM PROFILES  --------------------
    plist = [p for p in atm.keys() if (p != 'HGT' and p != 'header')]
    for p in plist:
        out[p] = np.interp(hgt_scale, atm['HGT'], atm[p])

    outname = fname[:-4] + '_vert.atm'
    writeAtm(out, outname)

    if outpath:
        return out, outname
    else:
        return out

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

# ---- Prerequesite function to make gaussian profile ---
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

def insertPlume(fname, h, th, elev=90.0, scd=0.0, h2s = 0.0, h2o=0.0, co2=0.0, sif4=0.0, h2o_sc=1.0, TD=0.0,
                gradient_flag=0, th_v=True, outpath=False, zero=False):
    """
    This function modifies a standard atmosphere to insert a Plume layer with given SO2 and H2O amounts, and raising the
    plume temperature. The user needs to define the height and thickness of the plume layer. SO2 and H2O are given as
    slant column densities (SCD) and distributed across the plume layer based on a concentration gradient. If no viewing
    angle is given, vertical observation is assumed. Thickness is assumed to be the vertical thickness. The th_v flag
    can be used to define a slant thickness directly, in which case the vertical thickness is recalculated to match.
    The function writes a series of modified atmospheric profiles to be passed in the *ATM section of RFM, and returns
    a dictionary with the modified atmosphere.

    SYNTAX: out = insertPlume(fname, h, th [, elev=90.0, scd=0.0, h2o_sc=0.0, TD=0.0, gradient_flag=0, th_v=True,
                                                outpath=False])

    :param fname: Pathname to the atmosphere to be modified
    :param h: Height (center) of the plume                                                          [km]
    :param th: Thickness of the plume.
            This is the vertical thickness assuming an infinite plane parallel plume. Slant thickness is calculated
                using the viewing_angle below.

    :param elev: viewing angle for the slant observation. default is 90 (vertical view)             [degrees]
    :param scd: Slant column density of SO2 to be added within the plume                            [ppm.m]
            Actual distribution is defined by gradient_flag
            Default is 0.0
    :param sif4: Slant column density of SiF4 to be added within the plume                          [ppm.m]
            Actual distribution is defined by gradient_flag
            Default is 0.0
    :param h2o_sc: Slant column density of H2O                                                     [ppm.m]
            Actual distribution is defined by gradient_flag
            Default is 0.0
    :param TD: Temperature increase within the plume                                                [K]
            Actual distribution is defined by gradient_flag
            Default is 0.0
    :param gradient_flag: Defines the distribution of quantities (so2, temp and RH) within the plume
            # 0 = Uniform (step function)
            # 1 = Triangular
            # 2 = Gaussian
            Default is 0
    :param th_v: Forces the thickness (th) to be treated as slant thickness if set to False.
            Default is True
    :param outpath: If True, also returns the pathname to the new atmosphere.
            Default is False

    :return: Dictionary containing the modified atmospheric profiles (see readAtm for details)
            The script also creates a new text file with the modified atmosphere in the local directory
    """

    # Define plume limits
    h1 = h - th/2
    h2 = h + th/2
    if th_v:
        th_slant = th / np.sin(np.pi * elev / 180)
    else:
        th_slant = th
        th = th_slant * np.sin(np.pi * elev / 180)

    # Read in standard profile
    atm = readAtm(fname)
    hgt_old = atm['HGT']
    pres_old = atm['PRE']
    temp_old = atm['TEM']
    h2o_old = atm['H2O']
    co2_old = atm['CO2']
    o3_old = atm['O3']
    n2o_old = atm['N2O']
    co_old = atm['CO']
    ch4_old = atm['CH4']
    o2_old = atm['O2']
    so2_old = atm['SO2']
    sif4_old = np.zeros(len(atm['HGT']))
    h2s_old = np.zeros(len(atm['HGT']))

    # Make plume height axis
    # Flag = 0: Uniform layer
    if gradient_flag == 0:
        xx = [h1 - 0.001, h1, h2, h2 + 0.001]  # 4 position plume coordinates
        xx = np.asarray(xx)
    # Flag = 1: Triangular profile
    if gradient_flag == 1:
        xx = [h1, h, h2]  # 3 position plume coordinates
        xx = np.asarray(xx)
    # Flag = 2: Gaussian profile
    if gradient_flag == 2:
        step = th / 9  # Divide plume in 10 layers
        xx = np.arange(h1, h2 + step, step)


    # --------------  1: EXTRACT ATM PROFILES  --------------------
    # Get indices for slices of the atmosphere
    below = [index for index, value in enumerate(hgt_old) if value < h1]
    above = [index for index, value in enumerate(hgt_old) if value > h2]

    # Get temperature profile within plume
    t_plume = np.interp(xx, hgt_old, temp_old)

    # Zero out profiles
    if zero:
        if scd != 0: so2_old[:] = 0.0
        if co2 != 0: co2_old[:] = 0.0
        if h2o != 0: h2o_old[:] = 0.0

    # Get H2O profile within plume
    h2o_plume = np.interp(xx, hgt_old, h2o_old)
    h2o_sc = h2o_sc - 1

    # Get profiles within plume
    so2_plume = np.interp(xx, hgt_old, so2_old)
    co2_plume = np.interp(xx, hgt_old, co2_old)


    # ----------- 2: CREATE PLUME BASED ON FLAG  ------------
    # Flag = 0: Uniform layer
    if gradient_flag == 0:
        h2o = h2o / (th_slant * 1000)
        so2 = scd / (th_slant * 1000)
        h2s = h2s / (th_slant * 1000)
        co2 = co2 / (th_slant * 1000)
        sif4 = sif4 / (th_slant * 1000)
        yy_so2 = so2_plume + np.array([0, 1, 1, 0]) * so2              # Raise SO2 conc within plume by a flat value
        yy_co2 = co2_plume + np.array([0, 1, 1, 0]) * co2
        yy_sif4 = np.array([0, 1, 1, 0]) * sif4
        yy_h2s = np.array([0, 1, 1, 0]) * h2s
        yy_temp = t_plume + np.array([0, 1, 1, 0]) * TD
        yy_h2o = h2o_plume * (1 + np.array([0, 1, 1, 0]) * h2o_sc)      # H2O scaling
        yy_h2o = yy_h2o + np.array([0, 1, 1, 0]) * h2o
    # Flag = 1: Triangular profile
    if gradient_flag == 1:
        h2o = 2 * h2o / (th_slant * 1000)
        so2 = 2 * scd / (th_slant * 1000)
        h2s = 2 * h2s / (th_slant * 1000)
        co2 = 2 * co2 / (th_slant * 1000)
        sif4 = 2 * sif4 / (th_slant * 1000)
        yy_so2 = so2_plume + np.array([0, 1, 0]) * so2                  # Linear tapering towards regular values
        yy_co2 = co2_plume + np.array([0, 1, 0]) * co2
        yy_h2s = np.array([0, 1, 0]) * h2s
        yy_sif4 = np.array([0, 1, 0]) * sif4
        yy_temp = t_plume + np.array([0, 1, 0]) * TD                    # Linear tapering towards regular values
        yy_h2o = h2o_plume * (1 + np.array([0, 1, 0]) * h2o_sc)      # Linear tapering towards regular values
        yy_h2o = yy_h2o + np.array([0, 1, 0]) * h2o
    # Flag = 2: Gaussian profile
    if gradient_flag == 2:
        step = th_slant / 9  # Divide plume in 10 layers
        sig = (h - h1) / 2.5  # Not quite Eulerian
        h2o = h2o / np.trapz(gauss(xx, h, sig), dx=(step * 1000))
        so2 = scd / np.trapz(gauss(xx, h, sig), dx=(step * 1000))
        co2 = co2 / np.trapz(gauss(xx, h, sig), dx=(step * 1000))
        h2s = h2s / np.trapz(gauss(xx, h, sig), dx=(step * 1000))
        sif4 = sif4 / np.trapz(gauss(xx, h, sig), dx=(step * 1000))
        yy_so2 = so2_plume + gauss(xx, h, sig) * so2                          # Scale normal distribution to appropriate concentration
        yy_co2 = co2_plume + gauss(xx, h, sig) * co2
        yy_h2s = gauss(xx, h, sig) * h2s
        yy_sif4 = gauss(xx, h, sig) * sif4  # Scale normal distribution to appropriate concentration
        yy_temp = t_plume + gauss(xx, h, sig) * TD                # Scale normal distribution to appropriate concentration
        yy_h2o = h2o_plume * (1 + gauss(xx, h, sig) * h2o_sc)     # Scale normal distribution to appropriate concentration
        yy_h2o = yy_h2o + gauss(xx, h, sig) * h2o


    # ----------- 3: FILL IN PLUME IN NEW PROFILES  ------------
    # Height profile
    first = hgt_old[below]  # Get values below that altitude from original profile
    rest = hgt_old[above]  # Get values above that altitude from original profile
    hgt_new = np.append(first, xx)
    hgt_new = np.append(hgt_new, rest)  # Fill in the plume

    # Temperature profile
    first = temp_old[below]  # Get values below that altitude from original profile
    rest = temp_old[above]  # Get values above that altitude from original profile
    temp_new = np.append(first, yy_temp)
    temp_new = np.append(temp_new, rest)
    if TD == 0.0: temp_new = np.interp(hgt_new, hgt_old, temp_old)

    # H2O profile
    first = h2o_old[below]  # Get values below that altitude from original profile
    rest = h2o_old[above]  # Get values above that altitude from original profile
    h2o_new = np.append(first, yy_h2o)
    h2o_new = np.append(h2o_new, rest)  # Fill in the plume
    if h2o_sc == 1.0: h2o_new = np.interp(hgt_new, hgt_old, h2o_old)

    # SO2 profile
    first = so2_old[below]  # Get values below that altitude from original profile
    rest = so2_old[above]  # Get values above that altitude from original profile
    so2_new = np.append(first, yy_so2)
    so2_new = np.append(so2_new, rest)  # Fill in the plume
    if scd == 0.0: so2_new = np.interp(hgt_new, hgt_old, so2_old)

    # CO2 profile
    first = co2_old[below]  # Get values below that altitude from original profile
    rest = co2_old[above]  # Get values above that altitude from original profile
    co2_new = np.append(first, yy_co2)
    co2_new = np.append(co2_new, rest)  # Fill in the plume
    if co2 == 0.0: co2_new = np.interp(hgt_new, hgt_old, co2_old)

    # H2S profile
    first = h2s_old[below]  # Get values below that altitude from original profile
    rest = h2s_old[above]  # Get values above that altitude from original profile
    h2s_new = np.append(first, yy_h2s)
    h2s_new = np.append(h2s_new, rest)  # Fill in the plume
    if h2s == 0.0: h2s_new = np.interp(hgt_new, hgt_old, h2s_old)

    # SiF4 profile
    first = sif4_old[below]  # Get values below that altitude from original profile
    rest = sif4_old[above]  # Get values above that altitude from original profile
    sif4_new = np.append(first, yy_sif4)
    sif4_new = np.append(sif4_new, rest)  # Fill in the plume
    if sif4 == 0.0: sif4_new = np.interp(hgt_new, hgt_old, sif4_old)


    # Resample other atmospheric profiles
    pres_new = np.interp(hgt_new, hgt_old, pres_old)  # Pressure
    # co2_new = np.interp(hgt_new, hgt_old, co2_old)  # CO2 profile
    o3_new = np.interp(hgt_new, hgt_old, o3_old)  # O3 profile
    n2o_new = np.interp(hgt_new, hgt_old, n2o_old)  # N2O profile
    co_new = np.interp(hgt_new, hgt_old, co_old)  # CO profile
    ch4_new = np.interp(hgt_new, hgt_old, ch4_old)  # CH4 profile
    o2_new = np.interp(hgt_new, hgt_old, o2_old)  # O2 profile


    # -------------  3: MAKE OUTPUT STRUCTURE  ------------------
    out = {}

    # Write header
    header = []
    header.append("! Standard profile modified with SO2 layer for input to the RFM forward model")
    header.append("! SO2 layer:")
    header.append("! %.2f - %.2f km" % (h1, h2))
    header.append("! Max SO2 = %.3f ppmv" % so2)
    header.append("! Temp = +%.1f K" % TD)
    header.append("! H2O scaling factor = +%.3f" % h2o_sc)
    header.append("! Original file: %s" % fname)
    out['header'] = header

    out['HGT'] = hgt_new
    out['PRE'] = pres_new
    out['TEM'] = temp_new
    out['H2O'] = h2o_new
    out['CO2'] = co2_new
    out['O3'] = o3_new
    out['N2O'] = n2o_new
    out['CO'] = co_new
    out['CH4'] = ch4_new
    out['O2'] = o2_new
    out['SO2'] = so2_new
    if any(sif4_new != 0.0): out['SiF4'] = sif4_new
    if any(h2s_new != 0.0): out['H2S'] = h2s_new


    # --------  4: WRITE MODIFIED ATMOSPHERIC PROFILE  ----------
    outname = fname[:-4] + '_plume.atm'
    writeAtm(out, outname)

    if outpath:
        return out, outname
    else:
        return out

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

def modifyAtm(fname, sounding, outpath=False):
    """
    This function modifies a standard atmosphere profile (like mid latitude summer) with a local sounding.

    SYNTAX: out [, outname] = modifyAtm(fname, sounding [,outpath=False])

    :param fname: Pathname to the standard atmosphere to be modified. Must be a text file formatted for RFM input
    :param sounding: Pathname to the text file with the sounding. Must be from a University of Wyoming sounding
    :param outpath: If True, also returns the pathname to the new atmosphere.
                    Default is False

    :return: Dictionary containing the modified atmospheric profiles
            The script also  creates a new text file with the modified atmosphere in the local directory
    """

    # Read in standard profile
    atm = readAtm(fname)

    # Read in sounding
    sound = readSounding(sounding)


    # --------------  1: MODIFY ATM PROFILES  --------------------
    # Replace height profile
    hgt_old = atm['HGT']
    hgt_new = sound['HGHT']
    bottom = sound['HGHT'].min()                  # What is the minimum altitude for sounding
    below = np.where(hgt_old < bottom)          # Find out where the original profile layer is below that altitude
    first = hgt_old[below]                      # Get values below that altitude from original profile
    top = sound['HGHT'].max()                     # What is the maximum altitude for sounding
    above = np.where(hgt_old > top)             # Find out where the original profile layer is below that altitude
    rest = hgt_old[above]                       # Get values above that altitude from original profile
    hgt_new = np.append(first, hgt_new)
    hgt_new = np.append(hgt_new, rest)          # Fill in the sounding

    # Replace pressure profile
    pres_old = atm['PRE']
    pres_new = sound['PRES']
    first = pres_old[below]                     # Get values below that altitude from original profile
    rest = pres_old[above]                      # Get values above that altitude from original profile
    pres_new = np.append(first, pres_new)
    pres_new = np.append(pres_new, rest)        # Fill in the sounding
    pres_new = np.interp(hgt_old, hgt_new, pres_new)  # Reproject over old height profile

    # Replace temperature profile
    temp_old = atm['TEM']
    temp_new = sound['TEMP']
    first = temp_old[below]                     # Get values below that altitude from original profile
    rest = temp_old[above]                      # Get values above that altitude from original profile
    temp_new = np.append(first, temp_new)
    temp_new = np.append(temp_new, rest)        # Fill in the sounding
    temp_new = np.interp(hgt_old, hgt_new, temp_new)  # Reproject over old height profile

    # Replace humidity profile
    h2o_old = atm['H2O']
    h2o_new = sound['MIXR']
    first = h2o_old[below]                      # Get values below that altitude from original profile
    rest = h2o_old[above]                       # Get values above that altitude from original profile
    h2o_new = np.append(first, h2o_new)
    h2o_new = np.append(h2o_new, rest)          # Fill in the sounding
    h2o_new = np.interp(hgt_old, hgt_new, h2o_new)  # Reproject over old height profile

    # -------------  2: MAKE OUTPUT STRUCTURE  ------------------
    out = {}

    # Write header
    header = []
    header.append("! Standard profile modified with balloon sounding from the University of Wyoming")
    header.append("! Designed for input to the RFM forward model")
    header.append("! Original file: %s" % fname)
    header.append("! Modified with file: %s" % sounding)
    out['header'] = header

    # Populate with modified profiles
    out['HGT'] = hgt_old
    out['PRE'] = pres_new
    out['TEM'] = temp_new
    out['H2O'] = h2o_new

    # Everything else stays the same
    for key in atm.keys():
        if key not in ['header', 'HGT', 'PRE', 'TEM', 'H2O']:
            out[key] = atm[key]

    # --------  3: WRITE MODIFIED ATMOSPHERIC PROFILE  ----------
    outname = fname[:-4] + '_mod.atm'
    writeAtm(out, outname)

    if outpath:
        return out, outname
    else:
        return out


# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------

def RH2ppm(RH, P, T):
    if T > 273.16:  # Saturation over water

        # Constants
        Tc = 647.096  # Critical temperature [in K]
        Pc = 220640  # Critical pressure [in hPa]
        c1 = -7.85951783
        c2 = 1.84408259
        c3 = -11.7866497
        c4 = 22.6807411
        c5 = -15.9618719
        c6 = 1.80122502

        t = 1 - (T / Tc)  # New variable
        polyfit = Tc / T * (c1 * t + c2 * t ** 1.5 + c3 * t ** 3 + c4 * t ** 3.5 + c5 * t ** 4 + c6 * t ** 7.5)
        Psat = np.exp(polyfit) * Pc

    else:  # Saturation over ice

        # Constants
        Pn = 6.11657  # Vapor pressure at triple point temperature [in hPa]
        Tn = 273.16  # Triple point temperature [in K]
        a0 = -13.928169
        a1 = 34.707823

        t = T / Tn  # New variable
        polyfit = a0 * (1 - t ** -1.5) + a1 * (1 - t ** -1.25)
        Psat = np.exp(polyfit) * Pn

    # Now to concentration
    ppm = RH / 100 * Psat / P * 1e6

    return ppm


# --------------------------------------------------------

def modifyAtm2(fname, h_obs=0, T_obs='', RH_obs='', pbl='', c_T=1.0, c_H2O=1.0, c_CO2=1.0,
               c_O3=1.0, c_N2O=1.0, c_CO=1.0, c_CH4=1.0, c_O2=1.0, c_SO2=1.0, outpath=False):
    """
    This function modifies a standard atmosphere profile (like mid latitude summer) with a series of scaling factors for
    use in the iterative retrieval scheme.

    SYNTAX: out = modifyAtm2modifyAtm2(fname [, h_obs='', T_obs='', RH_obs='', c_T=0, c_H2O=1.0, c_c_O2=1.0, pbl=0,
               c_O3=1.0, c_N2O=1.0, c_CO=1.0, c_CH4=1.0, c_O2=1.0, c_SO2=1.0, outpath=False)

    :param fname: Pathname to the standard atmosphere to be modified.
                    Must be a text file formatted for RFM input

    !!!     WARNING: If matching observations is selected, this overrides any scaling coefficients given later      !!!
    !!!                     Do not use if you want to manually scale a profile                                      !!!

    :param h_obs: Altitude of the observer                                                                  [km]
                    If present, will force a match to observed quantities
    :param T_obs: Temperature at the observer altitude.                                                     [K]
                    The temperature profile is scaled to match this temperature
    :param RH_obs: Water vapor mixing ratio at the observer altitude.                                       [ppmv]
                    The water vapor profile is scaled to match this temperature
    :param pbl: Thickness of the planetary boundary layer (PBL)                                             [km]
                    If present, scaling only applies within PBL

    :param c_T: Scaling factor for the temperature (T) profile. Default is 1.0
    :param c_H2O: Scaling factor for the water vapor (H2O) profile. Default is 1.0
    :param c_CO2: Scaling factor for the carbon dioxide (CO2) profile. Default is 1.0
    :param c_O3: Scaling factor for the ozone (O3) profile. Default is 1.0
    :param c_N2O: Scaling factor for the nitrous oxide (N2O) profile. Default is 1.0
    :param c_CO: Scaling factor for the carbon monoxide (CO) profile. Default is 1.0
    :param c_CH4: Scaling factor for the methane (CH4) profile. Default is 1.0
    :param c_O2: Scaling factor for the oxygen (O2) profile. Default is 1.0
    :param c_SO2: Scaling factor for the sulphur dioxide (SO2) profile. Default is 1.0

    :param outpath: If True, also returns the pathname to the new atmosphere. Default is False

    :return: Dictionary containing the modified atmospheric profiles
            The script also  creates a new text file with the modified atmosphere in the local directory
    """
    # Read in standard profile
    atm = readAtm(fname)
    hgt = atm['HGT']  # H profile
    pres = atm['PRE']  # P profile
    temp = atm['TEM']  # T profile
    h2o = atm['H2O']  # H2O profile
    co2 = atm['CO2']  # CO2 profile
    o3 = atm['O3']  # O3 profile
    n2o = atm['N2O']  # N2O profile
    co = atm['CO']  # CO profile
    ch4 = atm['CH4']  # CH4 profile
    o2 = atm['O2']  # O2 profile
    so2 = atm['SO2']  # SO2 profile

    # Check input is correct
    if h_obs != 0:
        if h_obs > hgt.max():
            print('!!! ERROR: Height observer above top of the atmosphere !!! ')
        if h_obs < hgt.min():
            print('!!! ERROR: Height observer below bottom of the atmosphere !!! ')
        if all([T_obs == '', RH_obs == '']):
            print('!!! ERROR: User must supply T and/or RH at observer height !!! ')
        P_obs = np.interp(h_obs, hgt, pres)  # Pressure at observer height
        if T_obs == '':
            T_obs = np.interp(h_obs, hgt, temp)  # Temperature at observer height
        if RH_obs != '':
            VMR_obs_old = np.interp(h_obs, hgt, h2o)  # H2O VMR at observer height
            VMR_obs = RH2ppm(RH_obs, P_obs, T_obs)  # H2O VMR from RH supplied by user
            c_H2O = VMR_obs / VMR_obs_old  # Make them match

    if pbl != '':
        if pbl < 1.0:
            pbl = 1.0
        # print(' Limiting scaling modifications to PBL < %.1f km' % pbl)
        idx = np.where(hgt <= pbl)

    # ---------------  1: SCALE ATM PROFILES  --------------------
    # Replace temperature profile
    if pbl == '':
        temp = temp * c_T
    else:
        c_T = np.linspace(c_T, 1.0, len(idx))
        temp[idx] = temp[idx] * c_T

    # Replace water vapor profile
    if pbl == '':
        h2o = h2o * c_H2O
    else:
        c_H2O = np.linspace(c_H2O, 1.0, len(idx))
        h2o[idx] = h2o[idx] * c_H2O

    # Now everything else
    if pbl == '':
        co2 = co2 * c_CO2  # Scale CO2 profile
        o3 = o3 * c_O3  # Scale O3 profile
        n2o = n2o * c_N2O  # Scale N2O profile
        co = co * c_CO  # Scale CO profile
        ch4 = ch4 * c_CH4  # Scale CH4 profile
        o2 = o2 * c_O2  # Scale O2 profile
        so2 = so2 * c_SO2  # Scale SO2 profile
    else:
        # Smooth scaling factors over PBL height
        c_CO2 = np.linspace(c_CO2, 1.0, len(idx))
        c_O3 = np.linspace(c_O3, 1.0, len(idx))
        c_N2O = np.linspace(c_N2O, 1.0, len(idx))
        c_CO = np.linspace(c_CO, 1.0, len(idx))
        c_CH4 = np.linspace(c_CH4, 1.0, len(idx))
        c_O2 = np.linspace(c_O2, 1.0, len(idx))
        c_SO2 = np.linspace(c_SO2, 1.0, len(idx))
        # Scale PBL only
        co2[idx] = co2[idx] * c_CO2  # Scale CO2 profile
        o3[idx] = o3[idx] * c_O3  # Scale O3 profile
        n2o[idx] = n2o[idx] * c_N2O  # Scale N2O profile
        co[idx] = co[idx] * c_CO  # Scale CO profile
        ch4[idx] = ch4[idx] * c_CH4  # Scale CH4 profile
        o2[idx] = o2[idx] * c_O2  # Scale O2 profile
        so2[idx] = so2[idx] * c_SO2  # Scale SO2 profile

    # ---------------  2: MAKE OUTPUT STRUCTURE  ----------------
    out = {}

    # Write header
    header = []
    header.append("! Modified atmospheric profile for RFM input")
    header.append("! Temperature scaled to match recorded observer")
    header.append("! Relative Humidity scaled to match recorded observer")
    header.append("! Scaling factors for other atmospheric species :")
    header.append("! Temp: %.2f" % c_T)
    header.append("! H2O: %.2f" % c_H2O)
    header.append("! CO2: %.2f" % c_CO2)
    header.append("! O3: %.2f" % c_O3)
    header.append("! N2O: %.2f" % c_N2O)
    header.append("! CO: %.2f" % c_CO)
    header.append("! CH4: %.2f" % c_CH4)
    header.append("! O2: %.2f" % c_O2)
    header.append("! SO2: %.2f" % c_SO2)
    header.append("! Original file: %s" % fname)

    out['header'] = header
    out['HGT'] = hgt
    out['PRE'] = pres
    out['TEM'] = temp
    out['H2O'] = h2o
    out['CO2'] = co2
    out['O3'] = o3
    out['N2O'] = n2o
    out['CO'] = co
    out['CH4'] = ch4
    out['O2'] = o2
    out['SO2'] = so2

    # --------  3: WRITE MODIFIED ATMOSPHERIC PROFILE  ----------
    outname = fname[:-4] + '_scaled.atm'
    writeAtm(out, outname)

    if outpath:
        return out, outname
    else:
        return out

# --------------------------------------------------------

def scaleAtm(fname, scaling, which=None, upto=None, outpath=False):
    """

    :param fname:
    :param scaling:
    :param which:
    :param upto:
    :param outpath:
    :return:
    """
    # Read in original profile
    atm = readAtm(fname)

    # Minimum height should be 1 km
    if upto is not None:
        if upto < 1.0:
            upto = 1.0
        idx = np.where(atm['HGT'] <= upto)

    # Get list of parameters to scale
    if which is None:
        plist = [p for p in atm.keys() if (p != 'HGT' and p != 'header')]
    else:
        plist = which.split()

    # ---------------  1: SCALE ATM PROFILES  --------------------
    for p in plist:
        if upto is None:
            atm[p] = atm[p] * scaling
        else:
            atm[p][idx] = atm[p][idx] * scaling

    # ---------------  2: MAKE OUTPUT STRUCTURE  ----------------
    # Write header
    header = []
    header.append("! Modified atmospheric profile for RFM input")
    header.append("! Scaling atmospheric profiles: %s by %.2f" % (', '.join(plist), scaling))
    header.append("! Original file: %s" % fname)

    atm['header'] = header

    # --------  3: WRITE MODIFIED ATMOSPHERIC PROFILE  ----------
    outname = fname[:-4] + '_scaled.atm'
    writeAtm(atm, outname)

    if outpath:
        return atm, outname
    else:
        return atm

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

def plotAtm(atm, title='RFM atmosphere'):

    """
    This function plots the atmospheric profile structure generated by either modifyAtm1 or modifyAtm2

    SYNTAX: plotAtm(atm)

    :param atm: Atmospheric profile structure
    :param title: Title for the plot. Default is 'RFM Atmosphere'

    :return: -none-
    """
    # ------------------------------------------------------------------
    # -------------------------  PLOT ----------------------------------
    # ------------------------------------------------------------------
    fig, ((ax1, ax2, ax3, ax4, ax5, ax6, ax7, ax8, ax9, ax10),
          (ax11, ax12, ax13, ax14, ax15, ax16, ax17, ax18, ax19, ax20)) \
        = plt.subplots(2, 10, figsize=[16.0, 8.0], tight_layout=True)
    # fig.subplots_adjust(hspace=0.02)
    fig.suptitle(title)

    # --- Top panels ---
    # PRES
    ax1.plot(atm['PRE'], atm['HGT'], 'k')
    ax1.set(title='Pressure', ylabel='Height [km]')
    ax1.tick_params(axis="y", direction="in", right="on")
    ax1.label_outer()
    # TEMP
    ax2.plot(atm['TEM'], atm['HGT'], 'k')
    ax2.set(title='Temperature')
    ax2.tick_params(axis="y", direction="in", right="on")
    ax2.label_outer()
    # HUMIDITY
    ax3.plot(atm['H2O'], atm['HGT'], 'b')
    ax3.set(title='H2O', xlabel='[ppmv]')
    ax3.tick_params(axis="y", direction="in", right="on")
    ax3.label_outer()
    # O3
    ax4.plot(atm['O3'], atm['HGT'], 'g')
    ax4.set(title='O3')
    ax4.tick_params(axis="y", direction="in", right="on")
    ax4.label_outer()
    # CO2
    ax5.plot(atm['CO2'], atm['HGT'], 'g')
    ax5.set(title='CO2')
    ax5.tick_params(axis="y", direction="in", right="on")
    ax5.label_outer()
    # N2O
    ax6.plot(atm['N2O'], atm['HGT'], 'g')
    ax6.set(title='N2O')
    ax6.tick_params(axis="y", direction="in", right="on")
    ax6.label_outer()
    # CO
    ax7.plot(atm['CO'], atm['HGT'], 'g')
    ax7.set(title='CO')
    ax7.tick_params(axis="y", direction="in", right="on")
    ax7.label_outer()
    # CH4
    ax8.plot(atm['CH4'], atm['HGT'], 'g')
    ax8.set(title='CH4')
    ax8.tick_params(axis="y", direction="in", right="on")
    ax8.label_outer()
    # O2
    ax9.plot(atm['O2'], atm['HGT'], 'g')
    ax9.set(title='O2')
    ax9.tick_params(axis="y", direction="in", right="on")
    ax9.label_outer()
    # SO2
    ax10.plot(atm['SO2'], atm['HGT'], 'r')
    ax10.set(title='SO2')
    ax10.tick_params(axis="y", direction="in", right="on")
    ax10.label_outer()

    # --- Bottom panels ---
    # PRES
    ax11.plot(atm['PRE'], atm['HGT'], 'k')
    ax11.set(xlabel='[mb]', ylabel='Height [km]', ylim=[0, 5])
    ax11.tick_params(axis="y", direction="in", right="on")
    # TEMP
    ax12.plot(atm['TEM'], atm['HGT'], 'k')
    ax12.set(xlabel='[K]', ylim=[0, 5])
    ax12.tick_params(axis="y", direction="in", right="on")
    ax12.label_outer()
    # HUMIDITY
    ax13.plot(atm['H2O'], atm['HGT'], 'b')
    ax13.set(xlabel='[ppmv]', ylim=[0, 5])
    ax13.tick_params(axis="y", direction="in", right="on")
    ax13.label_outer()
    # O3
    ax14.plot(atm['O3'], atm['HGT'], 'g')
    ax14.set(xlabel='[ppmv]', ylim=[0, 5])
    ax14.tick_params(axis="y", direction="in", right="on")
    ax14.label_outer()
    # CO2
    ax15.plot(atm['CO2'], atm['HGT'], 'g')
    ax15.set(xlabel='[ppmv]', ylim=[0, 5])
    ax15.tick_params(axis="y", direction="in", right="on")
    ax15.label_outer()
    # N2O
    ax16.plot(atm['N2O'], atm['HGT'], 'g')
    ax16.set(xlabel='[ppmv]', ylim=[0, 5])
    ax16.tick_params(axis="y", direction="in", right="on")
    ax16.label_outer()
    # CO
    ax17.plot(atm['CO'], atm['HGT'], 'g')
    ax17.set(xlabel='[ppmv]', ylim=[0, 5])
    ax17.tick_params(axis="y", direction="in", right="on")
    ax17.label_outer()
    # CH4
    ax18.plot(atm['CH4'], atm['HGT'], 'g')
    ax18.set(xlabel='[ppmv]', ylim=[0, 5])
    ax18.tick_params(axis="y", direction="in", right="on")
    ax18.label_outer()
    # O2
    ax19.plot(atm['O2'], atm['HGT'], 'g')
    ax19.set(xlabel='[ppmv]', ylim=[0, 5])
    ax19.tick_params(axis="y", direction="in", right="on")
    ax19.label_outer()
    # SO2
    ax20.plot(atm['SO2'], atm['HGT'], 'r')
    ax20.set(xlabel='[ppmv]', ylim=[0, 5])
    ax20.tick_params(axis="y", direction="in", right="on")
    ax20.label_outer()


# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

def writeRFMdrv(wn_start, wn_stop, npwer_wn, height=0.0, elev=90.0, atm="./atm/mls.atm", levels=[],
                gas="H2O", layer=False, conc={'H2O': 18000}, pres=1013, temp=298, out="RAD BBT OPT TRA",
                pathlength=0.1, CTM=True, MIX=True, PRF=False):
    """
    This function writes the RFM driver for a slant path looking upward into the atmosphere

    SYNTAX: writeRFMdrv(wn_start, wn_stop, npwer_wn, height=0.0, elev=90.0, atm="mid_lat_summer.atm",
                                levels=[])

    :param wn_start: Start of simulation range                                                    [cm^-1]
    :param wn_stop: End of simulation range                                                        [cm^-1]
    :param npwer_wn: Resolution of the simulation in points                                       [per cm^-1]
    :param height: Height of the observer                                                           [km]
                    Default is 0.0
    :param elev: Elevation angle of the slant path                                                  [degrees]
                    Default is 90.0 (vertical)
    :param atm:  Local name of the atmospheric profile to be used.
                    All atmospheric profiles should be stored in "./atm/"
                    Default is "./atm/mls.atm", a standard atmosphere for mid latitude summer
    :param levels: Array with the levels for intermediate output                                    [km]

    :return: nothing
    """
    # Open file
    drv = open(r"rfm.drv", "w")

    # WRite header
    drv.write("! RFM driver table for radiance calculation")
    drv.write("\n\n")

    # ---------------------- HDR SECTION  -----------------------
    # Write header
    drv.write("*HDR\n")
    if layer:
        drv.write("Single homogeneous layer")
    else:
        drv.write("Slant ground-based observation")
    drv.write("\n\n")

    # ---------------------- FLG SECTION  -----------------------
    # Write flags
    # ZEN: Zenith viewing (looking up)
    # OBS: Specifying the observer height
    # CTM: Include Continuum Absorption
    # MIX: Include Line-mixing
    # RAD: Output Radiance
    # BBT: Output Brightness Temperature
    # TRA: Output Transmission
    # ABS: Output Absorption
    # OPT: Output Optical Depth
    # PRF: Output RFM internal profile
    # LEV: Use additional levels for output
    # SHH: Silence terminal messages
    drv.write("*FLG\n")
    if layer:
        flags = "HOM"
    else:
        flags = "ZEN OBS"
    if "H2O" in gas.split() and CTM == True:
        flags = flags + " CTM"
    if "CO2" in gas.split() and MIX == True:
        flags = flags + " MIX"
    flags = flags + " " + out
    if PRF:
        flags = flags + " PRF"
    if levels != []:
        flags = flags + " LEV"
    flags = flags + " SHH"
    drv.write(flags)
    drv.write("\n\n")

    # ---------------------- SPC SECTION  -----------------------
    # Write Grid Details
    drv.write("*SPC\n")
    drv.write("%f\t%f\t%f" % (wn_start, wn_stop, npwer_wn))
    drv.write("\n\n")

    # ---------------------- GAS SECTION  -----------------------
    # Write Gas Details
    drv.write("*GAS\n")
    drv.write(gas)
    drv.write("\n\n")

    # ---------------------- ATM SECTION  -----------------------
    # Write Atmospheric profile location
    drv.write("*ATM\n")
    if layer:
        drv.write("TEM=%f\n" % temp)
        drv.write("PRE=%f\n" % pres)
        for item in gas.split():
            drv.write("%s=%f\n" % (item, conc[item]))
    else:
        drv.write("%s" % atm)
    drv.write("\n")

    if layer:
        # ---------------------- LEN SECTION  -----------------------
        # Write pathlength for single layer
        drv.write("*LEN\n")
        drv.write("%f" % pathlength)  # pathlength [in km]
        drv.write("\n\n")

    else:
        # ---------------------- ELE SECTION  -----------------------
        # Write elevation angle for slant path
        drv.write("*ELE\n")
        if isinstance(elev, list): drv.write(", ".join(["%f" % x for x in elev]))
        else: drv.write("%f" % elev)        # elevation angle in degrees from horizontal)
        drv.write("\n\n")

        # ---------------------- OBS SECTION  -----------------------
        # Write height of the observer
        drv.write("*OBS\n")
        drv.write("%f" % height)  # height of observer in km
        drv.write("\n\n")

    # ---------------------- HIT SECTION  -----------------------
    # Define location of HITRAN
    drv.write("*HIT\n")
    drv.write("./RFM/Hitran/hitran_2020.bin")
    drv.write("\n\n")

    # # ---------------------- LEV SECTION  -----------------------
    # # When present, indicate intermediate levels for additional output files
    if levels != []:
        drv.write("*LEV\n")
        line = [str(x) for x in levels]
        drv.write(' '.join(line))
        drv.write("  ! Additional levels for output")
        drv.write("\n\n")

    # ---------------------- OUT SECTION  -----------------------
    # Define output names
    drv.write("*OUT\n")
    drv.write("OUTDIR = ./RFM/output/\n")
    for item in out.split():
        drv.write("%sFIL = %s*.out\n" % (item, item.lower()))
    drv.write("PRFFIL = ./atm/prf.atm\n")
    drv.write("\n")

    # # ---------------------- XSC SECTION  -----------------------
    # # Define location of XSC profiles
    # drv.write("*XSC\n")
    # drv.write("./RFM/XSCfiles/*.xsc  ! Location of cross-sections for aerosol species\n")
    # drv.write("\n")

    # ---------------------- END FILE  -----------------------
    # Write END marker
    drv.write("*END\r")

    # Close file
    drv.close()


# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------

def sliceAtm(fname, h1, h2, outpath=False):
    """
    This function extracts a slice of an existing atmosphere between two specified heights.
    The function returns an atmospheric structure and writes a text file with the atmospheric profile for RFM input.

    SYNTAX: out[, outname] = sliceAtm(fname, h1, h2[, outpath=False])

    :param fname: Pathname to the standard atmospheric file to extract gas quantities
    :param h1: Bottom height of the slice                                                                   [km]
    :param h2: Top height of the slice                                                                      [km]
    :param outpath: If True, also returns the pathname to the new atmosphere.
                    Default is False

    :return: Dictionary containing the modified atmospheric profiles
            The script also creates a new text file with the modified atmosphere in the local directory
    """
    # Read in original profile
    atm = readAtm(fname)

    # ---------------  1: EXTRACT SLICE OF INTEREST  --------------------
    # Extract index of slice to be extracted
    hgt = [x for x in atm['HGT'] if h1 <= x <= h2]

    # Bracket heights with h1 and h2
    if len(hgt) == 0:
        hgt = [h1, h2]
    else:
        if hgt[0] > h1:
            hgt.insert(0, np.round(h1, 3))
        if hgt[-1] < h2:
            hgt.append(np.round(h2, 3))

    # Write new header
    header = []
    header.append("! Modified atmospheric profile for RFM input")
    header.append("! Slice taken from original atmosphere between altitudes: %.3f - %.3f km" % (h1, h2))
    header.append("! Original file: %s" % fname)

    # Rewrite atmosphere slice
    out = {}
    for key in atm.keys():
        if key == 'header':
            out[key] = header
        else:
            out[key] = np.interp(hgt, atm['HGT'], atm[key])     # Sample profile along new height axis

    # --------  2: WRITE MODIFIED ATMOSPHERIC PROFILE  ----------
    outname = fname[:-4] + '_slice.atm'
    writeAtm(out, outname)

    if outpath:
        return out, outname
    else:
        return out


# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

def readRFM(fname):
    """
    This function reads RFM output files.

    SYNTAX: file = readRFM(fname)

    :param fname: pathname of the file to be read

    :return: dictionary with the following:

        Field           Description                                                     Variable            Units

        ['header']      a list of strings with the first few lines of the file          String
        ['type']        the output data type                                            String
        ['wn_start]   the start of the spectral range                                 Float               [cm^-1]
        ['wn_stop]     the end of the spectral range                                   Float               [cm^-1]
        ['step]         the step interval between data points                           Float               [cm^-1]
        ['wn']        the X axis of the dataset in wavenumbers                        Float (array)       [cm^-1]
        ['data']        the spectrum                                                    Float (array)

    """

    # Open file
    file = open(fname, "r")

    # Read the whole file into a list
    temp = file.readlines()

    # Create output structure
    output = {}

    # Extract Header
    output['Header'] = temp[0:3]

    # Extract Grid details
    grid = temp[3].split()
    npts        = int(grid[0])
    wn_start  = float(grid[1])
    resolution  = float(grid[2])
    wn_stop    = float(grid[3])
    type        = grid[4][1:]

    # Append grid details
    output['npts']          = npts
    output['wn_start']    = wn_start
    output['resolution']    = resolution
    output['wn_stop']      = wn_stop
    output['type']          = type

    # Make X-axis
    # wn = np.empty(len(temp[4].split()))
    # for i in range(len(wn)): wn[i] = wn_start + i * resolution
    # output['wn'] = wn
    output['wn'] = np.linspace(wn_start, wn_stop,len(temp[4].split()))

    # Read data
    # data = np.empty(len(temp[4].split()))
    # for i in range(len(wn)): data[i] = float(temp[4].split()[i])
    # output['data'] = data
    output['data'] = np.asarray([float(x) for x in temp[4].split()])

    # Close file
    file.close()

    return output

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

def readSounding(fname):
    """
    This function reads in data from a University of Wyoming Sounding and returns a DataFrame with the relevant profiles

    SYNTAX: out = readSounding(fname)

    :param fname: Pathname of the file to read

    :return: dictionary with the following structure (some fields may not be included):

                                                        Units               Converted

        ['PRES']	Atmospheric Pressure	            [hPa]               [mb]
        ['HGHT']	Geopotential Height	                [meter]             [km]
        ['TEMP']	Temperature	                        [celsius]           [Kelvin]
        ['DWPT']	Dewpoint Temperature	            [celsius]           [Kelvin]
        ['FRPT']	Frost Point Temperature	            [celsius]           [Kelvin]
        ['RELH']	Relative Humidity	                [%]
        ['RELI']	Relative Humidity /Ice	            [%]
        ['MIXR']	Mixing Ratio	                    [gram/kilogram]     [ppmv]
        ['DRCT']	Wind Direction	                    [degrees true]
        ['SKNT']	Wind Speed	                        [knot]              [m/s]
        ['THTA']	Potential Temperature	            [kelvin]
        ['THTE']	Equivalent Potential Temperature	[kelvin]
        ['THTV']	Virtual Potential Temperature	    [kelvin]

    """

    # Open file for reading
    file = open(fname, 'r')
    lines = file.readlines()

    # Parse out time stamp
    header = lines[0].split()
    station_number = header[0]
    station_code = header[1]
    station_name = header[2]
    year = header[-1]
    month = header[-2]
    day = header[-3]
    time = header[-4]
    dt = datetime.strptime(year+month+day+time, "%Y%b%d%HZ")
    dt_text = datetime.strftime(dt, "%Y/%m/%d - %H:%M")

    # Find header

    # What are the variables?
    dashes = [i for i in range(len(lines)) if "--------------" in lines[i]]
    var = lines[dashes[0] + 1]
    var = var.strip()
    var = var.split()
    units = lines[dashes[0] + 2]
    units = units.strip()
    units = units.split()

    # Find the start and end of the data
    start = dashes[-1] + 1
    info_start = [i for i in range(len(lines)) if "Station information and sounding indices" in lines[i]][0]
    i = 1
    looking = True
    while looking:
        idx = info_start - i
        if len(lines[idx].split()) == len(var):
            looking = False
            end = idx
        else:
            i += 1

    # Extract data from text file
    data = np.loadtxt(fname, dtype=float, skiprows=start, max_rows=end-start)
    output = {}
    # Place in structure for output
    for x in range(len(var)):
        output[var[x]] = data[:,x]

    # Close file after use
    file.close()

    # Convert values to the ones used for RFM
    output['datetime'] = dt
    output['textdate'] = dt_text
    output['station_number'] = station_number
    output['station_name'] = station_name
    output['station_code'] = station_code
    output['PRES'] = output['PRES']                                 # Pressure im mb
    output['HGHT'] = output['HGHT'] / 1000                          # Height in kilometers
    output['TEMP'] = output['TEMP'] + 273.15                        # Temperature in Kelvins
    output['DWPT'] = output['DWPT'] + 273.15                        # Frost point in Kelvins
    output['MIXR'] = 1000 * output['MIXR'] * 28.964001 / 18.01528   # Mixing ratio in ppmv
    output['SKNT'] = output['SKNT'] * 0.514444                      # Windspeed in m/s

    return(output)


# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

def plotRFM(rfm, main='wn', reverse=False):
    """Plot an RFM output file with axes labelled according to its type"""

    if isinstance(rfm, str):
        rfm = readRFM(rfm)

    fig, ax = plt.subplots(figsize=[12, 6])

    ax.plot(rfm['wn'], rfm['data'])
    if reverse:
        ax.invert_xaxis()
    ax.set(xlabel='Wavenumber [$cm^{-1}$]', ylabel=rfm['type'])

    if rfm['wn'].max() < 10000:
        # Add micron wnlength axis
        def fw(x):
            return 10000 / x

        umax = ax.secondary_xaxis('top', functions=(fw, fw))
        umax.minorticks_on()
        umax.set_xlabel('Wavelength [$\mu$m]')
    else:
        # Add nanometer wnlength axis
        def fw(x):
            return 1e7 / x

        umax = ax.secondary_xaxis('top', functions=(fw, fw))
        umax.minorticks_on()
        umax.set_xlabel('Wavelength [$nm$]')


def readStd(fname, returnStd=False):

    # Read lines from text file
    with open(fname, 'r') as f:
        lines = f.readlines()

    # Extract header and column names
    atm = {}
    atm['header'] = [lines[0]]
    keys = lines[1].split()[1:]

    # Extract the values in each column
    std = {}
    for key in keys:
        std[key] = []
    for l in lines[2:]:
        for i, key in enumerate(keys):
            std[key].append(float(l.split()[i]))

    # Make into numpy arrays
    for key in std.keys():
        std[key] = np.asarray(std[key])

    # Creating RFM atmosphere object (from molec.cm^-3 to ppmv)
    for key in std.keys():
        if key == 'z(km)':
            atm['HGT'] = std[key]
        elif key == 'p(mb)':
            atm['PRE'] = std[key]
        elif key == 'T(K)':
            atm['TEM'] = std[key]
        elif '(cm-3)' in key and 'air' not in key:
            atm[key.replace('(cm-3)', '').upper()] = std[key] / std['air(cm-3)'] * 1e6

    # Flip the vertical profiles
    for key in atm.keys():
        if key != 'header':
            atm[key] = np.flip(atm[key])

    if returnStd:
        return atm, std
    else:
        return atm

def plotAtm2(atm, title='RFM atmosphere', upto=None):

    """
    This function plots the atmospheric profile structure generated by either modifyAtm1 or modifyAtm2

    SYNTAX: plotAtm(atm)

    :param atm: Atmospheric profile structure
    :param title: Title for the plot. Default is 'RFM Atmosphere'

    :return: -none-
    """
    # ------------------------------------------------------------------
    # -------------------------  PLOT ----------------------------------
    # ------------------------------------------------------------------

    gases = [key for key in atm.keys() if key not in ['HGT', 'PRE', 'TEM', 'header']]
    n = 2 + len(gases)

    fig, axes = plt.subplots(1, n, figsize=[2.5 + n * 1.25 , 6], tight_layout=True)
    fig.suptitle(title)

    for i, key in enumerate(['PRE', 'TEM'] + gases):
        if key in plot_colors.keys():
            color = plot_colors[key]
        else:
            color = 'tab:grey'
        title = key
        if key in gases:
            xlabel = ['ppmv']
        elif key == 'PRE':
            xlabel = '[mb]'
        elif key == 'TEM':
            xlabel = '[K]'

        # --- Top panels ---
        axes[i].plot(atm[key], atm['HGT'], c=color)
        axes[i].set(title=title, ylabel='Height [km]', xlabel=xlabel, ylim=[0, upto])
        axes[i].tick_params(axis="y", direction="in", right="on")
        axes[i].label_outer()
