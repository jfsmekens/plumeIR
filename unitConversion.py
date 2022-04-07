"""
Written by @jfsmekens
Last updated 01/04/2022

This module contains small functions to convert units

"""
import numpy as np
from constants import *

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
def ppm2scd(ppm, pathlength, temp=298, pres=1013, elev=90, output='molec/cm2'):
    """
    Simple function to calculate slant column density (SCD) from a volume mixing ratio (ppm) amd a given pathlength
       for a homogeneous layer. Ig given a vertcial profile of VMRS instead, it will calculate the total SCD over the
       entire profile.

    :param ppm: Gas VMR (or vertical profile of VMRs                                    [in ppm]
    :param pathlength: Pathlength of the layer (or altitudes)                           [in km]
    :param temp: Gas temperature. Default is 298.                                       [in K]
    :param pres: Gas pressure. Default is 1013.                                         [in mbar]
    :param output: Unit for the SCD output.
            'molec/cm2' [Default]
            'ppmm'
    :return: The computed SCD for the layer (or atmospheric column)
    """
    avo = 6.02214e23  # Avogadro constrant [mol-1]
    R = 8.31446261815324 * 1e-2  # Gas constant [m3⋅mbar⋅K−1⋅mol−1]

    if output == 'ppmm':
        scd = ppm * pathlength  # Ignore P,T and simply multiply by pathlength
    elif output == 'molec/cm2':
        n_tot = pres / R / temp  # Number of moles [mol/m3]]
        n_gas = ppm * 1e-6 * n_tot  # Number of moles of the relevant gas [mol/m3]
        n_gas = n_gas * avo  # in [molec/m3]
        if isinstance(pathlength, list) or isinstance(pathlength, np.ndarray):
            scd = np.trapz(n_gas, x=pathlength * 1e3) / np.sin(np.deg2rad(elev)) # Integrate over the atmospheric column
        else:
            scd = n_gas * pathlength * 1e3  # Integrate over the pathlength
        scd = scd * 1e-4  # in [molec/cm2]

    return scd


def scd2ppm(scd, pathlength, temp=298, pres=1013, input='molec/cm2', species=None):
    """
    Simple function to calculate Volume Mixing Ratio (VMR, in ppm) of a gas for a homogeneous layer
        given a Slant Column Density and a pathlength

    :param scd: SCD (see units in 'input' parameter
    :param pathlength: Pathlength of the layer                                      [in km]
    :param temp: Gas temperature. Default is 298.                                   [in K]
    :param pres: Gas pressure. Default is 1013.                                     [in mbar]
    :param input: Unit for the SCD input.
            'molec/cm2' [Default]
            'ppmm'
            'g/m2'
    :return: The computed Volume Mixing Ratio (VMR)                                 [in ppm]
    """
    avo = 6.02214e23  # Avogadro constrant [mol-1]
    R = 8.31446261815324 * 1e-2  # Gas constant [m3⋅mbar⋅K−1⋅mol−1]

    if input == 'ppmm':
        ppm = scd / (pathlength * 1e3) # Ignore P,T and simply divide by pathlength
    elif input == 'molec/cm2':
        n_gas = scd * 1e4 / (pathlength * 1e3)  # Turn to [molec/m2] and divide by pathlength to find conc [molec/m3]
        n_tot = pres / R / temp * avo  # Number of molecules in a m3 volume [molec/m3]
        ppm = n_gas / n_tot * 1e6  # Volume Mixing Ratio [in ppmv]
    elif input == 'g/m2':
        scd = gm2mcm2(scd, species=species)
        n_gas = scd * 1e4 / (pathlength * 1e3)  # Turn to [molec/m2] and divide by pathlength to find conc [molec/m3]
        n_tot = pres / R / temp * avo  # Number of molecules in a m3 volume [molec/m3]
        ppm = n_gas / n_tot * 1e6  # Volume Mixing Ratio [in ppmv]

    return ppm


def ppmm2mcm(ppmm, pres=1013, temp=298):
    """
    Converts Slant Column Density from [ppm.m] to [molec/cm2]

    :param ppmm: SCD in [ppm.m]
    :param pres: Gas pressure. Default is 1013.                                 [in mbar]
    :param temp: Gas temperature                                                [in K]
    :return: SCD in [molec/cm2]
    """
    avo = 6.02214e23  # Avogadro constrant [mol-1]
    R = 8.31446261815324 * 1e-2  # Gas constant [m3⋅mbar⋅K−1⋅mol−1]
    mcm = ppmm * 1e-6 * pres / R / temp * avo * 1e-4

    return mcm


def mcm2ppmm(mcm, pres=1013, temp=298):
    """
    Converts Slant Column Density from [molec/cm2] to [ppm.m]
    :param mcm: SCD in [molec/cm2]
    :param pres: Gas pressure. Default is 1013.                                 [in mbar]
    :param temp: Gas temperature                                                [in K]
    :return: SCD in [ppm.m]
    """
    avo = 6.02214e23  # Avogadro constrant [mol-1]
    R = 8.31446261815324 * 1e-2  # Gas constant [m3⋅mbar⋅K−1⋅mol−1]
    ppmm = mcm * 1e4 / avo * R * temp / pres * 1e6

    return ppmm

def mcm2gm2(mcm, species):

    avo = 6.02214e23  # Avogadro constrant [mol-1]
    mol_weight = weights[species]
    gm2 = mcm * 1e4 / avo * mol_weight

    return gm2

def gm2mcm2(gm2, species):

    avo = 6.02214e23  # Avogadro constrant [mol-1]
    mol_weight = weights[species]
    mcm = gm2 / mol_weight * avo / 1e4

    return mcm


def RH2ppm(RH, pres=1013, temp=298):
    if temp > 273.16:  # Saturation over water

        # Constants
        Tc = 647.096  # Critical temperature [in K]
        Pc = 220640  # Critical pressure [in hPa]
        c1 = -7.85951783
        c2 = 1.84408259
        c3 = -11.7866497
        c4 = 22.6807411
        c5 = -15.9618719
        c6 = 1.80122502

        t = 1 - (temp / Tc)  # New variable
        polyfit = Tc / temp * (c1 * t + c2 * t ** 1.5 + c3 * t ** 3 + c4 * t ** 3.5 + c5 * t ** 4 + c6 * t ** 7.5)
        Psat = np.exp(polyfit) * Pc

    else:  # Saturation over ice

        # Constants
        Pn = 6.11657  # Vapor pressure at triple point temperature [in hPa]
        Tn = 273.16  # Triple point temperature [in K]
        a0 = -13.928169
        a1 = 34.707823

        t = temp / Tn  # New variable
        polyfit = a0 * (1 - t ** -1.5) + a1 * (1 - t ** -1.25)
        Psat = np.exp(polyfit) * Pn

    # Now to concentration
    ppm = RH / 100 * Psat / pres * 1e6

    return ppm

def ppm2RH(ppm, pres=1013, temp=298):
    if temp > 273.16:  # Saturation over water

        # Constants
        Tc = 647.096  # Critical temperature [in K]
        Pc = 220640  # Critical pressure [in hPa]
        c1 = -7.85951783
        c2 = 1.84408259
        c3 = -11.7866497
        c4 = 22.6807411
        c5 = -15.9618719
        c6 = 1.80122502

        t = 1 - (temp / Tc)  # New variable
        polyfit = Tc / temp * (c1 * t + c2 * t ** 1.5 + c3 * t ** 3 + c4 * t ** 3.5 + c5 * t ** 4 + c6 * t ** 7.5)
        Psat = np.exp(polyfit) * Pc

    else:  # Saturation over ice

        # Constants
        Pn = 6.11657  # Vapor pressure at triple point temperature [in hPa]
        Tn = 273.16  # Triple point temperature [in K]
        a0 = -13.928169
        a1 = 34.707823

        t = temp / Tn  # New variable
        polyfit = a0 * (1 - t ** -1.5) + a1 * (1 - t ** -1.25)
        Psat = np.exp(polyfit) * Pn

    # Now to RH
    RH = ppm * pres / Psat * 100 / 1e6

    return RH