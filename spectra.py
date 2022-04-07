"""
Written by @jfsmekens - Last updated 01/04/2022

This module contains functions to read OP-FTIR spectra.
Currently supports file formats for Bruker and Midac.
Defines the object class Spectrum.

"""
from brukeropusreader import read_file as read_opus
from pyspectra.readers import read_spc
from datetime import datetime
import matplotlib.pyplot as plt
import numpy as np
import os
import sys
from constants import midac_extensions

class silentCall(object):
    """
    Dedicated object class used to silence warnings and terminal outputs when using third party functions
    """
    def __init__(self,stdout = None, stderr = None):
        self.devnull = open(os.devnull,'w')
        self._stdout = stdout or self.devnull or sys.stdout
        self._stderr = stderr or self.devnull or sys.stderr

    def __enter__(self):
        self.old_stdout, self.old_stderr = sys.stdout, sys.stderr
        self.old_stdout.flush(); self.old_stderr.flush()
        sys.stdout, sys.stderr = self._stdout, self._stderr

    def __exit__(self, exc_type, exc_value, traceback):
        self._stdout.flush(); self._stderr.flush()
        sys.stdout = self.old_stdout
        sys.stderr = self.old_stderr
        self.devnull.close()

def silence_stdout():
    """
    Function to silence the terminal messages
    :return: 
    """
    with open(os.devnull, "w") as devnull:
        old_stdout = sys.stdout
        sys.stdout = devnull
        yield
        sys.stdout = old_stdout

def read_spectrum(fname):
    """
    Generic function to read a spectrum of any kind
    :param fname: [str] Path to the spectrum file
    :return: Spectrum object
    """

    ext = fname.split('/')[-1].split('.')[-1]
    if ext in ['spc', 'sbm', 'ifg', 'sb', 'abs']:
            spec = read_midac(fname)
    else:
        try:
            spec = read_bruker(fname)
        except ValueError:
            raise ValueError('Unable to read file: %s' % fname)

    return spec

def read_midac(fname):
    """
    Read Midac file [.spc, .ifg, .sbm, .sb, .abs]
    :param fname: [str] Path to the spectrum file
    :return: Spectrum object
    """

    # Read the spectrum
    with silentCall():
        spec = read_spc.spc.File(fname)

    # If file is empty, raise error
    if spec is None:
        raise ValueError('Could not read data from suspected Midac file: %s' % fname)

    # Determine what is being plotted
    if spec.ylabel == 'Arbitrary Intensity':
        spec_type = 'sbm'
    elif spec.ylabel == 'Interferogram':
        spec_type = 'ifg'
    elif spec.ylabel == 'Transmittance':
        spec_type = 'tra'
    elif spec.ylabel == 'Absorbance':
        spec_type = 'abs'
    elif spec.ylabel == 'Emission':
        spec_type = 'em'

    # Extract acquisition time
    try:
        dtime = findDateinSPC(spec)
    except:
        try:
            dtime = findDateinName(fname)
        except:
            dtime = None

    # Extract data
    x = np.asarray(spec.x)
    y = np.asarray(spec.sub[0].y)

    # Extract metadata
    metadata = {}
    try:
        logdict = spec.log_dict
        for key, value in logdict.items():
            metadata[key.decode('utf-8')] = value.decode('utf-8')
    except:
        pass

    # Assign all to a Spectrum object
    spectrum = Spectrum(x, y, spec_type=spec_type, dtime=dtime, metadata=metadata, fname=fname)

    return spectrum

def read_bruker(fname):
    """
    Read Bruker files [.0 or any extension really]
    :param fname: [str] Path to the spectrum file
    :return: Spectrum object
    """

    try:
        spec = read_opus(fname)
    except ValueError:
        raise ValueError('Could not read data from suspected Bruker file: %s' % fname)

    # Extract data
    y = np.asarray(spec['ScSm'])
    x = np.asarray(spec.get_range('ScSm'))

    # Extract acquisition time
    try:
        dtime = findDateinBruker(spec)
    except:
        try:
            dtime = findDateinName(fname)
        except:
            dtime = None

    spec_type = 'sbm'

    # Extract metadata
    metadata = {}
    metadata['Optik'] = spec['Optik']
    metadata['Fourier Transform'] = spec['Fourier Transformation']
    metadata['Sample'] = spec['Sample']
    metadata['Instrument'] = spec['Instrument']

    # Assign all to a Spectrum object
    spectrum = Spectrum(x, y, spec_type=spec_type, dtime=dtime, metadata=metadata, fname=fname)

    return spectrum


def findDateinName(fname):
    """
    Function to find the date and time of acquisition encoded in a filenname
    :param fname: [str] Path to the spectrum file
    :return: Datetime object
    """

    # First split the file name to get rid of the directory and extension
    string = fname.split('/')[-1].split('.')[0]

    # Extract all numbers and
    numbers = ''
    for char in string:
        if char.isdigit():
            numbers = numbers + char

    # Padd with zeros if some elements are missing
    maxlen = 20
    difflen = maxlen - len(numbers)
    for i in range(difflen):
        numbers = numbers + '0'

    # Make datetime object (format is string with no separators
    dtime = datetime.strptime(numbers, '%Y%m%d%H%M%S%f')

    return dtime

def findDateinSPC(spc_object):
    """
    Read date and time from a Midac file (passed on as the SPC object)
    :param spc_object: SPC object returned by read_spc() function
    :return: Datetime object
    """

    try:
        year = '%04i' % spc_object.year
    except:
        year = '0000'

    try:
        month = '%02i' % spc_object.month
    except:
        month = '00'

    try:
        day = '%02i' % spc_object.day
    except:
        day = '00'

    try:
        hour = '%02i' % spc_object.hour
    except:
        hour = '00'

    try:
        minute = '%02i' % spc_object.minute
    except:
        minute = '00'

    try:
        second = '%02i' % spc_object.second
    except:
        second = '00'

    try:
        msec = '%06i' % spc_object.msec
    except:
        msec = '000000'

    numbers = year + month + day + hour + minute + second + msec

    if int(numbers) == 0:
        raise ValueError('Could not extract time info from SPC file')
    else:
        dtime = datetime.strptime(numbers, '%Y%m%d%H%M%S%f')

    return dtime


def findDateinBruker(opus_object):
    """
    Read time from a Bruker file (passed on as an OPUS object after reading)
    :param opus_object: OPUS object retuned by the read_opus() function
    :return: Datetime object
    """

    date = opus_object['ScSm Data Parameter']['DAT']
    time = opus_object['ScSm Data Parameter']['TIM']

    # tz = time.split()[1].strip('()')[:-2]
    # tz_offset = '%+05i' % (int(time.split()[1].strip('()')[-2:]) * 100)
    time = time.split()[0] + '000'
    dtime = datetime.strptime(date + time, "%d/%m/%Y%H:%M:%S.%f")

    return dtime


class Spectrum():
    """
    Spectrum object containing the info extracted from the spectral files
    """

    def __init__(self, x, y, spec_type=None, dtime=None, metadata=None, fname=None):
        """
        Initialise the Spectrum object
        :param x: [float array] Wavelength or wavenumber axis
        :param y: [float array] Spectrum
        :param spec_type: [str] Type of spectrum
                    sbm     -   Single Beam (intensity, no meaningful units)
                    ifg     -   Interferogram
                    bbt     -   Brightness Temperature (in K)
                    rad     -   calibrated Radiance
                    diff    -   Radriance Difference (for emission fits)
        :param dtime: [datetime object] Timestamp
        :param metadata: [any]  Random collection of metadata in whatever form it comes (string or dictionnary most likely)
        :param fname: [str] Path to the file as it was read
        """

        # Make sure values are in ascending order
        if x[0] > x[-1]:
            x = np.flip(x)
            y = np.flip(y)

        self.spectrum = np.row_stack([x, y])    # The spectrum

        self.npts = len(y)                      # Length of the spectrum
        self.spacing = (x[1:] - x[0:-1]).mean() # Spectral resolution
        self.start_wave = x.min()               # Start wavenumber
        self.end_wave = x.max()                 # End wavenumber

        self.dtime = dtime                      # Timestamp

        if spec_type is not None:
            self.spec_type = spec_type          # Type of spectrum

        if metadata is not None:
            self.metadata = metadata            # Metadata if it exists (mostly from Bruker)
        else:
            self.metadata = {}

        try:
            if self.metadata['Optik']['CHN'] == 'Reference':
                self.bb = True                  # Is it a black body?
                self.target_temp = metadata['Instrument']['TSM'] + 273.15   # If so, what was the temperature?
        except KeyError:
            self.bb = False

        if fname is not None:
            self.fname = fname                  # File name as it was first read


    def plot(self):
        """
        Function to plot the spectrum with labels according to its type
        :return:
        """

        # Define axes labels based on what type of spectrum it is
        if self.spec_type == 'sbm':
            xlabel = 'Wavenumber [$cm^{-1}$]'
            ylabel = 'Single Beam'
        elif self.spec_type == 'ifg':
            xlabel = 'Steps'
            ylabel = 'Interferogram'
        elif self.spec_type == 'bbt':
            xlabel = 'Wavenumber [$cm^{-1}$]'
            ylabel = 'Brightness Temperature [K]'
        elif self.spec_type == 'rad':
            xlabel = 'Wavenumber [$cm^{-1}$]'
            ylabel = 'Radiance [$mW / (m^2.sr.cm^{-1})$]'
        elif self.spec_type == 'diff':
            xlabel = 'Wavenumber [$cm^{-1}$]'
            ylabel = 'Radiance Difference [$mW / (m^2.sr.cm^{-1})$]'

        # Unpack spectrum
        x, y = self.spectrum

        # Plot spectrum
        fig, ax = plt.subplots(figsize=[8, 4])
        ax.plot(x, y)
        ax.set(xlabel=xlabel, ylabel=ylabel)
        plt.show()

    def update(self, spectrum=None, dtime=None, bb=None, target_temp=None, metadata=None, fname=None):
        """
        Function to update any given value of the SPectrum object during processing
        :param spectrum: New spectrum (x, y)
        :param dtime: New timestamp
        :param bb: [bool] Is this a blackbody spectrum
        :param target_temp: [float] Temperature of the blackbody
        :param metadata: [any] New metadata
        :param fname: [str] New filepath
        :return:
        """

        if spectrum is not None:
            x, y = spectrum
            if len(x) == len(y):
                self.spectrum = spectrum
                self.npts = len(y)
                self.spacing = (x[1:] - x[0:-1]).mean()
                self.start_wave = x.min()
                self.end_wave = x.max()

            else:
                raise ValueError('Spectrum must be provided as a tuple with tow arrays of the same dimension')

        if dtime is not None:
            self.dtime = dtime

        if bb is not None:
            self.bb = bb

        if target_temp is not None:
            self.target_temp = target_temp

        if metadata is not None:
            self.metadata = metadata

        if fname is not None:
            self.fname = fname
