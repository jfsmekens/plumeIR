"""
Written by @jfsmekens - Last updated 01/04/2022

This module contains all the functions necessary to initialise a retrieval:
    1. Read in the config file
    2. Sort files within data folder and rename if necessary
    3. Create Geometry and Retrieval objects
    4. Create Analyser objects

"""
# ======================================================================================================================
#                                         Read configuration files
# ======================================================================================================================
def readConfig(fname='./plumeIRconfig.txt'):
    """
    Read a configuration text file and return a directory with the various sections
    :param fname: [str] Path to the config file
    :return: Dictionary with the variables arranged by sections
    """

    # Read from config file
    with open(fname) as f:
        lines = f.readlines()

    # Remove empty lines
    lines = [l for l in lines if not l.startswith('\n')]

    # Remove comment lines
    lines = [l for l in lines if not l.startswith('#')]

    config = {}

    # Loop through lines
    for line in lines:

        # Remove comments
        line = line.split('#')[0]
        line = line.split('(')[0]

        # Look for section headers
        if line.startswith('*'):
            sec = line.strip('*\t\n ')
            config[sec] = {}

        # Assign parameters to active section
        else:
            # Extract key value pairs
            key, value = line.split(':')
            key = key.strip()
            value = value.strip()

            # Parse out which type of value it is
            if value == 'None':
                value = None
            elif value == '':
                value = None
            elif value == 'True':
                value = True
            elif value == 'False':
                value = False
            elif len(value.split(',')) > 1:
                try:
                    value = [float(x) for x in value.split(',')]
                except ValueError:
                    value = [x.strip() for x in value.split(',')]
            else:
                try:
                    value = float(value)
                except ValueError:
                    value = value

            # Assign parameter to dictionary
            config[sec][key] = value

    return config

# ======================================================================================================================
#                                 Read standard file with column amounts
# ======================================================================================================================
def readStandard():
    """
    Read the standard text file, which contains default values for reference quantities.
        (gas concentrations, particle sizes, etc.)
    :return: Dictionary with the default variables for all species
    """

    # Read from .ini file
    with open('./standards.txt') as f:
        lines = f.readlines()

    # Find the categories
    line = [l for l in lines if l.startswith('*')][0]
    line = line.strip('*')
    line = line.split('#')[0]
    keys = [key.strip() for key in line.split(':')]

    # Remove empty lines
    lines = [l for l in lines if not l.startswith('\n')]

    # Remove comment lines
    lines = [l for l in lines if not l.startswith('*')]
    lines = [l for l in lines if not l.startswith('#')]

    std = {}

    # Loop through lines
    for line in lines:

        # Remove comments
        line = line.split('#')[0]
        line = line.split('(')[0]

        # Extract values pairs
        values = line.split(':')
        for i, v in enumerate(values):

            # Remove spaces and hidden characters
            v = v.strip()

            # Parse out which type of value it is
            if v == 'None':
                v = None
            elif v == '':
                v = None
            elif v == 'True':
                v = True
            elif v == 'False':
                v = False
            elif len(v.split(',')) > 1:
                try:
                    v = [float(x) for x in v.split(',')]
                except ValueError:
                    v = [x.strip() for x in v.split(',')]
            else:
                try:
                    v = float(v)
                except ValueError:
                    v = v

            # Replace value in list
            values[i] = v

        # Assign values and fill in dictionary
        for i, key in enumerate(keys):
            if i == 0:
                name = values[i]
                std[name] = {}
            else:
                std[name][key] = values[i]

    return std

from constants import *
from parameters import Parameters, Geometry, Retrieval
from analysis import Analyser
from spectra import read_spectrum, findDateinName
from datetime import datetime
import numpy as np
from tqdm import tqdm
import logging
import glob
import os

logger = logging.getLogger(__name__)

# ======================================================================================================================
#                                               Main initialise call
# ======================================================================================================================
def initialiseFit(config_file='./plumeIRconfig.txt'):
    """
    Function to initialise the retrieval based on the config file.
    :param config_file: [str] Path to the config file
    :return: config, geometry, params, retrieval, analysers
                config      - The config dictionary, updated with a TARGET section
                geometry    - The Geometry object for the retrieval
                params      - A list of Parameters objects (1 for each fit)
                retrieval   - The Retrieval object for the retrieval
                analysers   - A list of Analysers objects (1 for each fit)
    """

    # Read config file
    config = readConfig(config_file)

    # ---------------------------------------------------------------------
    # Define working directory and make list fo spectra
    # ---------------------------------------------------------------------
    data_dir = config['DATA']['data_dir']
    if not data_dir.endswith('/'):
        data_dir = data_dir + '/'
    if not os.path.exists(data_dir):
        raise ValueError('Invalid path for data directory: %s does not exist' % data_dir)
    logger.info('Working in: %s' % data_dir)
    flist = glob.glob(data_dir + '*')
    config['DATA']['data_dir'] = data_dir

    # Only keep the potential spectra files
    flist = [f for f in flist if not os.path.isdir(f)]
    flist = [f for f in flist if f.split('.')[-1] not in invalid_extensions]
    if config['DATA']['filter_files'] and config['DATA']['filter_ext'] is not None:
        flist = [f for f in flist if f.split('.')[-1] in config['DATA']['filter_ext']]
    
    # Look for clear and blackbody spectra and remove from main list
    bbc_list = [f for f in flist if any([s in f for s in ['bbc', 'BBC']])]
    bbh_list = [f for f in flist if any([s in f for s in ['bbh', 'BBH']])]
    clear_list = [f for f in flist if any([s in f for s in ['clear', 'sky']])]
    self_list = [f for f in flist if any([s in f for s in ['self']])]
    
    flist = [f for f in flist if not any([f in L for L in [bbc_list, bbh_list, clear_list, self_list]])]

    # Only keep files with certain filenames?
    if config['DATA']['filter_files']:
        flist = [f for f in flist if config['DATA']['filter_string'] in f]

    # Sort by time
    n_spec = len(flist)
    try:
        findDateinName(flist[0])
        flist.sort()
        logger.info('Found %i spectra, sorted by name' % n_spec)
    except:
        logger.info('Found %i spectra, filename does not contain timestamp' % n_spec)
        logger.info('Sorting by acquisition time and renaming...')

        # Get acquisition times
        times = []
        pbar = tqdm(total=n_spec)
        pbar.set_description('Reading times')
        corrupt_count = 0
        corrupt_list = []
        for i, fname in enumerate(flist):
            try:
                spec = read_spectrum(fname)
                times.append(datetime.timestamp(spec.dtime))
                pbar.update()
            except ValueError:
                corrupt_count += 1
                pbar.set_postfix_str('%i corrupted_files' % corrupt_count)
                flist.remove(fname)
                corrupt_list.append(fname)
                pbar.update()
        pbar.close()

        # Update n_spec based on removals
        n_spec = len(flist)

        # Relocate corrupted items
        corrupted_dir = os.path.dirname(flist[0]) + '/corrupted_files'
        if not os.path.exists(corrupted_dir):
            os.mkdir(corrupted_dir)
        for fname in corrupt_list:
            os.rename(fname, corrupted_dir + '/' + fname.split('/')[-1])

        # Rename
        pbar = tqdm(total=n_spec)
        pbar.set_description('Renaming')
        # breakpoint()
        for i, fname in enumerate(flist):
            ext = fname.split('.')[-1]
            if ext not in midac_extensions:
                ext = '0'
            old = fname.split('/')[-1]
            dir = fname.replace(old, '')
            root = ''.join([i for i in old.split('.')[0] if not i.isdigit()])
            datestr = datetime.strftime(datetime.fromtimestamp(times[i]), '%Y_%m_%d_%H%M_%S_%f')
            new = dir + root + datestr + '.' + ext
            os.rename(fname, new)
            flist[i] = new
            pbar.update()
        pbar.close()
        flist.sort()

        logger.info('...Done!')

    # Limit analysis to a max number of spectra?
    max_spec = int(config['DATA']['max_spec'])
    if max_spec is not None:
        if n_spec > max_spec:
            flist = flist[0:max_spec]
            logger.info('Analyzing the first %i' % max_spec)
    
    # Append all lists to config file
    config['DATA']['spec_list'] = flist
    config['DATA']['bbc_list'] = bbc_list
    config['DATA']['bbh_list'] = bbh_list
    config['DATA']['clear_list'] = clear_list

    # ---------------------------------------------------------------------
    # Make Geometry object
    # ---------------------------------------------------------------------
    geometry = Geometry(**config['GEOMETRY'])

    # ---------------------------------------------------------------------
    # Make Retrieval object
    # ---------------------------------------------------------------------
    config['RETRIEVAL']['type'] = config['GEOMETRY']['type']
    retrieval = Retrieval(**config['RETRIEVAL'])

    # ---------------------------------------------------------------------
    # Make Parameters and Analyzers
    # ---------------------------------------------------------------------
    fit_dicts = [item for key, item in config.items() if 'FIT' in key and item['fit']]
    names = [key for key, item in config.items() if 'FIT' in key and item['fit']]
    params = []
    analysers = []

    if retrieval.unified:
        fit_windows = []
        param = Parameters()
        for i, fit in enumerate(fit_dicts):
            param.extract(fit, retrieval)
            fit_windows = fit_windows + fit['fit_window']
        params.append(param)
        analysers.append(Analyser(param, geometry, fit_windows, retrieval,
                                  name='UNIFIT', data_dir=data_dir))

    for i, fit in enumerate(fit_dicts):

        param = Parameters()
        param.extract(fit, retrieval)
        params.append(param)
        analysers.append(Analyser(param, geometry, fit['fit_window'], retrieval,
                                  name=names[i], data_dir=data_dir))

    # ---------------------------------------------------------------------
    # Get the targets and ratios
    # ---------------------------------------------------------------------
    # How many fits are there?
    n_fits = len(analysers)

    # What are the target gases?
    targets = []
    for i in range(n_fits):
        targets = targets + analysers[i].params.targetList()

    # Which ratios can we plot with those?
    possible_ratios = []
    for a in targets:
        for b in targets:
            possible_ratios.append('%s:%s' % (a, b))
    ratios = [x for x in interesting_ratios if x in possible_ratios]

    # Append to config
    config['TARGET'] = {}
    config['TARGET']['targets'] = targets
    config['TARGET']['ratios'] = ratios
    
    # Append the number of everything important
    config['n_spec'] = n_spec if n_spec <= max_spec else max_spec
    config['n_fits'] = n_fits
    config['n_targets'] = len(targets)
    config['n_ratios'] = len(ratios)

    return config, geometry, params, retrieval, analysers

