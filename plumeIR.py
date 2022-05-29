"""
Written by @jfsmekens - Last updated 01/04/2022

Main script for plumeIR. This is where the user sets the target data (or read from config file) and how the code should
    behave during the retrieval. The core modules of plumeIR are called to:

    1. Read in config file and creates objects for analysis (Geometry, Amnalyser(s), Retrieval)
    2. Plot results as they are being processed
    3. Create dataframes to save the results as they are being generated (save option must be on)
    4. Plot time series and ratios after analysis
    5. Option for a summary plot (also comparing with iFit data if any exists)

"""
# ======================================================================================================================
#                                               Import dependencies
# ======================================================================================================================
import sys
import os
import logging
import pickle
import pandas as pd
from tqdm import tqdm

from spectra import read_spectrum
from initialise import initialiseFit, readStandard
from plotting import *
from atmosphere import writeAtm

dark_mode = False
if dark_mode:
    plt.style.use('dark_background')

# ======================================================================================================================
#                                     Set up log output to standard output
# ======================================================================================================================
# Start a logger instance
logger = logging.getLogger()
logger.setLevel(logging.INFO)

# Set the date format for display
date_fmt = '%H:%M:%S'
formatter = logging.Formatter('%(asctime)s - %(message)s', date_fmt)

# Add a handler for log file
handler1 = logging.FileHandler('plumeIRlog.txt', 'w+')
handler1.setLevel(logging.INFO)
handler1.setFormatter(formatter)
logger.addHandler(handler1)

# Add a handler for console output
handler2 = logging.StreamHandler(sys.stdout)
handler2.setLevel(logging.INFO)
handler2.setFormatter(formatter)
logger.addHandler(handler2)

# ======================================================================================================================
#                     Initiate the retrieval from config file and define run behaviour
# ======================================================================================================================
logger.info('########## Retrieval run initiated - %s %s ###########' % (logotext, versiontext))
# Can be initiated by pointing to a file directly
config_name = './plumeIRconfig_emission.txt'     # Where to get the config from
config_name = None
if config_name is not None:
    logger.info('User supplied config file: %s' % config_name)

# Or by using a data directory where the config file can be found
# datadir = './data/Emission_170322_Vulcano/6/'
datadir = './data/2021-08-11/'
# datadir = None
if datadir is not None:
    config_list = glob(datadir + '*plumeIR*config*.txt')
    if len(config_list) == 0:
        raise ValueError('No config file found in directory: %s' % datadir)
    elif len(config_list) == 1:
        config_name = config_list[0]
        logger.info('User supplied datadir: %s' % datadir)
        logger.info('Found 1 config file: %s' % config_name)
    else:
        config_name = config_list[-1]
        logger.info('User supplied datadir: %s' % datadir)
        logger.info('Found %i config files. Using most recent: %s' % (len(config_list), config_name))

# Visual outputs during run
force_reference = False      # Force a rerun of the reference file even if it already exists
plot_reference = False      # Plot spectral reference for each fit?
write_analysis = False       # Write the analysis results in the progress bar?
plot_analysis = True        # Display results in the Analysis canvas?
scroll = 50                 # How many spectra to display at any one time
if not plot_analysis:
    write_analysis = True   # Write analysis results in the progress bar?
plotmass = False            # Plot all ratios as mass ratios?
plot_full_spectrum = False  # Display results in the Full Spectrum canvas

# Visual outputs after analysis
plot_results = True         # Display results at the end of the analysis
if plot_results:
    which = 'all'       # 'ts', 'ratios' or 'all'
    targets = 'all'     # List of targets to include in plots or 'all'
    ratios = 'all'      # List of ratios to include in plots or 'all
    use_cmap = False    # Color ratio plots by a 3rd quantity?df
    color_by = 'so2:h2so4'  # Which quantity? Any of the target gases or ratios (case insensitive)
    errorbar = True     # Plot error as errorbars? If not the error is shown as a shaded area in time series instead
    window = 10         # Moving window to calculate time series of ratios
    plot_individual = False     # Plot each fit as a separate time series (includes all species including non targets)

# Save results to disk
save_results = False        # Save results to disk? (includes dataframes as .csv and .xlsx and plots as .png)
save_frames = False         # Save individual frames during analysis?


# ======================================================================================================================
#                                 Initialize fit: Geometry + Species to fit
# ======================================================================================================================
# Initialize fit from config file
logger.info('Reading fit parameters from config file')
config, geometry, params, retrieval, analysers = initialiseFit(config_name, force_ref=force_reference)

# Extract total number of spectra, fits, targets and ratios
n_spec = config['n_spec']
n_fits = config['n_fits']
n_targets = config['n_targets']
n_ratios = config['n_ratios']

# Plot reference if requested
if plot_reference:
    for i in range(n_fits):
        analysers[i].plotReference(save=save_results)

# ======================================================================================================================
#                                 Make the figure canvas to display the results
# ======================================================================================================================
# Make Analysis canvas
logger.info('Making plot canvas...')
if plot_analysis:
    analysis_canvas = makeAnalysisCanvas(config)
# Make Full Spectrum canvas
if plot_full_spectrum:
    spectrum_canvas = makeSpectrumCanvas(config, analysers)

# ======================================================================================================================
#                                 Make Dataframes to hold the results
# ======================================================================================================================
# --- One DataFrame for each fit ---
df = []
for n in range(n_fits):

    # Make a list of the column names
    cols = ['File', 'Time']
    for name, p in params[n].items():
        # Species parameters: [scaling, scaling error, molec SCD, molec SCD error, mass SCD, mass SCD error]
        if any([p.atm_gas, p.plume_gas, p.plume_aero]):
            cols += [f'{name}_val', f'{name}_err', f'{name}_scd [molec.cm^-2]', f'{name}_scd error [molec.cm^-2]',
                     f'{name}_scd [g.m^-2]', f'{name}_scd error [g.m^-2]']
        elif 'deff' in name:    # Size parameters: [size, error]
            cols += [f'{name} [um]', f'{name} error [um]']
        else:   # Other parameters (shift, fov, polynomial, etc.) : [param, error]
            cols += [name, f'{name}_err']

    # Add RMSE and R^2 goodness of fit
    cols += ['RMSE', 'R2']

    # Create dataframe and append to list
    df.append(pd.DataFrame(index=np.arange(n_spec), columns=cols))

# --- Combined dataframe with only targets and ratios ---
cols = ['File', 'Time']
for n in range(n_fits):
    for name, p in params[n].items():
        if p.target:
            # Species parameters: [scaling, scaling error, molec SCD, molec SCD error, mass SCD, mass SCD error]
            if any([p.atm_gas, p.plume_gas, p.plume_aero]):
                cols += [f'{name}_val', f'{name}_err', f'{name}_scd [molec.cm^-2]', f'{name}_scd error [molec.cm^-2]',
                         f'{name}_scd [g.m^-2]', f'{name}_scd error [g.m^-2]']
            else:  # Other parameters (shift, fov, polynomial, etc.) : [param, error]
                cols += [name, f'{name}_err']
        if 'deff' in name:  # Size parameters: [size, error]
            cols += [f'{name} [um]', f'{name} error [um]']

    # Add RMSE and R^2 goodness of fit for each fit
    cols += ['%s RMSE' % analysers[n].name, '%s R2' % analysers[n].name]

# Add maximum RMSE and minimum R^2 goodness of fit for each fit
cols += ['MAX RMSE', 'MIN R2']

# Create empty summary dataframe
df_sum = pd.DataFrame(index=np.arange(n_spec), columns=cols)

# Create empty list to store full results
full_results = []

# ======================================================================================================================
#                                               Loop through files
# ======================================================================================================================
fitname = 'fitResults_%s' % (datetime.now().strftime("%y%m%d_%H%M%S"))  # Timecoded name for output
outdir = config['DATA']['data_dir'] + fitname + '/'                     # Where to save the results?
if save_results or save_frames:
    os.mkdir(outdir)    # Create output directory
if save_frames:
    os.mkdir(outdir + 'plots/')     # Also create directory for frames
outnames = []       # List of names for saving individual fits
for n in range(n_fits):
    outnames.append(outdir + '%s_%s.csv' % (fitname, analysers[n].name))
outnames.append(outdir + '%s_ALL.csv' % fitname)

pbar = tqdm(total=n_spec, position=0)  # Progress bar
fails = 0       # Keep track of failed fits

for i, fname in enumerate(config['DATA']['spec_list']):

    # ---------------------------------------------------------------------
    # Read the spectrum and extract the timestamp
    # ---------------------------------------------------------------------
    if not write_analysis:
        postfix = 'Processing file: %s' % fname.split('/')[-1] + '; %i failed' % fails
        pbar.set_postfix_str(postfix)
    spectrum = read_spectrum(fname)
    dtime = spectrum.dtime
    time = datetime.strftime(dtime, '%Y/%m/%d - %H:%M:%S.%f')

    # ---------------------------------------------------------------------
    # Fit the spectrum and read out the parameters
    # ---------------------------------------------------------------------
    # Initiate empty lists
    results = []
    row_all = [fname.split('/')[-1], dtime]  # Summary row

    # --- Loop through fits ---
    for n in range(n_fits):

        # Fit name
        fit_name = analysers[n].name

        # --- 1: Fit the spectrum ---
        results.append(analysers[n].fitSpectrum(spectrum, update_params=retrieval.update_params,
                       use_bounds=retrieval.use_bounds, findEnvelope=retrieval.fix_envelope))

        # --- 2: Read out parameters ---
        row_n = [fname.split('/')[-1], dtime]   # Individual row

        for name, p in results[n].params.items():

            # For gases, use molecular column densities and derive mass
            if p.atm_gas or p.plume_gas:
                val = p.fit_val
                err = p.fit_err
                scd1 = val * p.ref_column
                scd2 = mcm2gm2(scd1, name)
                err1 = err * p.ref_column
                err2 = mcm2gm2(err1, name)
                # Add to row
                row_n += [val, err, scd1, err1, scd2, err2]
                # If it's a target species, also add to summary row
                if p.target:
                    row_all += [val, err, scd1, err1, scd2, err2]
            # For aerosols, use mass column densities and derive molecular fractions
            elif p.plume_aero:
                val = p.fit_val
                err = p.fit_err
                scd2 = val * p.ref_column
                err2 = err * p.ref_column
                scd1 = gm2mcm2(scd2 * p.ref_comp, name)
                err1 = gm2mcm2(err2 * p.ref_comp, name)
                # Add to row
                row_n += [val, err, scd1, err1, scd2, err2]
                # If it's a target species, also add to summary row
                if p.target:
                    row_all += [val, err, scd1, err1, scd2, err2]
            # For everything else, only use the fit value
            else:
                val = p.fit_val
                err = p.fit_err
                # Add to row
                row_n += [val, err]
                # If it's a target species, also add to summary row
                if p.target or 'deff' in name:
                    row_all += [val, err]

        # --- 3: Also write the errors ---
        row_n += [results[n].rmse, results[n].r2]
        row_all += [results[n].rmse, results[n].r2]

        # Assign row to Dataframe for each fit
        df[n].loc[i] = row_n

    # Also record max errors
    row_all += [max([results[j].rmse for j in range(n_fits)]), min([results[j].r2 for j in range(n_fits)])]

    # Assign summary row to Dataframe
    df_sum.loc[i] = row_all

    if not results[0].nerr:
        fails += 1

    # ---------------------------------------------------------------------
    # Update plots
    # ---------------------------------------------------------------------
    if plot_full_spectrum:
        updateSpectrumCanvas(spectrum_canvas, i, spectrum, results, save=save_frames, outdir=outdir)

    if plot_analysis:
        updateAnalysisCanvas(analysis_canvas, i, config, results, df_sum, geometry,
                             save=save_frames, outdir=outdir, scroll=scroll, plotmass=plotmass)
    if write_analysis:
        postfix = []
        postfix.append('R2: %.3f' % df_sum['MIN R2'][i])
        for target in config['TARGET']['targets']:
            postfix.append('%s: %.2f' % (target, df_sum['%s_val' % target][i]))
        pbar.set_postfix_str("; ".join(postfix) + '; %i failed' % fails)

    # ---------------------------------------------------------------------
    # Save dataframes to csv files, full results to pkl file
    # ---------------------------------------------------------------------
    if save_results:
        for n in range(n_fits):
            df[n].to_csv(outnames[n])
        df_sum.to_csv(outnames[-1])

        full_results.append(results)
        outname = outdir + fitname + '_RAW.pkl'
        with open(outname, 'wb') as f:
            pickle.dump(full_results, f)

    pbar.update()

df_all = df.copy()
df_all.append(df_sum)

pbar.close()


# ---------------------------------------------------------------------
# Save all dataframes to single Excel file and save log and INI file
# ---------------------------------------------------------------------
if save_results:
    outname = outnames[-1].replace('_ALL.csv', '.xlsx')
    with pd.ExcelWriter(outname) as writer:
        for n in range(n_fits):
            df[n].to_excel(writer, sheet_name=analysers[n].name)
        df_sum.to_excel(writer, sheet_name='Target')

    logger.info('Analysis finished! Results saved to: %s' % outname)

    # Save a copy of the config file to results directory
    outname = outdir + fitname + '_config.txt'
    with open(config_name) as f:
        lines = f.readlines()
    with open(outname, 'w') as f:
        for line in lines:
            f.write(line)
    
    # Save a copy of the log file to results directory
    outname = outdir + fitname + '_log.txt'
    with open('./plumeIRlog.txt') as f:
        lines = f.readlines()
    with open(outname, 'w') as f:
        for line in lines:
            f.write(line)

    # Save a copy of the reference(s) file to results directory
    for analyser in analysers:
        outname = outdir + fitname + '_%s_reference.pkl' % analyser.name
        with open(outname, 'wb') as f:
            pickle.dump(analyser.reference, f)

    if analyser.type in ['solar', 'emission']:
        outname = outdir + fitname + '_referenceAtm.atm'
        writeAtm(analyser.geometry.atm, outname)

        
else:
    logger.info('Analysis finished! Save mode was off. Run again to save results')

# ---------------------------------------------------------------------
# Plot results
# ---------------------------------------------------------------------
if plot_results:
    plotResults(df_all, config=config, geometry=geometry, targets=targets, ratios=ratios, which=which, use_cmap=use_cmap,
                color_by=color_by, window=window, plot_individual=plot_individual, save=save_results, outdir=outdir)










