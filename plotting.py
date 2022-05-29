"""
Written by @jfsmekens - Last updated 01/04/2022

This modules contains functions to plot annalysis and results. This includes plotting canvases for real time feedback
    during analysis (meant to serve as future panels in a GUI), and dedicated plots for output dataframes

"""
from datetime import datetime
import numpy as np
import pandas as pd
from scipy.stats import siegelslopes
import os
from glob import glob

import matplotlib
# matplotlib.use("Qt5agg")
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib import gridspec


from constants import *
from unitConversion import *

# Update the default font sizes and line properties
plt.rcParams.update({'font.size': 8.0,
                     'lines.linewidth': 1.0,
                     'axes.prop_cycle': plt.cycler(color=plt.cm.Dark2.colors),
                     'xtick.direction': 'in',
                     'xtick.top': True,
                     'ytick.direction': 'in',
                     'ytick.right': True,
                     'axes.grid': True,
                     'axes.axisbelow': True,
                     'grid.linestyle': '--',
                     'axes.formatter.limits': (-4, 4)})

# Get a formatter for time series plots
formatter = mdates.DateFormatter('%H:%M')
        
# ======================================================================================================================
#                                    Create the canvas for Analysis tab
# ======================================================================================================================
def makeAnalysisCanvas(config):

    """
    This function creates an empty plot canvas for displaying real-time analysis results from plumeIR

    :param config: The configuration dictionary read from the initialseFit function
    :return: plot canvas [axes, lines, legends, title]
    """
    
    # How many fits are there?
    fit_type = config['GEOMETRY']['type']
    n_fits = config['n_fits']

    # What are the target gases?
    targets = config['TARGET']['targets']
    n_targets = len(targets)
    fit_titles = [config['FIT%i' % i]['targets'] for i in range(n_fits + 1) if config['FIT%i' % i]['fit']]
    
    # Which ratios are we plotting?
    ratios = config['TARGET']['ratios']
    n_ratios = len(ratios)
    if n_ratios == 0:
        n_ratios = 1
        ratios = ['N/A:N/A']

    # ---------------------------------------------------------------------
    # Define the layout of the plots
    # ---------------------------------------------------------------------
    # Open the figure
    if config['n_fits'] >= 3 or config['n_targets'] >= 6:
        figsize = [16, 8]
    else:
        figsize = [12, 8]
    fig = plt.figure(figsize=figsize, constrained_layout=True)
    gs0 = gridspec.GridSpec(10, 1)  # Main grid (10 rows)
    gs1 = gridspec.GridSpecFromSubplotSpec(5, n_fits, subplot_spec=gs0[0:5, :], hspace=0.05)    # Fits grid (top 5 rows)
    gs2 = gridspec.GridSpecFromSubplotSpec(1, 1, subplot_spec=gs0[5:7, :])      # Time series grid (1 row)
    gs3 = gridspec.GridSpecFromSubplotSpec(1, n_ratios, subplot_spec=gs0[7:, :])    # Ratios grid (bottom 4 rows)
    
    # Position plots on the grid
    ax1 = []        # Fits
    ax2 = []        # Residuals
    for i in range(n_fits):
        ax1.append(fig.add_subplot(gs1[0:4, i]))    # Fits in top 4/5 of the plot
        ax2.append(fig.add_subplot(gs1[4:5, i]))    # Residuals in bottom 1/5
    ax3 = fig.add_subplot(gs2[:, :])    # Time series (1 long across whole figure)
    ax4 = []        # Ratios
    for i in range(n_ratios):
        ax4.append(fig.add_subplot(gs3[:, i]))
    
    # ---------------------------------------------------------------------
    # Create empty plot lines to update during fit
    # ---------------------------------------------------------------------
    # Create empty line handles
    l1a = []        # Measured
    l1b = []        # Model
    l1c = []        # Background
    for i in range(n_fits):
        line, = ax1[i].plot([], [], c='tab:blue', label='Measured')
        l1a.append(line)
        line, = ax1[i].plot([], [], c='tab:orange', label='Model')
        l1b.append(line)
        line, = ax1[i].plot([], [], ls='--', c='tab:orange', label='Background')
        l1c.append(line)
    l2 = []        # Residuals
    for i in range(n_fits):
        line, = ax2[i].plot([], [], c='k', label='Residual')
        l2.append(line)
    l3 = []        # Time series
    for i in range(n_targets):  # All target species
        line, = ax3.plot([], [], c=plot_colors[targets[i]], label='%s' % pretty_names[targets[i]])
        l3.append(line)
    line, = ax3.plot([], [], c=plot_colors['R2'], label='%s' % pretty_names['R2'])  # Add one for goodness of fit
    l3.append(line)

    # Create empty scatter plot handles
    s4 = []         # All ratios
    s5 = []         # Scrolling ratios
    s6 = []         # Latest ratio
    l4 = []         # Linear regression
    l5 = []         # Intercept
    for i in range(n_ratios):
        scatter, = ax4[i].plot([], [], ls='', marker='.', c='tab:grey', zorder=0, alpha=0.3)
        s4.append(scatter)
        scatter, = ax4[i].plot([], [], ls='', marker='.', c='tab:red', zorder=1)
        s5.append(scatter)
        scatter, = ax4[i].plot([], [], ls='', marker='.', c='b', markersize=10, zorder=2)
        s6.append(scatter)
        line, = ax4[i].plot([], [], ls='--', c='k', label='%s = wait...' % pretty_ratios[ratios[i]], zorder=0)
        l4.append(line)
        line = ax4[i].axhline(np.nan, ls='--', label='Intercept...', zorder=0)
        l5.append(line)
    
    # ---------------------------------------------------------------------
    # Titles, axes labels and legends
    # ---------------------------------------------------------------------
    # Spectral plots
    # Display title
    for i in range(n_fits):
        ax1[i].set_title(fit_titles[i], fontweight='bold', fontstyle='italic')

    # Label axis only on the left one
    ax1[0].set(ylabel='Spectrum')
    ax2[0].set(ylabel='Residual')
    leg1 = []   # Fits legend
    leg2 = []   # Residuals legend
    for i in range(n_fits):
        plt.setp(ax1[i].get_xticklabels(), visible=False)   # Remove labels on inside of plots
        ax2[i].set(xlabel='Wavenumber [$cm^{-1}$]')         # Label wave axis on bottom plot only
        leg1.append(ax1[i].legend(loc='best'))
        leg2.append(ax2[i].legend(loc='best'))
    
    # Label Time Series plot
    ax3.set(ylabel='Normalized Target')
    leg3 = ax3.legend(loc='upper center', ncol=n_targets + 1)
    
    # Ratio plots
    leg4 = []
    for i in range(n_ratios):
        leg4.append(ax4[i].legend(bbox_to_anchor=[0, -0.1], loc='upper left'))
    ax4[0].set(ylabel='Ratios')     # Label left plot only

    # Tighten the layout and add titles and logo
    gs0.tight_layout(fig, rect=[0, 0, 1, 0.96])
    title = fig.suptitle('Waiting for spectrum...', fontsize=12, fontweight='bold', va='top')   # Main title
    plt.text(0.97, 0.99, logotext, va='top', ha='right',
             transform=fig.transFigure, fontdict=logofont)                                      # Logo
    plt.text(0.97, 0.97, versiontext, va='bottom', ha='left',
             transform=fig.transFigure, fontdict=versionfont)                                   # Version
    plt.text(0.01, 0.99, fit_type.upper(), va='top', ha='left',
             transform=fig.transFigure, fontsize=12, fontstyle='italic')                        # Fit type
    plt.show(block=False)

    axes = [ax1, ax2, ax3, ax4]
    lines = [l1a, l1b, l1c, l2, l3, l4, l5, s4, s5, s6]
    legends = [leg1, leg2, leg3, leg4]
    
    return axes, lines, legends, title


# ======================================================================================================================
#                                        Update the fitting plots
# ======================================================================================================================
def updateAnalysisCanvas(canvas, step, config, results, dataframe, geometry,
                         save=False, outdir='./plots/', scroll=100, plotmass=False):
    """
    This function updates the Analysis plot canvas created by makeAnalysisCanvas with results as they come in

    :param canvas: The plot canvas with all lines and handles
    :param step: [int] Step in the iteration process
    :param config: The config file read by the initialiseFit function
    :param results: An array with all FitResults objects up until the latest step
    :param dataframe: The dataframe with the summarised results
    :param geometry: Geometry object for the fit
    :param save: [bool] set to True to save each new plot as a PNG frame
    :param outdir: [str] Pathto the directory to save the frames
    :param scroll: [int] Limit the time series to this number of results at a time
                            This also affects the ratio plots, as only these results will be considered for the
                            ratio plots and the ratio is calculated using a moving window
    :param plotmass: [bool] set to True if you want to plot all ratios as mass ratios. If False, only the ratios
                            involving particulates will be mass ratios
    :param which_H2SO4:
    :return:
    """

    if scroll is None:
        scroll = 9999
    else:
        scroll = int(scroll)

    if step <= scroll:
        start = 0
    else:
        start = step - scroll

    error = 'MIN R2'

    # Unpack the canvas
    axes, lines, legends, title = canvas
    ax1, ax2, ax3, ax4 = axes
    l1a, l1b, l1c, l2, l3, l4, l5, s4, s5, s6 = lines
    leg1, leg2, leg3, leg4 = legends

    # Update title
    if results[-1].dtime is not None:
        title.set_text('SPECTRUM %i of %i: %s' % (step + 1, config['n_spec'],
                                                  datetime.strftime(results[-1].dtime, '%Y/%m/%d - %H:%M:%S')))
    else:
        title.set_text('SPECTRUM %i of %i: no timestamp' % (step + 1, config['n_spec']))

    # Define axes labels based on what type of spectrum it is
    if all([results[n].spec_type == 'sbm' for n in range(config['n_fits'])]):
        ax1[0].set(ylabel='Single Beam')
    elif all([results[n].spec_type == 'bbt' for n in range(config['n_fits'])]):
        ax1[0].set(ylabel='Brightness Temperature [K]')
    elif all([results[n].spec_type == 'rad' for n in range(config['n_fits'])]):
        ax1[0].set(ylabel='Radiance [$mW / (m^2.sr.cm^{-1})$]')
    elif all([results[n].spec_type == 'raddiff' for n in range(config['n_fits'])]):
        ax1[0].set(ylabel='Radiance Difference\n[$mW / (m^2.sr.cm^{-1})$]')
    elif all([results[n].spec_type == 'bbtdiff' for n in range(config['n_fits'])]):
        ax1[0].set(ylabel='Brightness Temperature Difference [K]')
    else:
        ax1[0].set(ylabel='Spectrum')

    for n in range(config['n_fits']):
        if results[n].resid_type == 'Absolute':
            ax2[0].set(ylabel='Residual')
        elif results[n].resid_type == 'Percentage':
            ax2[0].set(ylabel='Residual [%]')

    # Add all axes to a list to scale them all in one go later
    all_axes = []
    for ax in axes:
        if isinstance(ax, list):
            for subax in ax:
                all_axes.append(subax)
        else:
            all_axes.append(ax)

    # Plot spectrum and fits
    for n in range(config['n_fits']):
        l1a[n].set_data(results[n].grid, results[n].spec)
        l1b[n].set_data(results[n].grid, results[n].model)
        l1c[n].set_data(results[n].grid, results[n].bkg)
        l2[n].set_data(results[n].grid, results[n].res)
        leg2[n].get_texts()[0].set_text('$R^2$= %.3f' % results[n].r2)

    # Time series
    if config['n_targets'] > 0:
        for n in range(config['n_targets']):
            xdata = np.arange(start, step + 1, 1).astype(int)
            ydata = dataframe[config['TARGET']['targets'][n] + '_val'][start:step + 1].astype(float)
            l3[n].set_data(xdata, ydata / np.abs(ydata).max())
        xdata = np.arange(start, step + 1, 1).astype(int)
        ydata = dataframe[error][start:step + 1].astype(float)
        l3[n + 1].set_data(xdata, ydata)

    # Ratios
    if config['n_ratios'] > 0:
        for n in range(config['n_ratios']):

            if config['TARGET']['ratios'][n] != 'N/A:N/A':  # Only proceed if there is a ratio to plot
                xgas = config['TARGET']['ratios'][n].split(':')[1]      # X coordinate species
                ygas = config['TARGET']['ratios'][n].split(':')[0]      # Y coordinate species
                force_mass = \
                    not plotmass and any([gas in ['ASH', 'WATER', 'H2SO4'] for gas in config['TARGET']['ratios'][n].split(':')])
                if plotmass or force_mass:
                    legtype = '[$g.m^{-2}$]'
                    xdata_all = dataframe[xgas + '_scd [g.m^-2]'][0:step + 1].astype(float)
                    ydata_all = dataframe[ygas + '_scd [g.m^-2]'][0:step + 1].astype(float)
                    xdata = xdata_all[start:step + 1]
                    ydata = ydata_all[start:step + 1]
                else:
                    legtype = '[$molec.cm^{-2}$]'
                    xdata_all = dataframe[xgas + '_scd [molec.cm^-2]'][0:step + 1].astype(float)
                    ydata_all = dataframe[ygas + '_scd [molec.cm^-2]'][0:step + 1].astype(float)
                    xdata = xdata_all[start:step + 1]
                    ydata = ydata_all[start:step + 1]
                s4[n].set_data(xdata_all, ydata_all)        # All data points
                s5[n].set_data(xdata, ydata)                # Only the ones in the scroll window
                s6[n].set_data(xdata[step], ydata[step])    # Latest data

                # Linear fit for ratio
                if step > 10:   # Only if there are at least 10 points
                    try:
                        # Calculate linear fit
                        idx = np.isfinite(xdata) & np.isfinite(ydata)   # Remove NaN values from failed fits
                        m, p = siegelslopes(ydata[idx], x=xdata[idx])
                        x = np.linspace(xdata_all.min(), xdata_all.max())
                        y = m * x + p
                        idx = [np.nanmin(ydata) <= value <= np.nanmax(ydata) for value in y]

                        # If atmospheric gases in the target gases, calculate background concentration
                        if plotmass:
                            input = 'g/m2'
                        else:
                            input = 'molec/cm2'

                        legtxt1 = '%s = %.3g' % (pretty_ratios[config['TARGET']['ratios'][n]], m)

                        if geometry.type == 'layer':
                            atm_conc = scd2ppm(p, geometry.pathlength, pres=geometry.atm_pres,
                                               temp=geometry.atm_temp, input=input, species=ygas)
                            if ygas == 'H2O':
                                RH = ppm2RH(atm_conc, pres=geometry.atm_pres, temp=geometry.atm_temp)
                                legtxt2 = 'bkg RH: %i %%' % (RH)
                            else:
                                legtxt2 = 'bkg %s: %i ppm' % (pretty_names[ygas], atm_conc)
                        elif geometry.type == 'solar':
                            if ygas in ['plume gases']:
                                atm_conc = scd2ppm(p, geometry.plume_thickness, pres=geometry.plume_pres,
                                                   temp=geometry.plume_temp, input=input, species=ygas)
                                legtxt2 = 'bkg %s: %i ppm' % (pretty_names[ygas], atm_conc)
                            else:
                                legtxt2 = 'bkg %s: %.3g' % (pretty_names[ygas], p)
                        elif geometry.type == 'emission':
                            if ygas in ['plume gases']:
                                atm_conc = scd2ppm(p, geometry.plume_thickness, pres=geometry.plume_pres,
                                                   temp=geometry.plume_temp, input=input, species=ygas)
                                legtxt2 = 'bkg %s: %i ppm' % (pretty_names[ygas], atm_conc)
                            else:
                                legtxt2 = 'bkg %s: %.3g' % (pretty_names[ygas], p)

                        # Update lines
                        l4[n].set_data(x[idx], y[idx])    # Linear regression
                        l5[n].set_ydata(p)      # Intercept

                        # Update legend
                        leg4[n].get_texts()[0].set_text(legtxt1)
                        leg4[n].get_texts()[1].set_text(legtxt2)


                    except:
                        # Remove line
                        l4[n].set_data([], [])

                        # Update legend
                        legtxt = '%s = too much spread' % (pretty_ratios[config['TARGET']['ratios'][n]])
                        leg4[n].get_texts()[0].set_text(legtxt)

                # Are we plotting mass or moles
                ax4[n].set_title(legtype, fontsize=8, loc='right')

        # Reset all axes limits
        for ax in all_axes:
            ax.relim()
            ax.autoscale_view()

    # Save frame as a PNG
    if save:
        plt.savefig(outdir + 'plots/analysis_%04i.png' % step)

    plt.pause(0.01)
    plt.show(block=False)

# ======================================================================================================================
#                                    Create the canvas for Spectrum tab
# ======================================================================================================================
def makeSpectrumCanvas(config, analysers=None):
    """
    This functions makes an empty canvas to plot the full spectrum and overlay the fits on top

    :param config: The config file read by the initialiseFit function
    :param analysers: List of Analyser objects
    :return: plot canvas [axes, lines, legends, title]
    """

    n_fits = config['n_fits']

    # Create the figure
    fig = plt.figure(figsize=[12, 8])
    gs = gridspec.GridSpec(5, 1, hspace=0.05)
    ax1 = fig.add_subplot(gs[0:4, :])
    ax2 = fig.add_subplot(gs[4:, :])

    # Create empty lines for later plotting
    l1a, = ax1.plot([], [], c='tab:grey', label='Full Spectrum', zorder=0)   # Main spectrum (greyed out)
    l1b_dummy, =ax1.plot([], [], c='tab:blue', label='Measured')   # Measured Spectrum within each fit
    l1b = []
    for i in range(n_fits):
        line, = ax1.plot([], [], c='tab:blue')
        l1b.append(line)
    l1c_dummy, = ax1.plot([], [], c='tab:orange', label='Model')  # Model spectrum within each fit
    l1c = []
    for i in range(n_fits):
        line, = ax1.plot([], [], c='tab:orange')
        l1c.append(line)
    l1d_dummy, = ax1.plot([], [], ls='--', c='tab:orange', label='Background')  # Model spectrum within each fit
    l1d = []
    for i in range(n_fits):
        line, = ax1.plot([], [], c='tab:orange')
        l1d.append(line)
    l2_dummy, = ax2.plot([], [], c='k', label='Residual') # Residual from fits
    l2 = []
    for i in range(n_fits):
        line, = ax2.plot([], [], c='k')
        l2.append(line)

    # ---------------------------------------------------------------------
    # Shaded areas for fit windows
    # ---------------------------------------------------------------------
    if analysers is not None:
        colors = ['palegreen', 'lightskyblue', 'lightpink', 'thistle', 'palegoldenrod']
        for i in range(n_fits):
            fit_window = analysers[i].fit_window
            label = '%s: %s %i-%i $cm^{-1}$' % (analysers[i].name, analysers[i].params.targetList(),
                                            analysers[i].fit_window[0], analysers[i].fit_window[1])
            ax1.axvspan(fit_window[0], fit_window[1], color=colors[i], zorder=0, label=label)

    # ---------------------------------------------------------------------
    # Titles, axes labels and legends
    # ---------------------------------------------------------------------
    title = fig.suptitle('Waiting for spectrum...', fontsize=12, fontweight='bold')

    # Label axes
    ax1.set(ylabel='Spectrum')
    ax1.label_outer()
    ax2.set(ylabel='Residual', xlabel='Wavenumber [$cm^{-1}$]')

    leg1 = ax1.legend(fontsize=8)
    leg2 = ax2.legend(fontsize=8)

    axes = [ax1, ax2]
    lines = [l1a, l1b, l1c, l1d, l2]
    legends = [leg1, leg2]

    gs.tight_layout(fig, rect=[0, 0, 1, 0.94])
    plt.show(block=False)

    return axes, lines, legends, title

# ======================================================================================================================
#                                    Update the canvas for Spectrum tab
# ======================================================================================================================
def updateSpectrumCanvas(canvas, step, spectrum, results=None, full_spectrum=True, save=False, outdir='./plots/'):

    if results is not None:
        n_fits = len(results)

    # Unpack the canvas
    axes, lines, legends, title = canvas
    ax1, ax2 = axes
    l1a, l1b, l1c, l1d, l2 = lines
    leg1, leg2 = legends

    # Add all axes to a list to scale them all in one go later
    all_axes = []
    for ax in axes:
        if isinstance(ax, list):
            for subax in ax:
                all_axes.append(subax)
        else:
            all_axes.append(ax)

    # Update title
    title.set_text('SPECTRUM %i: %s' % (step + 1, datetime.strftime(spectrum.dtime, '%Y/%m/%d - %H:%M:%S')))

    # Update with spectrum
    l2_dummy, = ax2.plot([], [], c='tab:grey', zorder=0)
    if not full_spectrum:
        lo_bound = np.min([results[i].grid.min() for i in range(n_fits)])
        hi_bound = np.max([results[i].grid.max() for i in range(n_fits)])
        idx = [i for i, wn in enumerate(spectrum.spectrum[0]) if lo_bound <= wn <= hi_bound]
    else:
        idx = [i for i, wn in enumerate(spectrum.spectrum[0])]

    l1a.set_data(spectrum.spectrum[0][idx], spectrum.spectrum[1][idx])
    l2_dummy.set_data(spectrum.spectrum[0][idx], spectrum.spectrum[1][idx] * 0)

    if results is None:
        full_spectrum == True
        l1a.set_color('tab:blue')

    # Update labels with proper units:
    if spectrum.spec_type == 'sbm':
        ax1.set(ylabel='Single Beam')
    elif spectrum.spec_type == 'bbt':
        ax1.set(ylabel='Brightness Temperature [K]')

    # Also plot the results of the various fits if passed on
    if results is not None:
        for i in range(n_fits):
            l1b[i].set_data(results[i].grid, results[i].spec)
            l1c[i].set_data(results[i].grid, results[i].model)
            l1d[i].set_data(results[i].grid, results[i].bkg)
            l2[i].set_data(results[i].grid, results[i].res)

    # Reset all axes limits
    for ax in all_axes:
        ax.relim()
        ax.autoscale_view()

    if save:
        plt.savefig(outdir + 'plots/fullspec_%04i.png' % step)

    plt.pause(0.01)
    plt.show(block=False)


# ======================================================================================================================
#                                               Plot FIT Results
# ======================================================================================================================
def plotResults(dataframe, config=None, geometry=None, targets='all', ratios='all', which='all', errorbar=True,
                use_cmap=False, color_by='time', window=30, plot_individual=False, save=False, outdir='./plots/',
                plotmass=False):
    """
    This function plots the results of the analysis extracted from a dataframe

    :param dataframe: [str|dataframe|list] The dataframe(s) to be plotted.
                    If [str]: The path to the dataframe file (.csv or .xlsx)
                    If [dataframe]: The dataframe
                    If [list]: A list of dataframes
    :param config: The configuration file used to produce the results. If none is given, will search for a config file
                    in the same directory as the results.
    :param geometry: Geometry object
    :param targets: [str|list] Which targets should be plotted.
                            'all': to plot all targets
                            ['gas', 'gas']: list of gases to be plotted
    :param ratios: [str|list] Which ratios should be plotted.
                            'all': to plot all ratios
                            ['gas:gas', 'gas:gas']: list of ratios to be plotted
    :param which: [str] 'ts' to plot time series only
                        'ratios' to plot ratios only
                         'all' to plot both
    :param errorbar: [bool] If True, will plot data points with errorbars on time series and ratio plots
                            If False, will plot time series as lines with shaded errors and no error on ratio plots
    :param use_cmap: [bool] If True will use a color map to display 3rd quantity in ratio plots (defined by color_by)
    :param color_by: [str] What quantity to use for color mapping. Default is 'time'
                        'time': Local time of acquisition on the spectrum
                        'gas': Any value of a species present in the dataframe (case insensitive)
                        'gas:gas': Any ratio between two species present in the dataframe
    :param window: [int]  Size of the moving window used to calculate ratio over time
    :param plot_individual: [bool] Set to True to plot individual fits (only use is supplying all dataframes)
    :param save: [bool] Set to True to save the plots to disk as .png files
    :param outdir: [str] Directory to save the results
    :return: nothing
    """

    # Read config from local file if none is provided
    if config is None and isinstance(dataframe, str):
        from initialise import readConfig
        config_name = glob('/'.join(dataframe.split('/')[0:-1]) + '/*config.txt')
        config = readConfig(config_name[-1])

    # Load results
    if isinstance(dataframe, str):
        if dataframe.split('.')[-1] == 'xlsx':      # Excel file with individual sheets for each fit
            file = pd.ExcelFile(dataframe)
            df = pd.read_excel(file, 'Target')
            indiv_dfs = {}
            for sheet in file.sheet_names:
                if 'FIT' in sheet:
                    indiv_dfs[sheet] = pd.read_excel(file, sheet)
        elif dataframe.split('.')[-1] == 'csv':     # .csv file with only one fit or a summary of targets
            df = pd.read_csv(dataframe)
    elif isinstance(dataframe, pd.DataFrame):       # Only one dataframe is provided (assumed to be the summary one)
        df = dataframe
        indiv_dfs = None
    elif isinstance(dataframe, list) and config is not None:               # Multiple datasets provided (assumes the last one is the summary)
        indiv_dfs = {}
        for i, fit in enumerate([key for key in config.keys() if 'FIT' in key and config[key]['fit']]):
             indiv_dfs[fit] = dataframe[i]
        df = dataframe[-1]

    # If no configuration is found, just extract the values needed from the dataframe
    if config is None or 'TARGET' not in config.keys():
        # Define targets
        targets_from_df = []
        for col in df.columns:
            if '_val' in col and not '_deff' in col:
                targets_from_df.append(col.replace('_val', ''))
        # Define ratios
        possible_ratios = []
        for a in targets_from_df:
            for b in targets_from_df:
                possible_ratios.append('%s:%s' % (a, b))
        ratios_from_df = [x for x in interesting_ratios if x in possible_ratios]
        print('Target and Ratios extracted from dataframe\n%s\n%s' % (targets_from_df, ratios_from_df))
    else:
        targets_from_df = config['TARGET']['targets']
        ratios_from_df = config['TARGET']['ratios']
        print('Target and Ratios read from config file\n%s\n%s' % (targets_from_df, ratios_from_df))

    # Are we using all ratios and targets?
    if targets == 'all':
        targets = targets_from_df
    if ratios == 'all':
        ratios = ratios_from_df

    # Extract key variables for background concentration calculation from Geometry object
    if geometry is not None:
        pathlength = geometry.pathlength
        atm_temp = geometry.atm_temp
        atm_pres = geometry.atm_pres
    elif config is not None:
        from parameters import Geometry
        geometry = Geometry(**config['GEOMETRY'])
        pathlength = geometry.pathlength
        atm_temp = geometry.atm_temp
        atm_pres = geometry.atm_pres
    else:           # If all else fails, just hard code the variables
        pathlength = 0.5
        atm_temp = 298
        atm_pres = 1013
        print('!WARNING: No geometry found. Derived background concentrations likely to be wrong!')

    # Option to color scatter plots by a third quantity
    if isinstance(use_cmap, bool):
        if use_cmap:
            scat_color = mdates.date2num(pd.to_datetime(df['Time'], utc=True))
            cmap = plt.cm.get_cmap('gnuplot')
        else:
            scat_color = 'tab:red'
            cmap = None
    elif isinstance(use_cmap, str):
        try:
            cmap = plt.cm.get_cmap(use_cmap)
        except:
            print('%s is not a valid colormap. Using default spring instead' % use_cmap)
            cmap = plt.cm.get_cmap('gnuplot')
        use_cmap = True

    # What are we coloring by
    if use_cmap:
        if color_by.lower() == 'time' or color_by is None:
            scat_color = mdates.date2num(pd.to_datetime(df['Time'], utc=True))
        elif ':' not in color_by and color_by.upper() not in targets:
            raise ValueError('Cannot color by %s. Information not contained in the dataframe' % color_by.upper())
        elif ':' in color_by and color_by.upper() not in ratios:
            raise ValueError('Cannot color by %s. Information not contained in the dataframe' % color_by.upper())
        elif ':' not in color_by:
            match = [key for key in plot_cmaps.keys() if color_by.lower() in key.lower()]
            if len(match) != 0:
                cmap = plt.cm.get_cmap(plot_cmaps[match[0]])
            match = [('%s_scd' % color_by).lower() in column.lower() and 'error' not in column.lower() for
                     column in df.columns]
            if len(match) != 0:
                if color_by.lower() in ['h2so4', 'ash'] or plotmass:
                    color_by2 = df.columns[match][1]
                else:
                    color_by2 = df.columns[match][0]
                scat_color = df[color_by2]
        elif ':' in color_by:
            ygas, xgas = color_by.upper().split(':')
            xmatch = [('%s_scd' % xgas).lower() in column.lower() and 'error' not in column.lower() for
                     column in df.columns]
            if len(xmatch) != 0:
                if any([gas.lower() in ['h2so4', 'ash'] for gas in [xgas, ygas]]) or plotmass:
                    xcolumn = df.columns[xmatch][1]
                else:
                    xcolumn = df.columns[xmatch][0]
            ymatch = [('%s_scd' % ygas).lower() in column.lower() and 'error' not in column.lower() for
                      column in df.columns]
            if len(ymatch) != 0:
                if any([gas.lower() in ['h2so4', 'ash'] for gas in [xgas, ygas]]) or plotmass:
                    ycolumn = df.columns[ymatch][1]
                else:
                    ycolumn = df.columns[ymatch][0]
            if len(xmatch) != 0 and len(ymatch) != 0:
                # Calculate linear fit
                ydata = df[ycolumn].astype(float)
                xdata = df[xcolumn].astype(float)
                idx = np.isfinite(xdata) & np.isfinite(ydata)  # Remove NaN values from failed fits
                m, p = siegelslopes(ydata[idx], x=xdata[idx])
                scat_color = (df[ycolumn].astype(float) - p) / df[xcolumn].astype(float)
            match = [key for key in plot_cmaps.keys() if color_by.split(':')[0].lower() in key.lower()]
            if len(match) != 0:
                cmap = plt.cm.get_cmap(plot_cmaps[match[0]])
                color_by2 = ':'.join([pretty_names[x.upper()] for x in color_by.split(':')])

    # Number of datapoints
    n = len(df['Time'])

    # ---------------------------------------------------------------------
    # Plot Time Series
    # ---------------------------------------------------------------------
    if len(targets) != 0 and (which == 'ts' or which == 'all'):

        # Make Plot canvas
        n_targets = len(targets)
        fig = plt.figure(figsize=[16, 8])
        fig.suptitle('ALL TARGETS')
        gs = gridspec.GridSpec(n_targets + 1, 1)

        for i in range(n_targets):
            col = targets[i]
            ax = fig.add_subplot(gs[i, :])
            if col in ['H2SO4', 'ASH', 'WATER']:
                legtype = 'mass'
                data = df[col + '_scd [g.m^-2]'].astype(float)
                err = df[col + '_scd error [g.m^-2]'].astype(float)
                ylabel = '[$g.m^{-2}$]'
            else:
                legtype = 'molar'
                data = df[col + '_scd [molec.cm^-2]'].astype(float)
                err = df[col + '_scd error [molec.cm^-2]'].astype(float)
                ylabel = '[$molec.cm^{-2}$]'
            if errorbar:
                ax.errorbar(df['Time'], data, yerr=err, c=plot_colors[col], label=pretty_names[col],
                             fmt='.', mec='k', mew=0.3, capsize=3, markersize=8, ls='-', lw=1)
            else:
                ax.plot(df['Time'], data, c=plot_colors[col], label=pretty_names[col])
                ax.fill_between(df['Time'], data - err, data + err, color=plot_colors[col], alpha=0.5, zorder=0)
            if col + '_deff [um]' in df.columns:
                twin_ax = ax.twinx()
                twin_ax.grid(visible=False)
                data_size = df[col + '_deff [um]'].astype(float)
                twin_ax.plot(df['Time'], data_size, '--', c=plot_colors[col], label=pretty_names[col] + ' size')
                twin_ax.set(ylabel=r'$d^{eff}$ [$/um$]', yscale='log')
                twin_ax.set_ylim(bottom=np.nanmin(data_size) / 100)
                twin_ax.legend(loc='lower right')

            ax.legend(loc='upper left')
            ax.set(xlabel='Time', ylabel=ylabel)
            ax.xaxis.set_major_formatter(formatter)
            ax.label_outer()

        # Add error
        col = 'MAX RMSE'
        ax = fig.add_subplot(gs[i+1, :])
        data = df[col].astype(float)
        ax.plot(df['Time'], data, c='k', label='Error')
        ax.legend()
        ax.set(xlabel='Time', ylabel='RMSE')
        ax.xaxis.set_major_formatter(formatter)
        ax.label_outer()

        fig.tight_layout(h_pad=0)

        if save:
            plt.savefig(outdir + 'TARGET_timeseries.png')

    if plot_individual and indiv_dfs is not None:
        for fitname in indiv_dfs.keys():

            i_df = indiv_dfs[fitname]

            # Make Plot canvas
            all_species = []
            for key in ['atm_gases', 'plume_gases', 'plume_aero']:
                if isinstance(config[fitname][key], str):
                    all_species.append(config[fitname][key])
                elif isinstance(config[fitname][key], list):
                    for item in config[fitname][key]:
                        all_species.append(item)

            n_targets = len(all_species)
            fig = plt.figure(figsize=[16, 8])
            fig.suptitle(fitname)
            gs = gridspec.GridSpec(n_targets + 1, 1)

            for i in range(n_targets):
                col = all_species[i]
                ax = fig.add_subplot(gs[i, :])
                if col in ['H2SO4', 'ASH', 'WATER']:
                    data = i_df[col + '_scd [g.m^-2]'].astype(float)
                    err = i_df[col + '_scd error [g.m^-2]'].astype(float)
                    ylabel = '[$g.m^{-2}$]'
                else:
                    data = i_df[col + '_scd [molec.cm^-2]'].astype(float)
                    err = i_df[col + '_scd error [molec.cm^-2]'].astype(float)
                    ylabel = '[$molec.cm^{-2}$]'
                if errorbar:
                    ax.errorbar(i_df['Time'], data, yerr=err, c=plot_colors[col], label=pretty_names[col],
                                fmt='.', mec='k', mew=0.3, capsize=3, markersize=8, ls='-', lw=1)
                else:
                    ax.plot(i_df['Time'], data, c=plot_colors[col], label=pretty_names[col])
                    ax.fill_between(i_df['Time'], data - err, data + err, color=plot_colors[col], alpha=0.5, zorder=0)
                if col + '_deff [um]' in i_df.columns:
                    twin_ax = ax.twinx()
                    data_size = i_df[col + '_deff [um]'].astype(float)
                    twin_ax.plot(i_df['Time'], data_size, '--', c=plot_colors[col], label=pretty_names[col] + ' size')
                    twin_ax.set(ylabel=r'$d^{eff}$ [$/um$]', yscale='log')
                    twin_ax.set_ylim(bottom=np.nanmin(data_size) / 100)
                    twin_ax.legend()

                ax.legend()
                ax.set(xlabel='Time', ylabel=ylabel)
                ax.xaxis.set_major_formatter(formatter)
                ax.label_outer()

            # Add error
            col = 'RMSE'
            ax = fig.add_subplot(gs[i+1, :])
            data = i_df[col].astype(float)
            ax.plot(i_df['Time'], data, c='k', label='Error')
            ax.legend()
            ax.set(xlabel='Time', ylabel='RMSE')
            ax.xaxis.set_major_formatter(formatter)
            ax.label_outer()

            fig.tight_layout(h_pad=0)

            if save:
                plt.savefig(outdir + '%s_timeseries.png' % fitname)

    # ---------------------------------------------------------------------
    # Plot ratios
    # ---------------------------------------------------------------------
    if len(ratios) != 0 and (which == 'ratios' or which == 'all'):

        # Make Ratios figure
        n_ratios = len(ratios)
        fig = plt.figure(figsize=[16, 8])
        if n_ratios == 1:
            gs0 = gridspec.GridSpec(1, 1)
        elif n_ratios == 2:
            gs0 = gridspec.GridSpec(1, 2)
        elif n_ratios == 3:
            gs0 = gridspec.GridSpec(1, 3)
        elif n_ratios == 4:
            gs0 = gridspec.GridSpec(2, 2)
        elif n_ratios == 5:
            gs0 = gridspec.GridSpec(2, 3)
        elif n_ratios == 6:
            gs0 = gridspec.GridSpec(2, 3)
        elif n_ratios == 7:
            gs0 = gridspec.GridSpec(2, 4)
        elif n_ratios == 8:
            gs0 = gridspec.GridSpec(2, 4)
        elif n_ratios == 9:
            gs0 = gridspec.GridSpec(2, 5)
        elif n_ratios == 10:
            gs0 = gridspec.GridSpec(2, 5)

        for i in range(n_ratios):

            gs1 = gridspec.GridSpecFromSubplotSpec(3, 1, subplot_spec=gs0[i])
            ratio_ax = fig.add_subplot(gs1[0:2, :])
            ts_ax = fig.add_subplot(gs1[2, :])

            ygas, xgas = ratios[i].split(':')
            if xgas in ['ASH', 'WATER'] or ygas in ['ASH', 'WATER']:
                legtype = '[$g.m^{-2}$]'
                input = 'g/m2'
                xdata = df[xgas + '_scd [g.m^-2]'].astype(float)
                ydata = df[ygas + '_scd [g.m^-2]'].astype(float)
                xerr = df[xgas + '_scd error [g.m^-2]'].astype(float)
                yerr = df[ygas + '_scd error [g.m^-2]'].astype(float)
            else:
                legtype = '[$molec.cm^{-2}$]'
                input = 'molec/cm2'
                xdata = df[xgas + '_scd [molec.cm^-2]'].astype(float)
                ydata = df[ygas+ '_scd [molec.cm^-2]'].astype(float)
                xerr = df[xgas + '_scd error [molec.cm^-2]'].astype(float)
                yerr = df[ygas + '_scd error [molec.cm^-2]'].astype(float)

            # Calculate linear fit
            idx = np.isfinite(xdata) & np.isfinite(ydata)
            m, p = siegelslopes(ydata[idx], x=xdata[idx])
            x = np.linspace(xdata.min(), xdata.max())
            y = m * x + p
            legtxt1 = '%s = %.3g' % (pretty_ratios[ratios[i]], m)

            # Plot scatter and fit
            scat = ratio_ax.scatter(xdata, ydata, s=10, c=scat_color, cmap=cmap, edgecolors='k', linewidths=0.3)
            if errorbar:
                if use_cmap:
                    if color_by is None or color_by.lower() == 'time':
                        ecolor = 'k'
                    elif ':' in color_by:
                        ecolor = plot_colors[color_by.split(':')[0].upper()]
                    else:
                        ecolor = plot_colors[color_by.upper()]
                else:
                    ecolor = 'tab:red'
                ratio_ax.errorbar(xdata, ydata, xerr=xerr, yerr=yerr, fmt='none', elinewidth=0.5,
                                 ecolor=ecolor, capsize=2, capthick=0.5, zorder=0)
            ratio_ax.plot(x, y, '--k', label=legtxt1)

            # Extrapolate intercept value
            if geometry.type == 'layer':
                atm_conc = scd2ppm(p, pathlength, pres=atm_pres,
                                   temp=atm_temp, input=input, species=ygas)
                if ygas == 'H2O':
                    RH = ppm2RH(atm_conc, pres=atm_pres, temp=atm_temp)
                    legtxt2 = 'bkg RH: %i %%' % (RH)
                else:
                    legtxt2 = 'bkg %s: %i ppm' % (pretty_names[ygas], atm_conc)
            elif geometry.type == 'solar':
                if ygas in ['plume gases']:
                    atm_conc = scd2ppm(p, geometry.plume_thickness, pres=geometry.plume_pres,
                                       temp=geometry.plume_temp, input=input, species=ygas)
                    legtxt2 = 'bkg %s: %i ppm' % (pretty_names[ygas], atm_conc)
                else:
                    legtxt2 = 'bkg %s: %.3g' % (pretty_names[ygas], p)
            elif geometry.type == 'emission':
                if ygas in ['plume gases']:
                    atm_conc = scd2ppm(p, geometry.plume_thickness, pres=geometry.plume_pres,
                                       temp=geometry.plume_temp, input=input, species=ygas)
                    legtxt2 = 'bkg %s: %i ppm' % (pretty_names[ygas], atm_conc)
                else:
                    legtxt2 = 'bkg %s: %.3g' % (pretty_names[ygas], p)
            ratio_ax.axhline(p, label=legtxt2, ls='--')

            # Label axes
            ratio_ax.set_title(legtype, fontsize=8, loc='right')
            ratio_ax.set(xlabel=pretty_names[xgas], ylabel=pretty_names[ygas])
            ratio_ax.legend()

            # Plot ratio over time
            ratio_ts = np.empty(len(xdata))
            for j in range(window, len(xdata) - window, 1):
                xdata_w = xdata[j - window : j + window]
                ydata_w = ydata[j - window : j + window]
                idx = np.isfinite(xdata_w) & np.isfinite(ydata_w)
                m, p = siegelslopes(ydata_w[idx], x=xdata_w[idx])
                ratio_ts[j] = m

            if use_cmap:
                ts_ax.plot(df['Time'][window:-window], ratio_ts[window:-window], c='k',
                                label='Ratio over time')
                ts_ax.scatter(df['Time'][window:-window], ratio_ts[window:-window], s=15,
                                c=scat_color[window:-window], cmap=cmap)
            else:
                ts_ax.plot(df['Time'][window:-window], ratio_ts[window:-window], c='tab:red',
                                label='Ratio over time')
            ts_ax.axvspan(df['Time'][0], df['Time'][window+1], label='Moving window: %i spectra' % window,
                               color="tab:grey", zorder=0, alpha=0.5)
            ts_ax.axvspan(df['Time'][n-window-1], df['Time'][n-1],
                               color="tab:grey", zorder=0, alpha=0.5)
            ts_ax.set(xlabel='Time', ylabel=pretty_ratios[ratios[i]])
            ts_ax.xaxis.set_major_formatter(formatter)

        if use_cmap:
            gs0.tight_layout(fig, rect=[0, 0, 1, 0.94])
            # Place colorbar(s)
            cbar_ax = fig.add_axes([0.2, 0.94, 0.6, 0.02])
            cbar = fig.colorbar(scat, cax=cbar_ax, orientation='horizontal')
            if color_by.lower() == 'time':
                label = 'Local Time'
                loc = mdates.AutoDateLocator()
                cbar.ax.xaxis.set_major_locator(loc)
                cbar.ax.xaxis.set_major_formatter(mdates.ConciseDateFormatter(loc))
            elif ':' in color_by:
                label = color_by2
            elif ':' not in color_by:
                label = pretty_names[color_by.upper()] + pretty_labels[color_by2]
            cbar.ax.xaxis.set_ticks_position('top')
            cbar.ax.xaxis.set_label_position('top')
            cbar.set_label(label)
        else:
            fig.tight_layout()

        if save:
            plt.savefig(outdir + 'TARGET_ratios.png')

    plt.show(block=False)
