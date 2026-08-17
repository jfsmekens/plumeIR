"""
Written by @jfsmekens - Last updated 01/04/2022

This modules contains functions to plot annalysis and results. This includes plotting canvases for real time feedback
    during analysis (meant to serve as future panels in a GUI), and dedicated plots for output dataframes

"""
from datetime import datetime
import numpy as np
import pandas as pd
from scipy.stats import siegelslopes, theilslopes
from scipy import odr
import os
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib import gridspec

from constants import *
from unitConversion import *

# Update the default font sizes and line properties
plt.rcParams.update({'font.size': 8.0,
                     'lines.linewidth': 1.0,
                     'xtick.direction': 'in',
                     'xtick.top': True,
                     'ytick.direction': 'in',
                     'ytick.right': True,
                     'axes.grid': True,
                     'axes.axisbelow': True,
                     'grid.linestyle': '--',
                     'grid.alpha': 0.5,
                     'axes.formatter.limits': (-4, 4)})

# Get a formatter for time series plots
formatter = mdates.DateFormatter('%H:%M')
        
# ======================================================================================================================
#                                    Create the canvas for Analysis tab
# ======================================================================================================================
def makeAnalysisCanvas(config, dark_mode=False):

    """
    This function creates an empty plot canvas for displaying real-time analysis results from plumeIR_dev

    :param config: The configuration dictionary read from the initialseFit function
    :return: plot canvas [axes, lines, legends, title]
    """

    if dark_mode:
        plt.style.use('dark_background')
        plt.style.use({
            'axes.facecolor': '#303030',
            'figure.facecolor': '#1A1A1A',
            'figure.edgecolor': '#1A1A1A',
            'savefig.facecolor': '#1A1A1A',
            'savefig.edgecolor': '#1A1A1A',
        })
        plt.rcParams.update({'axes.prop_cycle': plt.cycler(color=plt.cm.Dark2.colors)})
        c = dark_colors
    else:
        c = plot_colors

    # How many fits are there?
    fit_type = config['GEOMETRY']['type']
    n_fits = config['n_fits']

    # What are the target gases?
    targets = config['TARGET']['targets']
    n_targets = len(targets)
    fit_titles = [config[fit]['targets'] for fit in [key for key in config.keys() if 'FIT' in key] if config[fit]['fit']]
    for i in range(len(fit_titles)):
        title = fit_titles[i]
        if isinstance(title, str):
            fit_titles[i] = pretty_names[title]
        elif isinstance(title, list):
            fit_titles[i] = ' | '.join([pretty_names[x] for x in title])
    
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
    if config['n_fits'] >= 3 or config['n_ratios'] >= 6:
        figsize = [16, 8]
    elif config['n_fits'] == 1 and config['n_ratios'] <= 3:
        figsize = [8, 8]
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
        line, = ax1[i].plot([], [], c=c['meas'])
        l1a.append(line)
        line, = ax1[i].plot([], [], c=c['model'])
        l1b.append(line)
        line, = ax1[i].plot([], [], c=c['bkg'], ls='--')
        l1c.append(line)
    l2 = []        # Residuals
    for i in range(n_fits):
        line, = ax2[i].plot([], [], c=c['res'], label='Residual')
        l2.append(line)
    l3 = []        # Time series
    for i in range(n_targets):  # All target species
        line, = ax3.plot([], [], c=c[targets[i]], label='%s' % pretty_names[targets[i]])
        l3.append(line)

    # Create empty scatter plot handles
    s4 = []         # All ratios
    e4 = []
    s5 = []         # Scrolling ratios
    e5 = []
    s6 = []         # Latest ratio
    e6 = []
    l4 = []         # Linear regression
    l5 = []         # Intercept
    sh = []         # Confidence interval

    for i in range(n_ratios):
        scatter, = ax4[i].plot([], [], ls='', marker='.', c=c['scat_all'], zorder=0, alpha=0.3)
        s4.append(scatter)
        err = ax4[i].errorbar([], [], xerr=[], yerr=[], fmt='none', color=c['scat_all'], elinewidth=0.5, capsize=2, capthick=0.5, alpha=0.3)
        e4.append(err)
        scatter, = ax4[i].plot([], [], ls='', marker='.', c=c['scat_scroll'], zorder=1)
        s5.append(scatter)
        err = ax4[i].errorbar([], [], xerr=[], yerr=[], fmt='none', color=c['scat_scroll'], elinewidth=0.5, capsize=2, capthick=0.5)
        e5.append(err)
        scatter, = ax4[i].plot([], [], ls='', marker='.', c=c['scat_last'], markersize=10, zorder=2)
        s6.append(scatter)
        err = ax4[i].errorbar([], [], xerr=[], yerr=[], fmt='none', color=c['scat_last'], elinewidth=0.5, capsize=2, capthick=0.5)
        e6.append(err)
        line, = ax4[i].plot([], [], ls='--', c=c['regress'], label='%s = wait...' % pretty_ratios[ratios[i]], zorder=0)
        l4.append(line)
        line = ax4[i].axhline(np.nan, c=c['intercept'], ls='--', label='Intercept...', zorder=0)
        l5.append(line)
        shape = ax4[i].fill_between([], y1=[], y2=[], fc=c['confidence'], alpha=0.2, zorder=0)
        sh.append(shape)
    
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
        leg1.append(ax1[i].legend(title='Waiting for data...', loc='lower center'))
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
    lines = [l1a, l1b, l1c, l2, l3, l4, l5, sh, s4, s5, s6, e4, e5, e6]
    legends = [leg1, leg2, leg3, leg4]
    
    return axes, lines, legends, title


# ======================================================================================================================
#                                        Update the fitting plots
# ======================================================================================================================
def updateAnalysisCanvas(canvas, step, config, results, dataframe, geometry, save=False, outdir='./plots/',
                          scroll=100, plot_mass=False, show_error=False, fit_error=False, regress='siegel'):
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

    # Define scrolling window
    if scroll is None:
        scroll = 200   # More than 200 at a time is visually not very pleasing
    else:
        scroll = int(scroll)

    # Define the start of the scroll based on where we are in the sequence
    if step <= scroll:
        start = 0
    else:
        start = step - scroll

    # Unpack the canvas
    axes, lines, legends, title = canvas
    ax1, ax2, ax3, ax4 = axes
    l1a, l1b, l1c, l2, l3, l4, l5, sh, s4, s5, s6, e4, e5, e6 = lines
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
        ax1[0].set(ylabel='Brightness Temperature [%s]' % units['bbt'])
    elif all([results[n].spec_type == 'rad' for n in range(config['n_fits'])]):
        ax1[0].set(ylabel='Radiance [%s]' % units['rad'])
    elif all([results[n].spec_type == 'raddiff' for n in range(config['n_fits'])]):
        ax1[0].set(ylabel='Radiance Difference\n[%s]' % units['rad'])
    elif all([results[n].spec_type == 'bbtdiff' for n in range(config['n_fits'])]):
        ax1[0].set(ylabel='Brightness Temperature Difference\n[%s]' % units['bbt'])
    else:
        ax1[0].set(ylabel='Spectrum')

    # Label the residual axes
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
        # l1c[n].set_data(results[n].grid, results[n].bkg)
        l2[n].set_data(results[n].grid, results[n].res)
        leg1[n].set_title('%s: %.3f | %s: %.2f' % (pretty_names['fov'], results[n].params['fov'].fit_val,
                                                   pretty_names['max_opd'], results[n].params['max_opd'].fit_val))
        leg2[n].get_texts()[0].set_text('$R^2$= %.3f' % results[n].r2)

    # Time series
    if config['n_targets'] > 0:
        for n in range(config['n_targets']):
            xdata = np.arange(start, step + 1, 1).astype(int)
            if config['TARGET']['targets'][n] not in possible_gases + possible_aeros:
                ydata = dataframe[config['TARGET']['targets'][n]][start:step + 1].astype(float)
            else:
                ydata = dataframe[config['TARGET']['targets'][n] + '_val'][start:step + 1].astype(float)
            l3[n].set_data(xdata, ydata / np.abs(ydata).max())

    # Ratios
    if config['n_ratios'] > 0:
        for n in range(config['n_ratios']):

            if config['TARGET']['ratios'][n] != 'N/A:N/A':  # Only proceed if there is a ratio to plot
                xgas = config['TARGET']['ratios'][n].split(':')[1]      # X coordinate species
                ygas = config['TARGET']['ratios'][n].split(':')[0]      # Y coordinate species
                if plot_mass or any([gas == 'ASH' for gas in config['TARGET']['ratios'][n].split(':')]):
                    legtype = 'g.m^-2'
                    xdata_all = dataframe[xgas + '_scd [g.m^-2]'][0:step + 1].astype(float)
                    ydata_all = dataframe[ygas + '_scd [g.m^-2]'][0:step + 1].astype(float)
                    xdata = xdata_all[start:step + 1]
                    ydata = ydata_all[start:step + 1]
                    if fit_error:
                        xerr_all = dataframe[xgas + '_scd error [g.m^-2]'][0:step + 1].astype(float)
                        yerr_all = dataframe[ygas + '_scd error [g.m^-2]'][0:step + 1].astype(float)
                        xerr = xerr_all[start:step + 1]
                        yerr = yerr_all[start:step + 1]
                else:
                    legtype = 'molec.cm^-2'
                    xdata_all = dataframe[xgas + '_scd [molec.cm^-2]'][0:step + 1].astype(float)
                    ydata_all = dataframe[ygas + '_scd [molec.cm^-2]'][0:step + 1].astype(float)
                    xdata = xdata_all[start:step + 1]
                    ydata = ydata_all[start:step + 1]
                    if fit_error:
                        xerr_all = dataframe[xgas + '_scd error [molec.cm^-2]'][0:step + 1].astype(float)
                        yerr_all = dataframe[ygas + '_scd error [molec.cm^-2]'][0:step + 1].astype(float)
                        xerr = xerr_all[start:step + 1]
                        yerr = yerr_all[start:step + 1]

                # Update scatter coordinates
                s4[n].set_data(xdata_all, ydata_all)        # All data points
                s5[n].set_data(xdata, ydata)                # Only the ones in the scroll window
                s6[n].set_data(xdata[step], ydata[step])    # Latest data

                # Update errorbars?
                if show_error:
                    e4[n].remove()  # All data points
                    e4[n] = ax4[n].errorbar(xdata_all, ydata_all, xerr=xerr_all, yerr=yerr_all, fmt='none',
                                            color=e4[n][2][0].get_color(), elinewidth=0.5, capsize=2, capthick=0.5)
                    e5[n].remove()  # Only the ones in the scroll window
                    e5[n] = ax4[n].errorbar(xdata, ydata, xerr=xerr, yerr=yerr, fmt='none',
                                            color=e5[n][2][0].get_color(), elinewidth=0.5, capsize=2, capthick=0.5)
                    e6[n].remove()  # Latest data
                    e6[n] = ax4[n].errorbar(xdata[step], ydata[step], xerr=xerr[step], yerr=yerr[step], fmt='none',
                                            color=e6[n][2][0].get_color(), elinewidth=0.5, capsize=2, capthick=0.5)
                # Linear fit for ratio
                if step > 2:   # Only if there are at least 3 points
                    try:
                        idx = np.isfinite(xdata) & np.isfinite(ydata)   # Remove NaN values from failed fits

                        if regress == 'siegel':
                            popt = siegelslopes(ydata[idx], x=xdata[idx])
                            perr = [0, 0]

                        elif regress == 'theil':
                            m, p, mlo, mhi = theilslopes(ydata[idx], x=xdata[idx], method='joint')
                            popt = [m, p]
                            perr = [mhi - m, 0]

                        elif regress == 'odr':

                            beta0 = siegelslopes(ydata[idx], x=xdata[idx])   # First guess with Siegel

                            # Define dataset (with or without errors)
                            if fit_error:
                                data = odr.RealData(x=xdata[idx], y=ydata[idx], sx=xerr[idx], sy=yerr[idx])
                            else:
                                data = odr.RealData(x=xdata[idx], y=ydata[idx])

                            # Run ODR
                            model = odr.unilinear
                            myodr = odr.ODR(data, model, beta0=beta0)
                            out = myodr.run()
                            popt = out.beta
                            perr = out.sd_beta

                        x = np.linspace(xdata_all.min(), xdata_all.max())
                        y = popt[0] * x + popt[1]
                        yhi = (popt[0] + 2 * perr[0]) * x + (popt[1] + 2 * perr[1])
                        ylo = (popt[0] - 2 * perr[0]) * x + (popt[1] - 2 * perr[1])

                        # If atmospheric gases in the target gases, calculate background concentration
                        if plot_mass:
                            input = 'g/m2'
                        else:
                            input = 'molec/cm2'

                        legtxt1 = '%s = %.3g $\pm$ %.2g' % (pretty_ratios[config['TARGET']['ratios'][n]], popt[0], perr[0])
                        legtxt2 = 'Intercept = %.3g' % popt[1]

                        # Update lines
                        l4[n].set_data(x, y)        # Linear regression
                        sh[n].remove()
                        sh[n] = ax4[n].fill_between(x, y1=ylo, y2=yhi, fc=sh[n].get_fc(), alpha=0.1, zorder=0)
                        l5[n].set_ydata(popt[1])    # Intercept

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
                ax4[n].set_title('[%s]' % units[legtype], fontsize=8, loc='right')

    # Reset all axes limits
    for ax in all_axes:
        ax.relim()
        ax.autoscale_view()
    # Make sure the residual axis always has units
    for n in range(len(ax2)):
        ax2[n].set(xlim=ax1[n].get_xlim())

    # Save frame as a PNG
    if save:
        plt.savefig(outdir + 'plots/analysis_%04i.png' % step, dpi=600)

    plt.pause(0.01)
    plt.show(block=False)

