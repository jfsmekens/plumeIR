"""
Written by @jfsmekens - Last updated 01/04/2022

This module contains a series of constants associated with gas species, as well as
    lists of colors and descriptors used to format plots

"""
# Known extensions for MIDAC files
midac_extensions = ['spc', 'sbm', 'ifg', 'sb', 'abs']

# Invalid Extensions: do not include those when searching for spectra in a directory
invalid_extensions = ['pkl', 'csv', 'txt', 'ini', 'dat', 'xls', 'xlsx', 'doc', 'docx', 'jpg', 'png']

# Gas species in RFM + those extracted from NIST
rfm_gases = ['H2O', 'CO2', 'SO2', 'CO', 'OCS', 'NO', 'O3', 'N2O', 'NO2', 'NH3', 'HNO3', 'CH4', 'O2', 'HCl', 'HF', 'HBr',
             'HI', 'H2S', 'N2', 'ClO', 'SF6', 'SO3']
special_gases = ['SiF4']
possible_gases = rfm_gases + special_gases

possible_aeros = ['H2SO4', 'ASH', 'WATER']

# Interesting Ratios: If any combination of 2 targets makes one of those ratios, it will be plotted
interesting_ratios = ['CO2:SO2', 'H2O:SO2', 'H2O:CO2', 'SO2:CO', 'SO2:HCl', 'SO2:HF', 'SO2:H2S', 'SO2:SO3', 'CO2:CO',
                      'OCS:CO', 'SO2:SiF4', 'HCl:HF', 'SO2:H2SO4', 'SO2:SO4', 'H2SO4:ASH', 'H2O:WATER', 'N/A:N/A']

# Continuum Gases: gases in this list are treated differently when creating a reference
#       (include gases with continuum and/or line mixing issues)
continuum_gases = []
atm_temps = [200, 400]
gas_temps = [200, 800]

# Available Apodization functions for the ILS
apod_types = ['Boxcar', 'Uniform', 'Triangular', 'Blackman-Harris', 'Happ-Genzel', 'Hamming', 'Lorenz', 'Gaussian',
                  'NB weak', 'NB medium', 'NB strong', 'Cosine']

# Molecular weights of all gases
weights = {'H2O': 18.01528,
           'CO2': 44.01,
           'O3': 48,
           'N2O': 44.013,
           'CH4': 16.04,
           'O2': 15.999,
           'SO2': 64.066,
           'SO3': 80.06,
           'HCl': 36.458,
           'HF': 20.01,
           'CO': 28.01,
           'OCS': 60.0751,
           'NH3': 17.031,
           'SiF4': 104.0791,
           'H2S': 34.1,
           'H2SO4': 98.079,
           'ASH': 65.0,
           'WATER': 18.01528,
           'ICE': 18.01528}

# Plot Colors: A dedicated color for each target species
plot_colors = {'H2O_sc': 'lightblue',
               'H2O': 'tab:blue',
               'CO2': 'tab:orange',
               'O3': 'tab:pink',
               'N2O': 'gold',
               'CH4': 'darkviolet',
               'SO2': 'tab:red',
               'SO3': 'tab:orange',
               'CO': 'maroon',
               'OCS': 'goldenrod',
               'HCl': 'cyan',
               'HF': 'magenta',
               'NH3': 'yellow',
               'SiF4': 'purple',
               'H2S': 'olivedrab',
               'H2SO4': 'tab:green',
               'ASH': 'tab:grey',
               'WATER': 'deepskyblue',
               'source_temp': 'r',
               'gas_temp': 'orange',
               'atm_temp': 'skyblue',
               'gasE_temp': 'darkorange',
               'E_frac': 'k',
               'fov': 'k',
               'max_opd': 'k',
               'R2': 'k',
               'RMSE': 'k',
               'N/A': 'w',
               'meas': 'k',
               'model': 'r',
               'bkg': 'r',
               'res': 'k',
               'scat_all': 'tab:grey',
               'scat_scroll': 'tab:red',
               'scat_last': 'b',
               'regress': 'k',
               'intercept': 'darkcyan',
               'confidence': 'k'}
dark_colors = {'H2O_sc': 'lightblue',
               'H2O': 'tab:blue',
               'CO2': 'tab:orange',
               'O3': 'tab:pink',
               'N2O': 'gold',
               'CH4': 'darkviolet',
               'SO2': 'tab:red',
               'CO': 'maroon',
               'OCS': 'goldenrod',
               'HCl': 'cyan',
               'HF': 'magenta',
               'NH3': 'yellow',
               'SiF4': 'purple',
               'H2S': 'olivedrab',
               'H2SO4': 'tab:green',
               'ASH': 'tab:grey',
               'WATER': 'deepskyblue',
               'source_temp': 'r',
               'gas_temp': 'orange',
               'atm_temp': 'skyblue',
               'gasE_temp': 'darkorange',
               'fov': 'w',
               'max_opd': 'w',
               'R2': 'w',
               'RMSE': 'w',
               'N/A': 'k',
               'meas': 'w',
               'model': 'r',
               'bkg': 'r',
               'res': 'w',
               'scat_all': 'w',
               'scat_scroll': 'tab:pink',
               'scat_last': 'dodgerblue',
               'regress': 'w',
               'intercept': 'tab:cyan',
               'confidence': 'w'}

# Plot Color Maps: A dedicated colormap for each species
plot_cmaps = {'H2O': 'Blues',
              'CO2': 'Oranges',
              'O3': 'RdPu',
              'SO2': 'Reds',
              'CO': 'YlOrBr',
              'HCl': 'GnBu',
              'HF': 'PuRd',
              'NH3': 'YlOrBr',
              'SiF4': 'Purples',
              'H2S': 'YlOrBr',
              'H2SO4': 'Greens',
              'ASH': 'Greys',
              'WATER': 'Blues',
              'R2': 'Greys',
              'RMSE': 'Greys',
              'dt_prox': 'Oranges',
              'dt_plume': 'Reds'}

# Pretty name: Mathtext for each species to use in labels
pretty_names = {'H2O': '$H_2O$',
                'SO2': '$SO_2$',
                'SO3': '$SO_3$',
                'CO2': '$CO_2$',
                'NH3': '$NH_3$',
                'O3': '$O_3$',
                'N2O': '$N_2O$',
                'CH4': '$CH_4$',
                'CO': '$CO$',
                'OCS': '$OCS$',
                'HCl': '$HCl$',
                'HF': '$HF$',
                'SiF4': '$SiF_4$',
                'H2S': '$H_2S$',
                'SO4': '${SO_4}^{2-}$',
                'H2SO4': '${SO_4}^{2-}$ $aerosol$',
                'ASH': '$ash$',
                'WATER': '$H_2O$ $aerosol$',
                'ICE': '$ice$',
                'time': 'Local Time',
                'R2': '$R^2$',
                'RMSE': 'RMS error',
                'source_temp': '$T_{source}$',
                'gas_temp': '$T_{gas}$',
                'atm_temp': '$T_{atm}$',
                'gasE_temp': '$T_{gas}(E)$',
                'E_frac': r'$X{\epsilon}$',
                'fov': '$FOV$',
                'max_opd': '$OPD_{max}$',
                'N/A': '$n/a$'}

# Pretty Ratios: Use the math text in pretty_names to create pretty ratios for labels
pretty_ratios = {}
for ratio in interesting_ratios:
    yname, xname = ratio.split(':')
    pretty_ratios[ratio] = ':'.join([pretty_names[yname], pretty_names[xname]])

# Pretty Units: Mathtext with the units of SCD in molar or mass
pretty_labels = {'molar': ' SCD [$molec \cdot cm^{-2}$]',
                 'mass': ' SCD [$g \cdot m^{-2}$]'}

# Logo: The plumeIR_dev logo, version and fonts to display them
logotext = 'plumeIR'
logofont = {'family': 'Palatino',
            'color':  'tab:red',
            'weight': 'bold',
            'style': 'italic',
            'size': 18}
versiontext = 'v2.0'
versionfont = {'family': 'Palatino',
               'color':  'tab:red',
               'weight': 'normal',
               'style': 'italic',
               'size': 10}

# Primary windows
primary_windows = {'SO2': [2400, 2550],
                   'HCl': [2600, 2900],
                   'HF': [4000, 4100],
                   'CO2': [2020, 2150],
                   'SiF4': [1010, 1040]}

# Secondary windows
secondary_windows = {'SO2': [1080, 1120],
                     'HCl': [5730.0, 5780.0],
                     'HF': [4000, 4100],
                     'CO2': [2020, 2150]}
# Microwindows
micro_windows = {'SO2': [2400, 2550],
                 'HCl': [[2727.0, 2728.5], [2775.0, 2776.50], [2818.75, 2820.35], [2820.75, 2822.35], [2843.0, 2844.4],
                         [2903.35, 2904.85], [2923.0, 2924.50], [2925.0, 2926.75], [2942.0, 2943.5], [2960.3, 2961.825],
                         [2962.3, 2964.0], [2995.0, 2996.5]],
                 'HF': [4000, 4100],
                 'CO2': [2020, 2150]}

# Unit notations
units = {'rad': '$mW$ / ($m^{2} \cdot sr \cdot cm^{-1}$)',
         'bbt': '$K$',
         'opt': '$a.u.$',
         'wn': '$cm^{-1}$',
         'um': '$\mu$$m$',
         'nm': '$nm$',
         'molec.cm^-2': '$molec \cdot cm^{-2}$',
         'ppmm': '$ppm \cdot m$',
         'g.m^-2': '$g \cdot m^{-2}$',
         'ppmv': '$ppm$',
         'relh': '$%$',
         'N_density': '$cm^{-3}',
         'N_density_SI': '$m^{-3}',
         'g.cm^-3': '$g \cdot cm^{-3}$',
         'g.m^-3': '$g \cdot m^{-3}$'}



