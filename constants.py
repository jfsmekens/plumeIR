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
rfm_species = ['H2O', 'CO2', 'SO2', 'CO', 'NO', 'O3', 'N2O', 'NO2', 'NH3', 'HNO3', 'CH4', 'O2', 'HCl', 'HF', 'HBr',
               'HI', 'H2S', 'N2', 'ClO', 'SF6']
special_species = ['SiF4']
possible_species = rfm_species + special_species

# Interesting Ratios: If any combination of 2 targets makes one of those ratios, it will be plotted
interesting_ratios = ['CO2:SO2', 'H2O:SO2', 'H2O:CO2', 'CO2:CO', 'SO2:HCl', 'SO2:HF',
                      'SO2:SiF4', 'HCl:HF', 'SO2:H2SO4', 'ASH:H2SO4', 'N/A:N/A']

# Continuum Gases: gases in this list are treated differently when creating a reference
#       (inlcude gases with continuum and/or line mixing issues)
continuum_gases = ['H2O']

# Molecular weights of all gases
weights = {'H2O': 18.01528,
           'CO2': 44.01,
           'O3': 48,
           'N2O': 44.013,
           'CH4': 16.04,
           'O2': 15.999,
           'SO2': 64.066,
           'HCl': 36.458,
           'HF': 20.01,
           'CO': 28.01,
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
               'H2O_pl': 'tab:blue',
               'CO2_pl': 'tab:orange',
               'SO2': 'tab:red',
               'CO': 'maroon',
               'HCl': 'cyan',
               'HF': 'magenta',
               'NH3': 'yellow',
               'SiF4': 'purple',
               'H2S': 'olivedrab',
               'H2SO4': 'tab:green',
               'ASH': 'tab:grey',
               'WATER': 'deepskyblue',
               'R2': 'k',
               'RMSE': 'k',
               'N/A': 'w'}

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
              'RMSE': 'Greys'}

# Pretty name: Mathtext for each species to use in labels
pretty_names = {'H2O': '$H_2O$',
                'SO2': '$SO_2$',
                'CO2': '$CO_2$',
                'NH3': '$NH_3$',
                'O3': '$O_3$',
                'N2O': '$N_2O$',
                'CH4': '$CH_4$',
                'CO': '$CO$',
                'HCl': '$HCl$',
                'HF': '$HF$',
                'NH3': '$NH_3$',
                'SiF4': '$SiF_4$',
                'H2S': '$H_2S$',
                'H2SO4': '$H_2SO_4$ aerosol',
                'ASH': '$Ash$',
                'WATER': '$H_2O$ aerosol',
                'ICE': '$Ice$',
                'time': 'Local Time',
                'R2': '$R^2$',
                'RMSE': 'RMS error',
                'N/A': 'n/a'}

# Pretty Ratios: Use the math text in pretty_names to create pretty ratios for labels
pretty_ratios = {}
for ratio in interesting_ratios:
    yname, xname = ratio.split(':')
    pretty_ratios[ratio] = ':'.join([pretty_names[yname], pretty_names[xname]])

# Pretty Units: Mathtext with the units of SCD in molar or mass
pretty_labels = {'molar': ' SCD [$molec.cm^{-2}$]',
                 'mass': ' SCD [$g.m^{-2}$]'}

# Logo: The plumeIR logo, version and fonts to display them
logotext = 'plumeIR'
logofont = {'family': 'Palatino',
            'color':  'darkred',
            'weight': 'bold',
            'style': 'italic',
            'size': 18}
versiontext = 'v0.1'
versionfont = {'family': 'Palatino',
               'color':  'darkred',
               'weight': 'normal',
               'style': 'italic',
               'size': 10}

