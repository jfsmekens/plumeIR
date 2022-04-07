"""
Written by @jfsmekens - Last updated 01/04/2022

This module contains objects to define a spectral fit:

    Parameters - an object containing the environmental parameters for each fit
    Geometry - an object to define the viewing geometry
    Retrieval - an object to define the spectral parameters

"""
import copy
import numpy as np
from collections import OrderedDict
from atmosphere import readAtm, modifyAtm, scaleAtm, resampleAtm
from initialise import readStandard


# ======================================================================================================================
# ========================================= Parameters Collection ======================================================
# ======================================================================================================================

class Parameters(OrderedDict):
    """
    Parameters collection of Parameter objects
    """

    def __init__(self, *args, **kwargs):
        """Initialize the Parameters"""
        self.update(*args, **kwargs)

    def add(self, name, value=0, vary=True, atm_gas=False, plume_gas=False, plume_aero=False, target=False, ref_column=0,
            min=-np.inf, max=np.inf):
        """
        Function to add a parameter to the collection
        :param name: [str] Name of the parameter
        :param value: [float] Value of the initial guess for the parameter
        :param vary: [bool] Should the parameter vary in the forward?
        :param atm_gas: [bool] Is this an atmospheric gas?
        :param plume_gas: [bool] Is this a plume gas?
        :param plume_aero: [bool] Is this a plume particle
        :param target: [bool] Is this parameter part of the target species?
        :param ref_column: [float] Reference column amount for the species (Value acts as a scaling factor for this)
        :param min: [float] Lower bound to considered in bounded retrieval
        :param max: [float] Upper bound to considered in bounded retrieval
        :return: 
        """
        self.__setitem__(name, Parameter(name=name,
                                         value=value,
                                         vary=vary,
                                         atm_gas=atm_gas,
                                         plume_gas=plume_gas,
                                         plume_aero=plume_aero,
                                         target=target,
                                         ref_column=ref_column,
                                         min=min,
                                         max=max))

    def add_many(self, param_list):
        """ Add multiple Parameters to the Parameters object """
        for param in param_list:

            self.__setitem__(param.name, param)

    def extract(self, fit=None, retrieval=None):
        """Extract parameters from a FIT section in the config file"""

        if fit is None:
            raise ValueError("MUST supply a fit dictionary")

        if retrieval is None:
            retrieval = Retrieval()

        std = readStandard()

        # Unpack polynomial parameters
        if 'polydeg' in fit.keys():     # Overwrite if one is given in an individual fit
            if fit['polydeg'] is not None:
                polydeg = int(fit['polydeg'])
            else:
                polydeg = int(retrieval.polydeg)
        else:
            polydeg = int(retrieval.polydeg)
        for deg in range(polydeg):
            coeff = 'poly%i' % deg
            self.add(coeff, value=1.0)

        # Turn single gases in lists with length==1
        if isinstance(fit['atm_gases'], str) or fit['atm_gases'] is None:
            atm_gases = [fit['atm_gases']]
        else:
            atm_gases = fit['atm_gases']

        if isinstance(fit['plume_gases'], str) or fit['plume_gases'] is None:
            plume_gases = [fit['plume_gases']]
        else:
            plume_gases = fit['plume_gases']

        if isinstance(fit['plume_aero'], str) or fit['plume_aero'] is None:
            plume_aero = [fit['plume_aero']]
        else:
            plume_aero = fit['plume_aero']

        if isinstance(fit['targets'], str) or fit['targets'] is None:
            targets = [fit['targets']]
        else:
            targets = fit['targets']

        # Add the atmospheric gas parameters
        for species in atm_gases:
            if species is not None:
                if retrieval.type == 'emission':
                    if fit['fix_atm_gas'] is not None:
                        vary = species not in fit['fix_atm_gas']
                    else:
                        vary = True
                    self.add(species, value=1e-12, atm_gas=True, target=species in targets,
                               min=-np.inf, max=np.inf, vary=vary)
                else:
                    self.add(species, value=1.0, atm_gas=True, target=species in targets,
                               min=0.0, max=np.inf, vary=True)

        # Add the plume gas parameters
        for species in plume_gases:
            if species is not None:
                if species in atm_gases:  # Add a differentiator if gas is in both atm and plume
                    species = species + '_pl'
                self.add(species, value=0.01, plume_gas=True, target=species in targets,
                           min=0.0, max=np.inf, vary=True)

        # Add the aerosol parameters
        for species in plume_aero:
            if species is not None:
                self.add(species, value=0.01, plume_aero=True, target=species in targets,
                           min=0.0, max=np.inf, vary=True)
                self.add(species + '_deff', value=std[species]['size'], vary=retrieval.fit_size_aero,
                           min=std[species]['vmin'], max=std[species]['vmax'])

        # Add the spectral grid parameters
        self.add('fov', value=retrieval.fov, min=0.001, max=0.5, vary=retrieval.fit_fov)
        self.add('nu_shift', value=retrieval.nu_shift, min=-10, max=+10, vary=retrieval.fit_shift)
        self.add('offset', value=retrieval.offset, vary=retrieval.fit_offset)
        self.add('source_temp', value=retrieval.source_temp, min=100, vary=retrieval.fit_source_temp)

        # Add the special parameters for emission fits
        if retrieval.type == 'emission':
            self.add('dt_plume', value=retrieval.dt_plume, vary=retrieval.fit_tplume)
            self.add('dt_prox', value=retrieval.dt_prox, vary=retrieval.fit_tprox)
            self.add('H2O_sc', value=1.0, min=0.0, max=4.0, vary=retrieval.H2O_scaling > 0)


    def update_values(self, new_values):
        """ Update the values of each Parameter in order """
        n = 0
        for name in self:
            if self[name].vary:
                self[name].set(value=new_values[n])
                n += 1

    def valuesDict(self):
        """ Return an ordered dictionary of all parameter values """
        return OrderedDict((p.name, p.value) for p in self.values())

    def atmGasList(self):
        """ Return a list of the names of atmospheric gases """
        return [(p.name) for p in self.values() if p.atm_gas]

    def plumeGasList(self):
        """ Return a list of the names of plume gases """
        return [(p.name) for p in self.values() if p.plume_gas]
    
    def plumeAeroList(self):
        """ Return a list of the names of plume gases """
        return [(p.name) for p in self.values() if p.plume_aero]

    def targetList(self):
        """ Return a list of the names of target gases """
        return [(p.name) for p in self.values() if p.target]

    def fittedValuesDict(self):
        """ Return an ordered dictionary of fitted parameter values """
        return OrderedDict((p.name, p.value) for p in self.values() if p.vary)

    def poptDict(self):
        """ Return a dictionary of the optimised parameters """
        return OrderedDict((p.name, p.fit_val)
                           for p in self.values() if p.vary)
    def valuesList(self):
        """ Return a list of all parameter values """
        return [(p.value) for p in self.values()]

    def fittedValuesList(self):
        """ Return a list of the fitted parameter values """
        return [(p.value) for p in self.values() if p.vary]

    def poptList(self):
        """ Return a list of the optimised parameters """
        return [(p.fit_val) for p in self.values() if p.vary]

    def bounds(self):
        """ Return a list of the low and high bounds """
        return [[(p.min) for p in self.values() if p.vary],
                [(p.max) for p in self.values() if p.vary]]

    def make_copy(self):
        """ Return a deep copy of the Parameters object """
        return copy.deepcopy(self)

    def pretty_print(self, mincolwidth=10, precision=4, cols='optimal', which='gases'):
        """
        Print a tabulated list of parameters that will look good in a terminal
        :param mincolwidth:
        :param precision:
        :param cols:
        :return:
        """
        # Set default column choices
        def_cols = {'all':   ['name', 'value', 'vary', 'plume_gas', 'plume_aero', 'atm_gas', 'target', 'fit_val', 'fit_err', 'ref_column'],
                    'optimal': ['name', 'value', 'vary', 'plume_gas', 'plume_aero', 'atm_gas', 'target', 'ref_column'],
                    'basic': ['name', 'value', 'vary']}

        # Define which parameters to plot
        if which == 'all':
            whichlist = [key for key in self.valuesDict().keys()]
        elif which == 'fitted':
            whichlist = [key for key in self.fittedValuesDict().keys()]
        elif which == 'target':
            whichlist = [key for key in self.targetList()]
        elif which == 'atm':
            whichlist = [key for key in self.atmGasList()]
        elif which == 'plume':
            whichlist = [key for key in self.plumeGasList() + self.plumeAeroList()]
        elif which == 'gases':
            whichlist = [key for key in self.plumeGasList() + self.atmGasList()]

        # Make list of columns
        cols = def_cols[cols]

        colwidth = [mincolwidth] * (len(cols))

        if 'name' in cols:
            i = cols.index('name')
            colwidth[i] = max([len(name) for name in self]) + 2

        if 'value' in cols:
            i = cols.index('value')
            colwidth[i] = max([len(f'{p.value:.{precision}g}')
                               for p in self.values()]) + 2

        if 'vary' in cols:
            i = cols.index('vary')
            colwidth[i] = mincolwidth

        if 'plume_gas' in cols:
            i = cols.index('plume_gas')
            colwidth[i] = max([len(f'{p.plume_gas:.{precision}g}')
                               for p in self.values()]) + 2
        if 'plume_aero' in cols:
            i = cols.index('plume_aero')
            colwidth[i] = max([len(f'{p.plume_gas:.{precision}g}')
                               for p in self.values()]) + 2
        if 'atm_gas' in cols:
            i = cols.index('atm_gas')
            colwidth[i] = max([len(f'{p.atm_gas:.{precision}g}')
                               for p in self.values()]) + 2
        if 'target' in cols:
            i = cols.index('target')
            colwidth[i] = max([len(f'{p.target:.{precision}g}')
                               for p in self.values()]) + 2
        if 'fit_val' in cols:
            i = cols.index('fit_val')
            colwidth[i] = max([len(f'{p.fit_val:.{precision}g}')
                               for p in self.values()]) + 2

        if 'fit_err' in cols:
            i = cols.index('fit_err')
            colwidth[i] = max([len(f'{p.fit_err:.{precision}g}')
                               for p in self.values()]) + 2

        for n, w in enumerate(colwidth):
            if w < mincolwidth:
                colwidth[n] = mincolwidth

        # Generate the string
        title = ''
        for n, c in enumerate(cols):
            title += f'|{c:^{colwidth[n]}}'
        title += '|'

        msg = f'\n{"MODEL PARAMETERS":^{len(title)}}\n{title}\n' + \
              f'{"-"*len(title)}\n'

        for name, p in self.items():
            if name in whichlist:
                d = {'name': f'{p.name}',
                     'value': f'{p.value:.{precision}g}',
                     'ref_column': f'{p.ref_column:.{precision}g}',
                     'fit_val': f'{p.fit_val:.{precision}g}',
                     'fit_err': f'{p.fit_err:.{precision}g}',
                     'vary': f'{p.vary}',
                     'plume_gas': f'{p.plume_gas}',
                     'plume_aero': f'{p.plume_aero}',
                     'atm_gas': f'{p.atm_gas}',
                     'target': f'{p.target}'}

                for col in cols:
                    msg += f'|{d[col]:^{colwidth[cols.index(col)]}}'

                msg += '|\n'

        return msg


# ======================================================================================================================
# ========================================== Parameter Object ==========================================================
# ======================================================================================================================

class Parameter(object):
    """
    Parameter object
    """

    def __init__(self, name, value, vary=True, plume_gas=False, plume_aero=False, atm_gas=False, target=False, ref_column=None,
                 min=-np.inf, max=np.inf):
        """ Initialise the parameter """
        self.name = name
        self.value = value
        self.vary = vary
        self.plume_gas = plume_gas
        self.plume_aero = plume_aero
        self.atm_gas = atm_gas
        self.target = target
        self.ref_column = ref_column
        self.min = min
        self.max = max
        self.fit_val = np.nan
        self.fit_err = np.nan
        self.fit_val_scd = np.nan
        self.fit_err_scd = np.nan

    def set(self, value=None, vary=None, plume_gas=None, plume_aero=None, atm_gas=None, target=None, ref_column=None,
            min=None, max=None, fit_val=None, fit_err=None, fit_val_scd=None, fit_err_scd=None):
        """ Update the attributes of a Parameter """
        if value is not None:
            self.value = value

        if vary is not None:
            self.vary = vary

        if plume_gas is not None:
            self.plume_gas = plume_gas

        if plume_aero is not None:
            self.plume_aero = plume_aero

        if atm_gas is not None:
            self.atm_gas = atm_gas

        if target is not None:
            self.target = target

        if ref_column is not None:
            self.ref_column = ref_column

        if min is not None:
            self.min = min

        if max is not None:
            self.max = max

        if fit_val is not None:
            self.fit_val = fit_val

        if fit_err is not None:
            self.fit_err = fit_err

        if fit_val_scd is not None:
            self.fit_val_scd = fit_val_scd

        if fit_err_scd is not None:
            self.fit_err_scd = fit_err_scd

# ======================================================================================================================
# ========================================== Geometry Object ===========================================================
# ======================================================================================================================

class Geometry(object):
    """
    Geometry object describing the viewinng geometry and environmental conditions for the retrieval
    """

    def __init__(self, type, obs_height=0.0, elev=90.0, obs_temp=None, obs_RH=None,
                 pathlength=0.1, plume_temp=298, plume_pres=1013, plume_height=None, plume_thickness=None,
                 atm_temp=298, atm_pres=1013, atm_path='./atm/mls.atm', sounding=None, layer_model=1):
        """ Initialise the geometry """
        self.type = type
        if type == 'layer':
            self.pathlength = pathlength
            self.plume_thickness = pathlength
            self.atm_temp = atm_temp
            self.atm_pres = atm_pres
            self.plume_temp = plume_temp
            self.plume_pres = plume_pres
            self.obs_height = obs_height
            self.obs_temp = obs_temp
            self.obs_RH = obs_RH
        elif type == 'solar' or type == 'emission':
            self.plume_height = plume_height
            self.plume_thickness = plume_thickness
            self.pathlength = plume_thickness
            self.elev = elev
            self.obs_height = obs_height
            self.obs_temp = obs_temp
            self.obs_RH = obs_RH
            self.atm_path = atm_path
            self.sounding = sounding
            self.atm_temp = atm_temp
            self.atm_pres = atm_pres
            self.plume_temp = plume_temp
            self.plume_pres = plume_pres
            
            # Modify atmospheric profile with user supplied info (sounding, measured P, T, RH)
            if self.sounding is not None:
                atm, atm_path = modifyAtm(atm_path, sounding, outpath=True)
            # Resample over 25 layers
            atm, atm_path = resampleAtm(atm_path, outpath=True)
            # Read in modified (or not) atmosphere
            self.atm_path = atm_path
            self.atm = readAtm(self.atm_path)

            # Derive plume conditions at plume and observer height
            self.plume_temp = np.interp(self.plume_height, self.atm['HGT'], self.atm['TEM'])
            self.plume_pres = np.interp(self.plume_height, self.atm['HGT'], self.atm['PRE'])
            self.atm_temp = np.interp(self.plume_height, self.atm['HGT'], self.atm['TEM'])
            self.atm_pres = np.interp(self.plume_height, self.atm['HGT'], self.atm['PRE'])
            if self.obs_height is not None:
                self.obs_temp = np.interp(self.obs_height, self.atm['HGT'], self.atm['TEM'])
                self.obs_pres = np.interp(self.obs_height, self.atm['HGT'], self.atm['PRE'])
            if self.plume_thickness is None or self.plume_thickness == 0:
                self.plume_thickness = self.pathlength
            
            # Add layer model for emission
            if type == 'emission':
                self.pathlength = plume_thickness
                self.layer_model = layer_model
                if layer_model == 1:
                    self.h1 = plume_height
                    self.h2 = plume_height
                elif layer_model == 2:
                    self.h1 = plume_height - plume_thickness / 2
                    self.h2 = plume_height + plume_thickness / 2 
        else:
            raise ValueError('"%s" is not a valid type. Please choose: "layer" | "solar" | "emission"' % type)


    def set(self, type, plume=None, obs_height=None, elev=None,
                 pathlength=None, plume_height=None, plume_thickness=None):
        """ Update the attributes of a Parameter """
        if type is not None:
            self.type = type

        if plume is not None:
            self.plume = plume

        if obs_height is not None:
            self.obs_height = obs_height

        if elev is not None:
            self.elev = elev

        if pathlength is not None:
            self.pathlength = pathlength

        if plume_height is not None:
            self.plume_height = plume_height

        if plume_thickness is not None:
            self.plume_thickness = plume_thickness


class Retrieval(object):
    """
    Retrieval object which sets the spectral parameters for all fits in the retrieval
    """

    def __init__(self, type='layer',
                 update_params=False,
                 use_bounds=False,
                 plume_aero=None,
                 fit_size_aero=False,
                 use_Babs_aero=True,
                 fit_difference=True,
                 use_bbt=False,
                 bb_drift=False,
                 clear_drift=False,
                 fit_tplume=False,
                 dt_plume=0,
                 fit_tprox=True,
                 dt_prox=0,
                 H2O_scaling=3,
                 subtract_self=True,
                 no_gas=True,
                 fix_envelope=False,
                 polydeg=3,
                 fit_source_temp=False,
                 source_temp=1000,
                 unified=False,
                 fit_fov=True,
                 fov=0.03,
                 fit_shift=True,
                 nu_shift=0,
                 fit_offset=False,
                 offset=0):
        """ Initialise the retrieval """
        self.type = type    # Geometry
        # Linear regression
        self.update_params = update_params
        self.use_bounds = use_bounds
        # Background fitting
        self.fix_envelope = fix_envelope
        self.polydeg = int(polydeg)
        self.fit_source_temp = fit_source_temp
        self.source_temp = float(source_temp)
        self.unified = unified
        # Spectral parameters
        self.fit_fov = fit_fov
        self.fov = float(fov)
        self.fit_shift = fit_shift
        self.nu_shift = float(nu_shift)
        self.fit_offset = fit_offset
        self.offset = float(offset)
        # Aerosols
        self.plume_aero = plume_aero
        self.fit_size_aero = fit_size_aero
        self.use_Babs_aero = use_Babs_aero
        if type == 'layer':
            self.subtract_self = subtract_self
            self.no_gas = no_gas
        elif type == 'emission':
            self.fit_difference = fit_difference
            self.use_bbt = use_bbt
            self.bb_drift = bb_drift
            self.clear_drift = clear_drift
            self.fit_tplume = fit_tplume
            self.dt_plume = float(dt_plume)
            self.fit_tprox = fit_tprox
            self.dt_prox = float(dt_prox)
            self.H2O_scaling = int(H2O_scaling)
