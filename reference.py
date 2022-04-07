import logging
from collections import OrderedDict
from initialise import readConfig, readStandard
from unitConversion import *

logger = logging.getLogger(__name__)

class Reference(OrderedDict):

# ======================================================================================================================
#                                               Generate Reference
# ======================================================================================================================
    def __init__(self, params, geometry):
        """
        """

        # # Read in Standard column amounts for cross-sections
        std = readStandard()

        # Create empty structure
        self = {}
        self['wave'] = self.model_grid

        logger.info('Generating reference cross-sections for %s - type "%s"' % (self.name.upper(), self.type.upper()))

        # ---------------------------------------------------------------------
        # 1: Homogeneous layer geometry
        # ---------------------------------------------------------------------
        if self.type == 'layer':

            # Create empty structures
            self['atm'] = {}
            self['plume'] = {}
            self['atm']['ref_column'] = {}
            self['plume']['ref_column'] = {}

            # ----- 1: Atmospheric gases -----
            logger.info('Atmospheric gases...')

            for i, gas in enumerate(self.params.atmGasList()):
                # Assign reference column amount and find concentration based on pathlength
                ref_conc = std[gas]['conc']
                self['atm']['ref_column'][gas] = ppm2scd(ref_conc, self.geometry.pathlength,
                                                        temp=self.geometry.atm_temp, pres=self.geometry.atm_pres)

                # Write RFM driver and run
                writeRFMdrv(self.start_wave, self.end_wave, self.n_per_wave, layer=True,
                            pathlength=self.geometry.pathlength,
                            gas=gas, conc={gas: ref_conc}, temp=self.geometry.atm_temp, pres=self.geometry.atm_pres,
                            out="OPT")
                run('./RFM/source/rfm', stdout=DEVNULL, check=True)

                # Read Optical Depth
                self['atm'][gas] = readRFM('./RFM/output/opt%05d.out' % (self.geometry.pathlength * 1e3))['data']

            # ----- 2: Plume gases -----
            logger.info('Plume gases...')

            for i, gas in enumerate(self.params.plumeGasList()):

                # If gas is both atm and plume, remove suffix
                if '_pl' in gas: gas = gas.strip('_pl')

                # Assign reference column amount and find concentration based on pathlength
                ref_conc = std[gas]['conc']
                self['plume']['ref_column'][gas] = ppm2scd(ref_conc, self.geometry.pathlength,
                                                          temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)

                # Handle SiF4 outside of RFM
                if any([gas.lower() == x for x in ['sif4']]):
                    self['plume'][gas] = makeGasXSC(self.start_wave, self.end_wave, self.n_per_wave, conc=ref_conc,
                                                   species=gas)['Bext'] * self.geometry.pathlength * 1e3

                # Handle all other gases with RFM
                else:
                    # Write RFM driver and run
                    writeRFMdrv(self.start_wave, self.end_wave, self.n_per_wave, layer=True,
                                pathlength=self.geometry.pathlength,
                                gas=gas, conc={gas: ref_conc}, temp=self.geometry.plume_temp,
                                pres=self.geometry.plume_pres, out="OPT")
                    run('./RFM/source/rfm', stdout=DEVNULL)

                    # Read Optical Depth
                    self['plume'][gas] = readRFM('./RFM/output/opt%05d.out' % (self.geometry.pathlength * 1e3))['data']

                # --- 3: Plume aerosols ---
                logger.info('Plume aerosols...')

                for aero in self.params.plumeAeroList():

                    # Get size parameters from standard file
                    size = std[aero]['size']
                    sigma = std[aero]['sigma']
                    mass_conc = std[aero]['conc']

                    # Assign reference column amount
                    self['plume']['ref_column'][aero] = std[aero]['conc'] * self.geometry.pathlength * 1e3

                    # Is it a PSD?
                    if sigma is None:
                        Bext = \
                            makeAerosolXSC(self.start_wave, self.end_wave, self.n_per_wave, size, mass_conc,
                                           species=aero)[
                                'Bext']
                    else:
                        Bext = \
                            makeAerosolXSC(self.start_wave, self.end_wave, self.n_per_wave, size, mass_conc,
                                           psd=True,
                                           sigma=sigma, species=aero)[
                                'Bext']

                    # Generate cross-section
                    self['plume'][aero] = Bext * self.geometry.pathlength * 1e3


        # ---------------------------------------------------------------------
        # 2: Solar occultation geometry
        # ---------------------------------------------------------------------
        elif self.type == 'solar' or self.type == 'lunar':

            # Look for solar spectrum (coming soon!)

            # Create empty structures
            self['atm'] = {}
            self['plume'] = {}
            self['atm']['ref_column'] = {}
            self['plume']['ref_column'] = {}

            # Modify atmospheric profile with user supplied info (sounding, measured P, T, RH)
            if self.geometry.sounding is not None:
                atm, self.geometry.atm_path = modifyAtm(self.geometry.atm_path, self.geometry.sounding)
            # if self.geometry.obs_temp is not None or self.geometry.obs_RH is not None:
            #     atm, self.geometry.atm_path = scaleAtm(self.geometry.atm_path, self.geometry.obs_height,
            #                                            T_obs=self.geometry.obs_temp,
            #                                            RH_obs=self.geometry.obs_RH,
            #                                            pbl=self.geometry.plume_height + self.geometry.plume_thickness)

            # Read in modified (or not) atmosphere
            atm = readAtm(self.geometry.atm_path)

            # Derive plume conditions at plume and observer height
            self.geometry.plume_temp = np.interp(self.geometry.plume_height, atm['HGT'], atm['TEM'])
            self.geometry.plume_pres = np.interp(self.geometry.plume_height, atm['HGT'], atm['PRE'])
            if self.geometry.obs_height is not None:
                self.geometry.obs_temp = np.interp(self.geometry.obs_height, atm['HGT'], atm['TEM'])
                self.geometry.obs_pres = np.interp(self.geometry.obs_height, atm['HGT'], atm['PRE'])
            if self.geometry.plume_thickness is None or self.geometry.plume_thickness == 0:
                self.geometry.plume_thickness = self.geometry.pathlength

            # ----- 1: Atmospheric gases -----
            logger.info('Atmospheric gases...')

            pbar = tqdm(total=len(self.params.atmGasList()))  # Progress bar
            for i, gas in enumerate(self.params.atmGasList()):
                pbar.set_description(gas)  # Change pbar message

                # Assign total atmosphere column as reference
                self['atm']['ref_column'][gas] = ppm2scd(atm[gas], atm['HGT'] * 1e3, temp=atm['TEM'], pres=atm['PRE'])

                # Write RFM driver and run
                writeRFMdrv(self.start_wave, self.end_wave, self.n_per_wave, layer=False, atm=self.geometry.atm_path,
                            height=self.geometry.obs_height, elev=self.geometry.elev, gas=gas, out='OPT')
                run('./RFM/source/rfm', stdout=DEVNULL)

                # Read Optical Depth
                self['atm'][gas] = readRFM('./RFM/output/opt%05d.out' % (self.geometry.elev * 1e3))['data']

                pbar.update()  # Update pbar
            pbar.close()  # Close pbar

            # ----- 2: Plume gases -----
            logger.info('Plume gases...')

            for i, gas in enumerate(self.params.plumeGasList()):

                # If gas is both atm and plume, remove suffix
                if '_pl' in gas: gas = gas.strip('_pl')

                # Assign reference column amount and find concentration based on pathlength
                ref_conc = std[gas]['conc']
                self['plume']['ref_column'][gas] = ppm2scd(ref_conc, self.geometry.plume_thickness,
                                                          temp=self.geometry.plume_temp, pres=self.geometry.plume_pres)

                # Handle SiF4 outside of RFM
                if any([gas.lower() == x for x in ['sif4']]):
                    self['plume'][gas] = makeGasXSC(self.start_wave, self.end_wave, self.n_per_wave,
                                                   species=gas)['Bext'] * self.geometry.plume_thickness * 1e3
                # Handle all other gases with RFM
                else:
                    # Write RFM driver and run
                    writeRFMdrv(self.start_wave, self.end_wave, self.n_per_wave, layer=True,
                                pathlength=self.geometry.plume_thickness, gas=gas, conc={gas: ref_conc},
                                temp=self.geometry.plume_temp, pres=self.geometry.plume_pres, out="OPT")
                    run('./RFM/source/rfm', stdout=DEVNULL)

                    # Read Optical depth
                    self['plume'][gas] = readRFM('./RFM/output/opt%05d.out' % (self.geometry.plume_thickness * 1e3))[
                        'data']

            # --- 3: Plume aerosols ---
            logger.info('Plume aerosols...')

            for aero in self.params.plumeAeroList():

                # Get size parameters from standard file
                size = std[aero]['size']
                sigma = std[aero]['sigma']
                mass_conc = std[aero]['conc']

                # Assign reference column amount
                self['plume']['ref_column'][aero] = std[aero]['conc'] * self.geometry.plume_thickness * 1e3

                # Is it a PSD?
                if sigma is None:
                    Bext = \
                        makeAerosolXSC(self.start_wave, self.end_wave, self.n_per_wave, size, mass_conc,
                                       species=aero)[
                            'Bext']
                else:
                    Bext = \
                        makeAerosolXSC(self.start_wave, self.end_wave, self.n_per_wave, size, mass_conc,
                                       psd=True,
                                       sigma=sigma, species=aero)[
                            'Bext']

                # Generate cross-section
                self['plume'][aero] = Bext * self.geometry.plume_thickness * 1e3

        # ---------------------------------------------------------------------
        # 3: Emission geometry
        # ---------------------------------------------------------------------
        elif self.type == 'emission':

            # Modify atmospheric profile with user supplied info?
            if self.geometry.sounding is not None:
                atm, self.geometry.atm_path = modifyAtm(self.geometry.atm_path, self.geometry.sounding)
            if self.geometry.obs_temp is not None or self.geometry.obs_RH is not None:
                atm, self.geometry.atm_path = scaleAtm(self.geometry.atm_path, self.geometry.obs_height,
                                                       T_obs=self.geometry.obs_temp,
                                                       RH_obs=self.geometry.obs_RH,
                                                       pbl=self.geometry.plume_height * 2)

            # Read in modified (or not) atmosphere
            self.atm = readAtm(self.geometry.atm_path)

            # Derive plume conditions at plume height
            self.geometry.plume_temp = np.interp(self.geometry.plume_height, self.atm['HGT'], self.atm['TEM'])
            self.geometry.plume_pres = np.interp(self.geometry.plume_height, self.atm['HGT'], self.atm['PRE'])
            if self.geometry.obs_height is not None:
                self.geometry.obs_temp = np.interp(self.geometry.obs_height, self.atm['HGT'], self.atm['TEM'])
                self.geometry.obs_pres = np.interp(self.geometry.obs_height, self.atm['HGT'], self.atm['PRE'])
            if self.geometry.plume_thickness == 0:
                self.geometry.plume_thickness = self.geometry.pathlength

            # Build config header for reference
            config = readConfig()
            needed = ['plume_height', 'plume_thickness', 'elev', 'obs_height', 'obs_temp', 'obs_RH', 'atm_path', \
                      'sounding', 'fit_window', 'atm_gases', 'plume_gases', 'plume_aero']
            self['config'] = {}
            for key in config['GEOMETRY'].keys():
                if key in needed: self['config'][key] = config['GEOMETRY'][key]
            for key in config['FIT1'].keys():
                if key in needed: self['config'][key] = config['FIT1'][key]
            self['config']['model_spacing'] = self.model_spacing
            self['config']['model_padding'] = self.model_padding

            # Look in data directory to see if the same config has been run before
            logger.info('Looking for references matching FIT configuration...')
            ref_list = glob.glob(self.data_dir + 'emissionReference_*.pkl')
            ref_list.sort()
            matches = []
            if len(ref_list) != 0:
                for fname in ref_list:
                    with open(fname, 'rb') as f:
                        ref_pot = pickle.load(f)
                    if ref_pot['config'] == self['config']:
                        matches.append(fname)

            # If a matching reference exists, use the latest one
            if len(matches) > 0:
                matches.sort()
                logger.info('...Found %i! Opening most recent: %s' % (len(matches), matches[-1].split('/')))
                with open(matches[-1], 'rb') as f:
                    self = pickle.load(f)

            # Otherwise proceed
            else:

                logger.info('...No matching reference. Creating one according to configuration...')

                # Create empty structures
                self['atm_prox'] = {}
                self['atm_dist'] = {}
                self['plume'] = {}
                self['atm_prox']['ref_column'] = {}
                self['atm_dist']['ref_column'] = {}
                self['plume']['ref_column'] = {}

                # Atmospheric gases in proximal atmosphere
                logger.info('Atmospheric gases: from observer to plume height...')
                atm_prox, prox = sliceAtm(self.geometry.atm_path, self.geometry.obs_height, self.geometry.plume_height,
                                          outpath=True)

                # Create empty structures
                self['atm_prox']['L_calib'] = {}
                self['atm_prox']['OD_calib'] = {}

                # Start progress bar
                scalings = np.linspace(0, 2, 21)
                pbar = tqdm(total=len(self.params.atmGasList()) * len(scalings))

                # Start loop
                for i, gas in enumerate(self.params.atmGasList()):

                    # Assign total column as reference
                    self['atm_prox']['ref_column'][gas] = ppm2scd(atm_prox[gas], atm_prox['HGT'] * 1e3,
                                                                 temp=atm_prox['TEM'],
                                                                 pres=atm_prox['PRE'])
                    # Write RFM driver and run
                    L_atm_prox = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                    OD_atm_prox = np.empty([len(self.model_grid), len(scalings)], dtype=float)

                    for j, sc in enumerate(scalings):
                        # Update progress bar label
                        pbar.set_description('%s: scaling=%.1f' % (gas, sc))

                        # Write RFM driver and run
                        atm, scaled = scaleAtm(prox, sc, which=gas, outpath=True)
                        writeRFMdrv(self.start_wave, self.end_wave, self.n_per_wave, layer=False, atm=scaled,
                                    height=self.geometry.obs_height, elev=self.geometry.elev, gas=gas)
                        run('./RFM/source/rfm', stdout=DEVNULL)

                        # Read Optical Depth and Radiance
                        L_atm_prox[:, j] = readRFM('./RFM/output/rad%05d.out' % (self.geometry.elev * 1e3))[
                                               'data'] / 100
                        OD_atm_prox[:, j] = readRFM('./RFM/output/opt%05d.out' % (self.geometry.elev * 1e3))['data']

                        # Update progress bar
                        pbar.update()

                    # Perform calibration
                    self['atm_prox']['L_calib'][gas] = np.polyfit(scalings, L_atm_prox.T, 10)
                    self['atm_prox']['OD_calib'][gas] = np.polyfit(scalings, OD_atm_prox.T, 10)

                # Close progress bar
                pbar.set_description('Done')
                pbar.close()

                # Atmospheric gases in distal atmosphere
                logger.info('Atmospheric gases: from plume_height to edge...')
                atm_dist, dist = sliceAtm(self.geometry.atm_path, self.geometry.plume_height, self.atm['HGT'].max(),
                                          outpath=True)

                # Create empty structures
                self['atm_dist']['L_calib'] = {}
                self['atm_dist']['OD_calib'] = {}

                # Start progress bar
                scalings = np.linspace(0, 2, 21)
                pbar = tqdm(total=len(self.params.atmGasList()) * len(scalings))

                for i, gas in enumerate(self.params.atmGasList()):

                    # Assign total column as reference
                    self['atm_dist']['ref_column'][gas] = ppm2scd(atm_dist[gas], atm_dist['HGT'] * 1e3,
                                                                 temp=atm_dist['TEM'],
                                                                 pres=atm_dist['PRE'])

                    # Create empty stack fro polynnomial fit
                    L_atm_dist = np.empty([len(self.model_grid), len(scalings)], dtype=float)
                    OD_atm_dist = np.empty([len(self.model_grid), len(scalings)], dtype=float)

                    # Start loop
                    for j, sc in enumerate(scalings):
                        # Update progress bar label
                        pbar.set_description('%s: scaling=%.1f' % (gas, sc))

                        # Write RFM driver and run
                        atm, scaled = scaleAtm(dist, sc, which=gas, outpath=True)
                        writeRFMdrv(self.start_wave, self.end_wave, self.n_per_wave, layer=False, atm=scaled,
                                    height=self.geometry.plume_height, elev=self.geometry.elev, gas=gas)
                        run('./RFM/source/rfm', stdout=DEVNULL)

                        # Read Optical Depth and Radiance
                        L_atm_dist[:, j] = readRFM('./RFM/output/rad%05d.out' % (self.geometry.elev * 1e3))[
                                               'data'] / 100
                        OD_atm_dist[:, j] = readRFM('./RFM/output/opt%05d.out' % (self.geometry.elev * 1e3))['data']

                        # Update progress bar
                        pbar.update()

                    # Perform calibration
                    self['atm_dist']['L_calib'][gas] = np.polyfit(scalings, L_atm_dist.T, 10)
                    self['atm_dist']['OD_calib'][gas] = np.polyfit(scalings, OD_atm_dist.T, 10)

                # Close progress bar
                pbar.set_description('Done')
                pbar.close()

                # --- 2: Plume gases ---
                logger.info('Plume gases...')
                for i, gas in enumerate(self.params.plumeGasList()):

                    # If gas is both atm and plume, remove suffix
                    if '_pl' in gas: gas = gas.strip('_pl')

                    # Assign reference column amount and find concentration based on plume thickness
                    ref_conc = std[gas]['conc']
                    self['plume']['ref_column'][gas] = ppm2scd(ref_conc, self.geometry.plume_thickness,
                                                              temp=self.geometry.plume_temp,
                                                              pres=self.geometry.plume_pres)

                    # Handle SiF4 outside of RFM
                    if any([gas.lower() == x for x in ['sif4']]):
                        self['plume'][gas] = makeGasXSC(self.start_wave, self.end_wave, self.n_per_wave,
                                                       species=gas)['Bext'] * self.geometry.plume_thickness * 1e3

                    # Handle all other gases with RFM
                    else:
                        # Write RFM driver and run
                        writeRFMdrv(self.start_wave, self.end_wave, self.n_per_wave, layer=True,
                                    pathlength=self.geometry.plume_thickness,
                                    gas=gas, conc={gas: ref_conc}, temp=self.geometry.plume_temp,
                                    pres=self.geometry.plume_pres)
                        run('./RFM/source/rfm', stdout=DEVNULL)

                        # Read Optical Depth
                        self['plume'][gas] = readRFM('./RFM/output/opt%05d.out' % (self.geometry.plume_thickness * 1e3))[
                            'data']

                # --- 3: Plume aerosols ---
                logger.info('Plume aerosols...')
                for aero in self.params.plumeAeroList():

                    # Get size parameters from standard file
                    size = std[aero]['size']
                    sigma = std[aero]['sigma']
                    mass_conc = std[aero]['conc']

                    # Assign reference column amount
                    self['plume']['ref_column'][aero] = std[aero]['conc'] * self.geometry.plume_thickness * 1e3

                    # Is it a PSD?
                    if sigma is None:
                        Bext = \
                            makeAerosolXSC(self.start_wave, self.end_wave, self.n_per_wave, size, mass_conc,
                                           species=aero)[
                                'Bext']
                    else:
                        Bext = \
                            makeAerosolXSC(self.start_wave, self.end_wave, self.n_per_wave, size, mass_conc, psd=True,
                                           sigma=sigma, species=aero)[
                                'Bext']

                    # Generate cross-section
                    self['plume'][aero] = Bext * self.geometry.plume_thickness * 1e3

                # For emission references, save a copy to avoid lengthy reruns
                outname = self.data_dir + 'emissionReference_%s.pkl' % datetime.strftime(datetime.now(), '%Y%m%dT%H%M')
                with open(outname, 'wb') as f:
                    pickle.dump(self, f)

        logger.info('...Done!')

        # Assign Reference dict to Analyser
        self.reference = self

        # Compute theoretical clear sky on model grid
        if self.type == 'emission' and self.fit_difference:
            OD_prox = 0
            OD_dist = 0
            for i, gas in enumerate(self.params.atmGasList()):
                OD_prox = OD_prox + np.polyval(self.reference['atm_prox']['OD_calib'][gas], 1.0)
                OD_dist = OD_dist + np.polyval(self.reference['atm_dist']['OD_calib'][gas], 1.0)
            T_prox = np.exp(-OD_prox)
            L_prox = planck(self.model_grid, self.geometry.obs_temp) * (1 - T_prox)
            T_dist = np.exp(-OD_dist)
            L_dist = planck(self.model_grid, self.geometry.plume_temp) * (1 - T_dist)
            self.L_clear_model = L_dist * T_prox + L_prox

        # Update gas parameters with reference column amount
        for name, p in self.params.items():
            if name in self.params.atmGasList():
                if self.type == 'emission':
                    p.set(ref_column=self['atm_dist']['ref_column'][name] + self['atm_prox']['ref_column'][name])
                else:
                    p.set(ref_column=self['atm']['ref_column'][name])
            if name in self.params.plumeGasList():
                if '_pl' in name: name = name.strip('_pl')
                p.set(ref_column=self['plume']['ref_column'][name])
            if name in self.params.plumeAeroList():
                p.set(ref_column=self['plume']['ref_column'][name])

        # Print the list of gases in reference
        self.prettyPrint()


# ======================================================================================================================
#                                        Plot Reference
# ======================================================================================================================
    def plotReference(self, name=None):

        self = self.reference
        wave = self.model_grid
        if name is None:
            name = self.name
        targets = self.params.targetList()

        fig, ax = plt.subplots(figsize=[10, 5])
        ax2 = plt.twinx(ax)
        fig.suptitle('%s %s: %i - %i $cm^{-1}$' % (name, targets, self.start_wave, self.end_wave))
        atm_colors = {'H2O': 'lightblue',
                      'CO2': 'lightgreen',
                      'O3': 'lightpink',
                      'N2O': 'palegoldenrod',
                      'CH4': 'thistle'}
        plume_colors = {'H2O': 'blue',
                        'CO2': 'green',
                        'SO2': 'red',
                        'HCl': 'teal',
                        'HF': 'magenta',
                        'CO': 'maroon',
                        'NH3': 'yellow',
                        'CH4': 'purple',
                        'SiF4': 'purple',
                        'H2SO4': 'tab:green',
                        'ASH': 'k',
                        'WATER': 'b'}

        for gas in self.params.atmGasList():
            if self.type == 'emission':
                T = np.exp(-np.polyval(self['atm_dist']['OD_calib'][gas], 1.0))
                ax.plot(wave, T, c=atm_colors[gas], label=gas, zorder=0, alpha=1.0)
            else:
                T = np.exp(-self['atm'][gas])
                ax.plot(wave, (T - np.min(T)) / np.ptp(T), c=atm_colors[gas], label=gas, zorder=0, alpha=1.0)

        for gas in self.params.plumeGasList():
            if '_pl' in gas: gas = gas.strip('_pl')
            A = 1 - np.exp(-self['plume'][gas])
            ax2.plot(wave, (A - np.min(A)) / np.ptp(A), c=plume_colors[gas], label=gas, zorder=1, alpha=1.0)

        for aero in self.params.plumeAeroList():
            A = 1 - np.exp(-self['plume'][aero])
            ax2.plot(wave, (A - np.min(A)) / np.ptp(A), c=plume_colors[aero], label=aero, zorder=1, alpha=1.0)

        ax.set(xlabel='Wavenumber [$cm^{-1}$]', ylabel='Normalised Transmittance\n(atm gases)', ylim=[0, 1])
        ax2.set(ylabel='Normalised Absorbance\n(plume gases)', ylim=[0, 1])
        ax.legend(title='Atmosphere')
        ax2.legend(title='Plume')

        plt.tight_layout(rect=[0, 0, 1, 0.95])
        plt.show()