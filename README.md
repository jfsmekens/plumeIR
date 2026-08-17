# plumeIR

plumeIR is a program designed to retrieve gas and particulate column densities from IR spectra measured in emission geometry by Open-Path Fourier Transform (OP-FTIR) spectrometers, with the specific purpose of monitoring volcanic emissions.

## Create Python Environment
plumeIR requires Python 3.6+ with the following packages:

`numpy`         - for basic array manipulation
`scipy`         - for spectral optimisation and statistical analysis of the data
`matplotlib`    - for plotting
`pandas`        - to create and save databses with the results
`tqdm`          - to display progressbars
`PyMieScatt`    - to perform Mie scattering calculation for particulates
`scikit-learn`  - for error analysis 

To get started with Anaconda create a new environment:

```
conda create -n OPFTIR numpy scipy matplotlib tqdm pandas scikit-learn
```

where `OPFTIR` is the name of the environment. The environment can then be activated with:

```
conda activate OPFTIR
```

The PyMieScatt package is not on the standard Anaconda channel and must be installed after activating the environment:

```
conda install -c conda-forge pymiescatt
```
## Installing RFM
plumeIR uses the Reference Forward Model (RFM) to perform radiative transfer simulations. Before running the program, you MUST install RFM in the plumeIR directory.

**RFM is not distributed with plumeIR.** It is developed and maintained at Oxford University (AOPP) by Anu Dudhia, and is obtained directly from the author: email anu.dudhia@physics.ox.ac.uk with a brief note on what you plan to use it for, and he will send you the source code. See https://eodg.atm.ox.ac.uk/RFM/ for details.

Instructions on how to install and compile RFM can be found here: http://eodg.atm.ox.ac.uk/RFM/sum/rfm_installation.html.

If you use plumeIR in published work, please cite the RFM alongside it:
Dudhia, A. The Reference Forward Model (RFM). *Journal of Quantitative Spectroscopy and Radiative Transfer* **186** (2017) 243-253.

## Get Started

The main folder includes all the plumeIR functions, separated into modules. The first step is to modify the configuration file, which contains all the information necessary to run a retrieval (pointing to the data, spectral parameters for multiple fits, which mode is being run, etc.) 
Navigate to the plumeIR folder, make sure the Anaconda environment is active and run the main script: 

```
python plumeIR.py
```

Data can be placed in the local '/data/' subfolder for easier access, in which case you can specify data directory as local paths (e.g., './data/OPFTIR_dataset/). Alternatively you can just point to anny other directory. 

If using custom atmospheric profiles or local soundings,  ake sure to place them in the './atm/' folder.

## License

plumeIR is released under the MIT License — see [LICENSE](LICENSE).

This covers plumeIR only. The RFM is separately owned and maintained at Oxford (AOPP), is not distributed here, and is not covered by this license. See "Installing RFM" above for how to obtain it.

