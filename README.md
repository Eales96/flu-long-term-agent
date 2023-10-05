# flu-long-term-agent
This github repository includes the code supporting the article ['How immunity shapes the long-term dynamics of seasonal influenza'](https://www.medrxiv.org/content/10.1101/2023.09.08.23295244v1.full)

# Documenation
The main python scripts that simulate the long-term dynamics of influenza are included in the main directory `flu-long-term-agent` with the source code describing the agent based model included in the sub-directory `flu-long-term-agent/src`.

The code required to fit to the antigenic data avaialble from [Bedford et al 2014](https://elifesciences.org/articles/01914) to parameterise the global drift model is included in the subdirectory `flu-long-term-agent/global-drift-model`. The parameter values are already in the main python scripts and so do not need to be updated before running.

The code required to produce the figures in the publication are then included in the sub-directory `flu-long-term-agent/figure-scripts`. The figure scripts rely on the output of the three python scripts in the main directory.

## Python simulations (main model)
There are three python scripts that simulate the long-term dynamics of influenza. 
1. `main.py` runs 160 year simulations of influenza dynamics recording the yearly attack rate by age. 256 simulations are run for each set of parameters provided. The code runs simulations in parallel so that a single simulation is run on each available core simultaneously (the more cores available the faster the code will run). To speed up code for testing you can change the global parameters at the top of the script. Reducing the number of simulations, population size, or number of years will all speed up the run time of the scipt. The ouput will appear in `flu-long-term-agent/output` and should be moved to `flu-long-term-agent\figure-scripts\input-data\AttackRates\` for plotting.
2. `demo.py` runs 8 160 year simulations for a population of 20000 for the baseline parameters only and offers a convenient way to observe the code and its output in a time efficient manner. This should produce a single csv file. There should be 80 columns for the 80 possible ages. The first 160 rows are the 160 years of the first simulation, the next 160 rows are the 160 years of the second simulation, and so on.
3. `parameter-selection.py` runs 40 year simulations of influenza dynamics recording the yearly attack rate and the week of peak incidence. For each set of parameters 20 simulations are run. Parameter values are varied for the seasonality and average contact parameter (beta0 and beta1) across a 2d grid. The ouput (`parameter_selection_ar.csv` and `parameter_selection_pk_wk.csv`)  will appear in `flu-long-term-agent/output` and should be moved to `flu-long-term-agent\figure-scripts\input-data\ParameterSelection\` for plotting. 
4. `detailed_sim_20_years.py` runs a single simulation of influenza dynamics for 20 years recording the daily attack rate by strain, and recording the strains present in each year of the model. The code will output 20 csv files with the incidence curves for each year and 2 csv files with the mean strain coordinates and raw strain coordnates. The output is small and is available in `flu-long-term-agent\figure-scripts\input-data\StrainCurves\`

## Global drift model fitting
The code required to fit to the global drift model to the antigenic data avaialble from [Bedford et al 2014](https://elifesciences.org/articles/01914) is included in the subdirectory `flu-long-term-agent/global-drift-model`. The R scipt `flu-long-term-agent/global-drift-model/drift_model.R` loads the antigenic data (`flu-long-term-agent/global-drift-model/antigenic_data.csv`), defines a model in STAN, and then fits the model to the antigenic data. The mean parameters for `lamda`, `theta` and `sigma` are used to parameterise the main agent based model for influenza dynamics implemented in python. The entire posterior distribution of the model fitting is saved to `posterior.rds` for plotting in a figure. The files `antigenic_data.csv` and `posterior.rds` (already fitted) are available in `flu-long-term-agent/figure_scripts/input_data/` and are used for plotting figures of the model fit (SFig. 1). The model fitting should take approximately 10 minutes (or 2.5 minutes if 4 cores are available).

## Figure scripts
There are multiple figure scripts located in `flu-long-term-agent/figure_scipts/` which are used to convert the raw output from the python simulations, and the global drift model fit posterior, into figures and values (e.g. correlations, mean yearly attack rate, etc). The figure scipts have some functions defined in `flu-long-term-agent/plot_functions.R` on which they rely on. Note that some of the figure scripts can take approximately one hour to run over output from 256 simulations, code will be quicker if fewer numbers of years or simulations are used as input (if a different number of years, simulations, or population size are used when running the python model the the global parameters (`N_iter` and `N_years` and `N_pop_size`) at the top of the figure scripts must be updated). Note that some of the figure scripts can be run without running any of the simulations/ model fitting defined above as the output has already been saved into the subdirectory `flu-long-term-agent/figure_scipts/input_data/`, but many of the figure scipts can not be run as they depend on outputs that are too large to host on Github. The figure scipts are: 
1. `baseline_graphs.R` produces Fig.3, Fig.4, SFig.4, SFig.5 and SFig.8
2. `baseline_high_graphs.R` produces SFig. 6
3. `baseline_low_graphs.R` produces SFig. 7
4. `example_simulation.R` produces Fig.2
5. `immunity_methods_figure.R` produces Fig.1, SFig.1, SFig.2 and SFig.3
6. `longterm_immunity_graphs.R` produces SFig.10
7. `parameter_selection.R` produces SFig.11
8. `waning_immunity_graphs.R` produces Fig.6
9. `antigenic_seniority_graphs.R` produces Fig.5 and SFig.9

# System Requirements
## Hardware requirements
The simulations as performed in `main.py` take approximately 1 hour to complete (length varies with immunity parameters). Simulations are run in parallel using the maximum number of cores available on the computer. We ran the simulations on a machine supporting 32 cores allowing 256 simulations (for a single set of parameters) to be performed in approximately 8 hours. To speed up the code for testing you can decrease the number of simulations, the population size, or the number of years to simulate. Alternatively using a machine with a greater number of cores will allow more simulations to be perfomed simultaneously, speeding up the analysis.

## Software requirements
### OS Requirements
The code does not rely on any specific operating system and only requires that R and Python be installed (see versions and dependencies below).
The code has been tested on the following systems:
+ Windows 10 21H2
+ Linux: Ubuntu 20.04
### Python Dependencies
The Python code was implemented using `Python 3.11`.

The python scripts have the following dependencies:
```
contourpy==1.0.7
cycler==0.11.0
fonttools==4.39.2
joblib==1.2.0
kiwisolver==1.4.4
matplotlib==3.7.1
numpy==1.24.2
packaging==23.0
pandas==1.5.3
Pillow==9.4.0
pyparsing==3.0.9
python-dateutil==2.8.2
pytz==2022.7.1
seaborn==0.12.2
six==1.16.0
```
These are also listed in requirments.txt and can be installed as shown below in the installation guide.

### R Dependencies
The R code was implemented in `R version 4.2.3`.

The scipts written in R have the following dependencies (version given is the version the code was run with):
```
ggplot2 version 3.4.2
RColorBrewer version 1.1.3
ggrepel version 0.9.3
grid version 4.2.3
gridExtra version 2.3
cowplot version 1.1.1
conmat version 0.0.2.9000
tidyr version 1.3.0
ggdist version 3.2.1
dplyr version 1.1.2
ggridges version 0.5.4
mgcv version 1.8.42
metR version 0.14.0
rstan version 2.21.3
```

# Installation Guide:
### Install from Github
```
git clone https://github.com/Eales96/flu-long-term-agent
cd flu-long-term-agent
pip install virtualenv (if you don't already have virtualenv installed)
virtualenv venv to create your new environment (called 'venv' here)
source venv/bin/activate to enter the virtual environment
pip install -r requirements.txt (sets up virtual environment for running the python scipts)
```
The above installation should take no longer than 10 minutes. 

Specific versions of R packages can be installed by running the following in the R console:
```
install.packages("remotes") (if you do not have the package remotes installed already)
library(remotes)
install_version("PACKAGE_NAME", "PACKAGE_VERSION") (repeat for any packages needed)
```
Fitting the global drift model in `flu-long-term-agent/global-drift-model/drift_model.R` requires the package `Rstan`. For full details on the installation of RStan please refer to the [RStan Getting Started GitHub](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).


# License
This project is covered under the **Apache 2.0 License**.
