# flu-long-term-agent
This github repository includes the code supporting the article ['How immunity shapes the long-term dynamics of seasonal influenza'](https://www.medrxiv.org/content/10.1101/2023.09.08.23295244v1.full)

# Documenation
The main python scripts that simulate the long-term dynamics of influenza are included in the main directory `flu-long-term-agent` with the source code describing the agent based model included in the sub-directory `flu-long-term-agent/src`.

The code required to fit to the antigenic data avaialble from [Bedford et al 2014](https://elifesciences.org/articles/01914) to parameterise the global drift model is included in the subdirectory `flu-long-term-agent/XXXXXXXXXXXXXX`. The parameter values are already in the main python scripts and so do not need to be updated before running.

The code required to produce the figures in the publication are then included in the sub-directory `flu-long-term-agent/figure-scripts`. The figure scripts rely on the output of the three python scripts in the main directory.

## Python simulations
There are three python scripts that simulate the long-term dynamics of influenza. 
1. `main.py` runs 160 year simulations of influenza dynamics recording the yearly attack rate by age. 256 simulations are run for each set of parameters provided. The code runs simulations in parallel so that a single simulation is run on each available core simultaneously (the more cores available the faster the code will run). To speed up code for testing you can change the global parameters at the top of the script. Reducing the number of simulations, population size, or number of years will all speed up the run time of the scipt. The ouput will appear in `flu-long-term-agent/output` and should be moved to `flu-long-term-agent\figure-scripts\input-data\AttackRates\` for plotting.
2. `parameter-selection.py` runs 40 year simulations of influenza dynamics recording the yearly attack rate and the week of peak incidence. For each set of parameters 20 simulations are run. Parameter values are varied for the seasonality and average contact parameter (beta0 and beta1) across a 2d grid. The ouput (`parameter_selection_ar.csv` and `parameter_selection_pk_wk.csv`)  will appear in `flu-long-term-agent/output` and should be moved to `flu-long-term-agent\figure-scripts\input-data\ParameterSelection\` for plotting.
3. `detailed_sim_20_years.py` runs a single simulation of influenza dynamics for 20 years recording the daily attack rate by strain, and recording the strains present in each year of the model. The code will output 20 csv files with the incidence curves for each year and 2 csv files with the mean strain coordinates and raw strain coordnates. The output is small and is available in `flu-long-term-agent\figure-scripts\input-data\StrainCurves\`

## Global drift model fitting

## Figure scripts

# System Requirements
## Hardware requirements

## Software requirements
### OS Requirements

### Python Dependencies


# Installation Guide:
### Install from Github


# License
