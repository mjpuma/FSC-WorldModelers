# Food Shocks Cascade Model
- A simple agent-based network model that computes chain-reactions due to negative production anomalies based on dynamic food balance sheets at the country level.
- Based on original code at https://github.com/pmarchand1/cereals-network-shocks, which was described in the paper by Marchand *et al.* [Reserves and trade jointly determine exposure to food supply shocks](http://iopscience.iop.org/article/10.1088/1748-9326/11/9/095009).
- Revised version: multiple people including B. Rose, A. Heslin, P. Marchand, M. Puma

## Model Vignette
- INPUT (required): Bilateral trade of commodities at country level, production/consumption/storage for all countries
- INPUT (ancillary): Commodity list, country list, conversion factors from commodity mass to common units (e.g. kcal, protein, US dollars)
- OUTPUT: Changes in stocks and consumption at country level
- CODE: Written in R
- RUNTIME: few minutes on desktop computer
- RESOLUTION: Country level

## Model Scripts
### main.R
This is the main script for running FSC from the command line. It wraps much of the functionality demonstrated in `Analyze.R` into a set of callable functions.

### MatrixCreation.R
Creates trade, production, and reserves matrices for use in cascade model. Requires existing files in *ancillary* and *inputs* directory: 

### CascadeFunction.R
Contains functions for the FSC model

### Analyze.R
- Example script to run the FSC model, calling functions in *CascadeFunction.R* for the case where production 1) in a single country is negatively affected and 2) in multiple countries are negatively affected.

## Ancillary files
- crop_list.csv = croplist with kcal conversions
- country_list.csv = FAO country code and true/false for country pop over 500k 
- ciso3.txt = country codes

## Input files
- Trade data from FAOSTAT, detailed trail matrix, normalized, all data. The trade matrix is available here - http://www.fao.org/faostat/en/#data/TM - on the right side bar under "Bulk Downloads", select "All Data Normalized".   *trade_dat <- read.csv("Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"*
- Production data from FAOSTAT, production quantity in tonnes. The crop production data are available at: http://www.fao.org/faostat/en/#data/QC. *prod_dat<-read.csv("productiondataFAOSTAT.csv")*
- Reserves data from USDA, downloadable dataset - psd grains pulses. The stocks data are available here - https://apps.fas.usda.gov/psdonline/app/index.html#/app/downloads - listed as "Grains" the file is called "psd_grains_pulses_csv.zip". *psd <- read.csv("psd_grains_pulses.csv")*


## Automatically pulling inputs
```
cd inputs
wget 'http://fenixservices.fao.org/faostat/static/bulkdownloads/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip'
wget 'http://fenixservices.fao.org/faostat/static/bulkdownloads/Production_Crops_E_All_Data_(Normalized).zip'
wget 'https://apps.fas.usda.gov/psdonline/downloads/psd_grains_pulses_csv.zip'

unzip 'Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip'
unzip 'Production_Crops_E_All_Data_(Normalized).zip'
unzip 'psd_grains_pulses_csv.zip'
```

## Running a Simulation
To run a simulation, first clone this repository and navigate to the top-level directory with:

```
git clone git@github.com:mjpuma/FSC-WorldModelers.git
cd FSC-WorldModelers
```

Next, you need to install the required R Packages:

```
rscript main/Requirements.R
```

Next, you should prepare the input data:

```
rscript main/MatrixCreation.R
```

Now you are ready to run a simulation. To run a simulation you should choose the following:

* `year`: the year you wish to simulate
* `country`: the [ISO 3 country code](https://unstats.un.org/unsd/tradekb/Knowledgebase/Country-Code) for the country of interest. *Note*: if you choose `All`, an equal shock is simulated for all countries
* `production decrease`: the decrease in production you wish to induce (from 0 to 1, where 1 equals a 100% decrease)
* `fractional reserve access`: the percentage of fractional reserves which may be accessed (from 0 to 1, where 1 equals a 100% decrease)
* `output file name`: this is the name of the file which will be created by the simulation. Note that if you chose `all` for `country`, this will be the name of the directory that is created housing the several output files. This should not include a file type (for example, it should be `some_output` instead of `some_output.csv`).

Now, you can run a simulation with something like the following:

```
rscript main/main.R 2005 "USA" 0.4 0.5 single_country_example
```
In this case, we have chosen the following parameters:

* `year`: 2005
* `country`: United States
* `production decrease`: 40%
* `fractional reserve access`: 50%
* `output file name`: single_country_example


## Running with Docker
First, you must build the Docker image. To do this, navigate to the top-level of this repository and then run:

```
docker build -t fsc/latest .
```

Here, we have tagged the container as `fsc/latest`. Once the container is built we can run it by mounting an `outputs` directory from our host to the container and then passing the appropriate arguments in the correct order:

```
docker run -v $PWD/outputs:/outputs fsc/latest 2005 "USA" 0.4 0.5 single_country_example
```
