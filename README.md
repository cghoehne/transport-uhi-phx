### Workflow

## Data preperation
- It is recommended to use Microsoft's Open R disriubtion
- Download and install R version 3.5.1 if not already present on system
- If you want to validate the model against remote sensed surface tempeatures, this approach does so using ASTER On Demand Surface Kinetic Temperature
- You could also potentially use Landsat 7/8 but would need to updates scripts accordingly
- If validating, custom locations for validation should be chosen such that the match the formant of "data/validation_sites.csv"
- ASTER LST data info: https://lpdaac.usgs.gov/dataset_discovery/aster/aster_products_table/ast_08
- If you want to replicate using MesoWest API to reterive historical weather data for model forcing, you will need an API key from Synoptic Labs
- MesoWest/ Synoptic info: https://developers.synopticdata.com/mesonet/
- Store raw ASTER data from desired periods after unzipping in "data/aster/raw"
- Store API token in file named local-token.txt in main directory (it is git ignored by default)

## R scripts

# 1a-sat-data-preprocess.R
- Should only need to be run once
- This script simply imports all ASTER .tif data, converts all values to to deg C
- Then batch dumps in a single folder "data/aster/all"

# 1b-satellite-LST-extraction.R
- This script reterives metadata for all ASTER scenes and cleans it
- Then according to user inputs, selects a small subset of desired scenes for use in validation
- Default is no cloud cover, then *THREE* custom scenes are selected that have good coverage of sites
- Because of weird projection issues of ASTER that R libraries have trouble interpreting,
- A python script through QGIS is used to fix the projection issue within this script
- This must be done manually before the second half of script currently **
- Will also plot selected scenes using a predefined shapefile of the metro region (Maricopa County UZA in our case)

# 2-weather-data-retrieval.R
- This script requires the API key from Synoptic (free, need an account)
- This script will pull all weather data from all stations in a predefined area and all corresponding weather data
- Default is to reterive all station data availabe in Phoenix metro for 2000 to 2018

# 3-heat-transfer-model.R
- This is the heat transfer model
- Primary weather inputs: temperature, solar rad, humidity/dew, wind
- If using MesoWest data from 2__.R, it should be properly cleaned already
- Inputs of desired parameters of materials (such as asphalt concrete pavement specifications)
- Inputs of model run parameters (iterations, day simulation lenght, etc)

# 4-model-post-process
- computes summary statistics
- plots simulated pavement temperature profiles
- compares remotely sensed validation LST data to simulated data