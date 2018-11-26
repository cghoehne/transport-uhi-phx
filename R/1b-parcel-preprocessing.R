# this analysis uses parking data at the parcel level
# this script clips parcel data for which we then append parking data via parcel id
# load unclipped regional parcel data, create clipped parcels based on variable station buffers
# and filter raw parcels to parcels within station buffers to then calculate full parcel area (used for partial parking area calcs b/c some parcels intersect buffer boundary)
# merge full.parcel.area varaible back to clipped station.parcels with duplicates true to keep all instances (b/c if some buffers overlap, parcels can be in multiple station buffers)

# start by maxing out the memory allocation (we use high number to force max allocation)
gc()
#memory.limit(size = 56000) 

# list of all dependant packages
list.of.packages <- c("tidyverse",
                      "data.table", 
                      "rgdal",
                      "rgeos",
                      "maptools",
                      "sp",
                      "doParallel",
                      "foreach",
                      "raster",
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
invisible(lapply(list.of.packages, library, character.only = T)) # invisible() just hides printing stuff in console

# load and clip parcels
parcels <- readRDS(here("data/outputs/temp/all_parcels_mag.rds")) # load full parcels in region
stations.buffered <- readRDS(here("data/outputs/station-buffers-sp-list.rds"))
l <- length(stations.buffered) # store index of largest buffer radii
parcels.trimmed <- intersect(parcels, stations.buffered[[l]])
saveRDS(parcels.trimmed, here("data/outputs/parcels-trimmed.rds"))
# end