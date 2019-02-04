## INTRO

# this script details the surface temperature validation via Landsat 8 ARD data
# for year 2016 at custom chosen sites to validate the 1D heat transfer model

# ** It is recommended to use Microsoft Open R (v3.5.1) for improved performance without compromsing compatibility **

## SCRIPT PREPERATION

# clear space and allocate memory
gc()
memory.limit(size = 56000) 
script.start <- Sys.time() # start script timestamp

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint)
}

# load all other dependant packages from the local repo
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(mailR, lib.loc = lib.path)
library(sp, lib.loc = lib.path)
library(rgdal, lib.loc = lib.path)
library(rgeos, lib.loc = lib.path)
library(raster, lib.loc = lib.path)
library(zoo, lib.loc = lib.path)
library(lubridate, lib.loc = lib.path)
library(data.table, lib.loc = lib.path)
library(here, lib.loc = lib.path)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

## IMPORT VALIDATION SITE DATA 

# load csv with site lat longs (change to your points of intreset)
my.sites <- read.csv(here("data/validation_sites.csv"))

# convert data frame of lat-long points to coordinates (SpatialPointDataFrame)
sites.proj <- my.sites

# assign X & Y
coordinates(sites.proj) <- c("X", "Y")

# project points to long-lat projection WSG84
proj4string(sites.proj) <- CRS("+proj=longlat +datum=WGS84") 

# check coords are correct
print(sites.proj@coords, digits=10)


## IMPORTING and PROCESSING LANDSAT DATA

# Landsat 8 (L8) ARD data is in .tif files 
# 30-meter spatial resolution in Albers Equal Area (AEA) projection 
# World Geodetic System 1984 (WGS84) datum 

# list of geotif files in sub folder of interation 'a'
tile.list <- list.files(here("data/landsat"), recursive= T, pattern="tif$", full.names= T)  

# stack the raw geotiff tiles

# stack list of geotifs into a raster stack for sub folder of interation 'a'
tile.stack <- list() # empty list first
#tile.stack <- stack(tile.list) # single
tile.stack <- lapply(tile.list, stack) # multiple stacks (?)

#tiles.prj <- list() # empty list

# merge tiles of same date into one (to get complete coveage of metro area)
tile.merge <- do.call(merge, c(tile.stack, tolerance = 1))



## PLOTTING of LANDSAT DATA

tmp <- raster(tif.list[9])
plot(tmp, col = rev(heat.colors(40)))

# get Phoenix Urbanized Area (UZA) data and match projection to overlay UZA boundary on plots
# lat lon projection
longlat.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# buffer uza boundary by 1 mile (1.6 km)
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # import Maricopa County UZA boundary
uza.buffer <- gBuffer(uza.border, byid = F, width = 5280) # add a one mile buffer to ensure no L8 data is lost when clipped

# create bounding box for UZA, which requires top left and bottom right coords c(lat, lon, lat, lon)
uza.bbox <- spTransform(uza.buffer, longlat.prj)
uza.bbox <- bbox(uza.bbox)


# project uza buffer to crs of daymet data to crop
uza.buffer.prj <- spTransform(uza.buffer, crs(tiles.m))
uza.border.prj <- spTransform(uza.border, crs(tiles.m))

# crop the merged tiles by the projected uza buffer
tiles.mc <- crop(tiles.m, uza.buffer.prj)

