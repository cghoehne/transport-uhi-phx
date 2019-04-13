# preprocess ASTER data by importing from raw folder unzips, adjusting temps and export
# this only needs to be done once. chosen dates for validation should then be
# fixed by QGIS (open and saving should do the trick)
# there are some unusual issues with R libraries not properly detecting the rotation/resolution/ect
# so it is seemingly impossible to fix them using rectify/transform ect.

# ** It is recommended to use Microsoft Open R (v3.5.1) for improved performance without compromsing compatibility **

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
library(sp, lib.loc = lib.path)
library(raster, lib.loc = lib.path)
library(rgdal, lib.loc = lib.path)
library(gdalUtils, lib.loc = lib.path)
library(rgeos, lib.loc = lib.path)
library(here, lib.loc = lib.path)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

## ASTER DATA:
# Coordinate System	Universal Transverse Mercator (UTM)
# Datum WGS84
# units: Kelvin
# 16-bit unsigned integer
# Fill value: N/A
# Valid range: 200 to 3200
# Scale Factor: 0.1

# create a list of lists that contain the .tif files by day
st.tile.list <- list.files(here("data/aster/raw"), recursive = T, full.names = T, pattern="tif$")

# create a raster stack from the list of lists
st.tile.stack <- lapply(st.tile.list, function(x) raster(x, layer = 1)) # make sure each element is a raster layer not a brick/stack

# convert surface temps to celcius with decimal. -273.15 C == 0 K.
# if a temp is 2000 (200 K, - 73 C), it is outside the scope and should be adjusted to NA
for(r in 1:length(st.tile.stack)){
  values(st.tile.stack[[r]]) <- ifelse(values(st.tile.stack[[r]]) == 2000, NA, (values(st.tile.stack[[r]]) - (273.15 * 10)) / 10)
}

# export all for selective processing in qgis
dir.create(here("data/aster/preprocessed"), showWarnings = FALSE) # creates output folder if it doesn't already exist
for(r in 1:length(st.tile.stack)){
  writeRaster(st.tile.stack[[r]], here("data/aster/preprocessed",paste0(names(st.tile.stack[[r]]),".tif")), overwrite = T)
}

