# raw parking data format


# clear space and allocate memory
gc()
memory.limit(size = 56000) 
script.start <- Sys.time() # start script timestamp

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
.libPaths(paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1"))
#lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1") # lib.loc = lib.path, 
library(sp)
library(raster)
library(data.table)
library(here)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 


# import raw parking data
parking <- fread(here("data/outputs/temp/parking/phx-parking-blkgrp.csv")) # phoenix total parking data (aggregated to blockgroup id)
parking.par <- readRDS(here("data/outputs/temp/parking/phoenix-parking-parcel.rds")) # phoenix off-street parking data (raw by parcel)
parking.pts <- shapefile(here("data/outputs/temp/parking/phx-parcel-points.shp")) # points of center of APN of parcels

# get data from point data
parking <- as.data.table(parking.pts@data)

# merge to get coords
parking <- merge(parking, parking.par, by = "APN")

# aggregate to unique APNs average the coords and spaces if multiple records exists for same APN
parking.a <- parking[, .(spaces = mean(spaces, na.rm = T),
                         X = mean(xcoord, na.rm = T),
                         Y = mean(ycoord, na.rm = T),
                         type = first(PROPTYPE)),
                     by = "APN"]

# shorten property type
parking.a[, type := as.character(type)]
parking.a[type == "Non-residential Off-street Spaces", type := "com"]
parking.a[type == "Residential Off-street Spaces", type := "res"]

# check everything looks as expected
table(parking.a$type)
parking[,.N]
parking.a[,.N]
length(unique(parking$APN))
length(parking.a$APN)
length(unique(parking.a$APN))

# save
saveRDS(parking.a, here("data/parking/phx-parking.rds"))
