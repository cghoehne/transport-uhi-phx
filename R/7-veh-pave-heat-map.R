######################
## HEAT MAP FIGURE  ##
######################

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
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(XML, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(sp, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(raster, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(rgdal, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(rgeos, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(maptools, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(gdalUtils, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(data.table, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(here, lib.loc = lib.path, quietly = T, warn.conflicts = F)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

# import data
blkgrp <- shapefile(here("data/shapefiles/boundaries/phx-blkgrp-geom.shp")) # census blockgroup shapfile (clipped to Maricopa UZA)
osm.block.min <- readRDS(here("data/outputs/osm-blockgroup-dissolved-min.rds"))
osm.block.max <- readRDS(here("data/outputs/osm-blockgroup-dissolved-max.rds"))
parking <- fread(here("data/phx-parking-blkgrp.csv")) # import parking data

# import most recent pavement model runs
# define folder for data reterival (automaticlly take most recent folder with "run_metadata" in it)
folder <- as.data.table(file.info(list.dirs(here("data/outputs/"), recursive = F)), 
                        keep.rownames = T)[grep("run_metadata", rn),][order(ctime)][.N, rn]
all.model.runs <- readRDS(paste0(folder, "/stats_all_model_runs.rds"))

# ROADS
# min road area by blockgroup
blkgrp$min.road.area.sqf <- gArea(osm.block.min, byid = T)
blkgrp$tot.area.sqf <- gArea(blkgrp, byid = T)
blkgrp$min.road.pct <- blkgrp$min.road.area.sqf / blkgrp$tot.area.sqf 

mean(blkgrp$min.road.pct)
max(blkgrp$min.road.pct)

# max road area by blockgroup
blkgrp$max.road.area.sqf <- gArea(osm.block.max, byid = T)
blkgrp$max.road.pct <- blkgrp$max.road.area.sqf / blkgrp$tot.area.sqf 

# min/mean/max total percent of area covered by roads
sum(blkgrp$min.road.area.sqf) / sum(blkgrp$tot.area.sqf)
(sum(blkgrp$min.road.area.sqf) + sum(blkgrp$max.road.area.sqf)) / 2 / sum(blkgrp$tot.area.sqf)
sum(blkgrp$max.road.area.sqf) / sum(blkgrp$tot.area.sqf)


# PARKING
# merge parking data to blkgrp shapefile
blkgrp <- merge(blkgrp, parking[, .(fid, res.off, com.off)], by = "fid", duplicateGeoms = T)  # ignore on-street spaces for area calcs because part of roadway

# assumed upper and lower area allocated per space
blkgrp$min.park.area.sqf <- (blkgrp$com.off * 250) + (blkgrp$res.off * 200)
blkgrp$max.park.area.sqf <- (blkgrp$com.off * 331) + (blkgrp$res.off * 500)

# min/mean/max total percent of area covered by roads
sum(blkgrp$min.park.area.sqf, na.rm = T) / sum(blkgrp$tot.area.sqf, na.rm = T)
(sum(blkgrp$min.park.area.sqf, na.rm = T) + sum(blkgrp$max.park.area.sqf, na.rm = T)) / 2 / sum(blkgrp$tot.area.sqf, na.rm = T)
sum(blkgrp$max.park.area.sqf, na.rm = T) / sum(blkgrp$tot.area.sqf, na.rm = T)
