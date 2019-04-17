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
.libPaths(paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1"))
#lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1") # lib.loc = lib.path, 
library(sp)
library(sf)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(gdalUtils)
library(doParallel)
library(data.table)
library(here)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

# import data
blkgrp <- shapefile(here("data/shapefiles/boundaries/phx-blkgrp-geom.shp")) # census blockgroup shapfile (clipped to Maricopa UZA)
uza.buffer <- readRDS(here("data/outputs/temp/uza-buffer.rds")) # Maricopa UZA buffered ~1mi

# import most recent pavement model run data
# define folder for data reterival (automaticlly take most recent folder with "run_metadata" in it)
folder <- as.data.table(file.info(list.dirs(here("data/outputs/"), recursive = F)), 
                        keep.rownames = T)[grep("run_metadata", rn),][order(ctime)][.N, rn]
all.model.runs <- readRDS(paste0(folder, "/stats_all_model_runs.rds")) # meta data by run
all.surface.data <- readRDS(paste0(folder, "/all_pave_surface_data.rds")) # surface temporal data by run

# ROAD AREA BY BLOCKGROUP
# min road area by blockgroup
blkgrp$min.road.area.sqf <- gArea(osm.block.min, byid = T)
blkgrp$tot.area.sqf <- gArea(blkgrp, byid = T)
blkgrp$min.road.pct <- blkgrp$min.road.area.sqf / blkgrp$tot.area.sqf 

# road area by raster cell (diff way)

mean(blkgrp$min.road.pct)
max(blkgrp$min.road.pct)

# max road area by blockgroup
blkgrp$max.road.area.sqf <- gArea(osm.block.max, byid = T)
blkgrp$max.road.pct <- blkgrp$max.road.area.sqf / blkgrp$tot.area.sqf 

# min/mean/max total percent of area covered by roads
sum(blkgrp$min.road.area.sqf) / sum(blkgrp$tot.area.sqf)
(sum(blkgrp$min.road.area.sqf) + sum(blkgrp$max.road.area.sqf)) / 2 / sum(blkgrp$tot.area.sqf)
sum(blkgrp$max.road.area.sqf) / sum(blkgrp$tot.area.sqf)

# calcualte total and fractional area of each fclass grouping 
hwy <- c("motorway") 
maj <-  c("primary", "trunk")
min <-  c("secondary", "tertiary")
maj <-  c("residential", "service", "unclassified")

#  * 0.092903 m2 per ft2
blkgrp$avg_hwy_area <- 0.1 * (blkgrp$min.road.area.sqf + blkgrp$max.road.area.sqf) / 2 * 0.092903
blkgrp$avg_maj_area <- 0.2 * (blkgrp$min.road.area.sqf + blkgrp$max.road.area.sqf) / 2 * 0.092903
blkgrp$avg_min_area <- 0.3 * (blkgrp$min.road.area.sqf + blkgrp$max.road.area.sqf) / 2 * 0.092903
blkgrp$avg_col_area <- 0.4 * (blkgrp$min.road.area.sqf + blkgrp$max.road.area.sqf) / 2 * 0.092903

#blkgrp$avg_hwy_frac <- blkgrp$avg_hwy_area / blkgrp$tot.area.sqf
#blkgrp$avg_maj_frac <- blkgrp$avg_maj_area / blkgrp$tot.area.sqf
#blkgrp$avg_min_frac <- blkgrp$avg_min_area / blkgrp$tot.area.sqf
#blkgrp$avg_col_frac <- blkgrp$avg_col_area / blkgrp$tot.area.sqf

# PARKING AREA BY BLOCKGROUP
# merge parking data to blkgrp shapefile
blkgrp <- merge(blkgrp, parking[, .(fid, res.off, com.off)], by = "fid", duplicateGeoms = T)  # ignore on-street spaces for area calcs because part of roadway

# assumed upper and lower area allocated per space
blkgrp$min.park.area.sqf <- (blkgrp$com.off * 250) + (blkgrp$res.off * 200)
blkgrp$max.park.area.sqf <- (blkgrp$com.off * 331) + (blkgrp$res.off * 500)

# min/mean/max total percent of area covered by roads
sum(blkgrp$min.park.area.sqf, na.rm = T) / sum(blkgrp$tot.area.sqf, na.rm = T)
(sum(blkgrp$min.park.area.sqf, na.rm = T) + sum(blkgrp$max.park.area.sqf, na.rm = T)) / 2 / sum(blkgrp$tot.area.sqf, na.rm = T)
sum(blkgrp$max.park.area.sqf, na.rm = T) / sum(blkgrp$tot.area.sqf, na.rm = T)

# SUMMARIZE MODEL SURFACE DATA

# calc flux vars (W/m2)
all.surface.data[, inc.sol := ((1 - albedo) * SVF * solar)]
all.surface.data[, ref.sol := albedo * SVF * solar]
all.surface.data[, net.flux := -inc.sol + q.rad + q.cnv]

# calculate total energy by average day in season by batch in MJ / m2 (1 MJ == 1E6 J = 1E6 W * s)
delta.t <- 30
surface.data.a <- all.surface.data[, .(out.heat = delta.t * sum(q.rad + q.cnv) / 1E6,
                                       inc.sol = delta.t * sum(inc.sol) / 1E6,
                                       net.heat = delta.t * sum(net.flux) / 1E6,
                                       ref.heat = delta.t * sum(ref.sol) / 1E6),
                                   by = c("batch.name", "season")]  
ground.heat <- surface.data.a[batch.name == "Bare Ground / Desert Soil", .(season,out.heat)]
setnames(ground.heat, "out.heat", "out.ground.heat")
surface.data.a <- merge(surface.data.a, ground.heat, by = "season")
surface.data.a[, added.heat := out.heat - out.ground.heat] # MJ / m2

# define roadway ratios to apply **TEMPORARY** WILL BE DEFINED LATER BY % AREA OF OSM BY FCLASS


surface.data.a <- merge(surface.data.a, data.table(ratio = c(0.30, 0.00, 0.00, 0.70), 
                                                   batch.name = unique(surface.data.a[, batch.name])),
                        by = "batch.name")

# temp avg day factor for all pave types
avg.day.heat <- sum(surface.data.a[, .(day.heat = mean(added.heat * ratio)), by = "batch.name"][ ,day.heat])

# average daily added heat (over undevelopedbare ground )
# in GJ / day / blkgrp
blkgrp$avg_hwy_day_heat <- blkgrp$avg_hwy_area * avg.day.heat / 1000
blkgrp$avg_maj_day_heat <- blkgrp$avg_maj_area * avg.day.heat / 1000
blkgrp$avg_min_day_heat <- blkgrp$avg_min_area * avg.day.heat / 1000
blkgrp$avg_col_day_heat <- blkgrp$avg_col_area * avg.day.heat / 1000

blkgrp$avg_day_heat_total <- blkgrp$avg_hwy_day_heat + blkgrp$avg_maj_day_heat + blkgrp$avg_min_day_heat + blkgrp$avg_col_day_heat
blkgrp$hectares <- blkgrp$tot.area.sqf * 9.2903E-6
blkgrp$avg_day_heat_GJha <- blkgrp$avg_day_heat_total / blkgrp$hectares

# output data
shapefile(blkgrp, here("data/outputs/osm-blkgrp-heat-working"), overwrite = T)
