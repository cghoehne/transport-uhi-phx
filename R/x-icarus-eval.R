# TEST ICARUS EVAL

# clear space and allocate memory
gc()
memory.limit(size = 50000) 
script.start <- Sys.time() # start script timestamp
options(scipen = 999)

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
.libPaths(paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1"))
library(doParallel)
library(foreach)
library(sf)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(data.table)
library(here)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 



# load vehicle travel network and data and convert to sf
traffic.net <- readRDS(here("data/outputs/network/icarus-network.rds")) # cleaned/simplified ICARUS traffic network to pair with traffic data
veh <- st_as_sf(traffic.net)

# calculate acutal link length and length reduction ratio to ensure accurate VKT estaimtes
veh$adj.length <- st_length(veh)
veh$adj.l.ratio <- veh$length / veh$adj.length 
#quantile(veh$adj.l.ratio, c(0, 0.50, 0.75, 0.90, 0.95, 0.99, 0.999, 0.9999, 1.0))

# ICARUS traffic data has 101 cols where col 1 is link id and the rest are from 12am to 12am divided in 100 chunks
# therefore every column from V2:V101 represents a 0.24 hour period
# morning rush would be from: hour = 7.92 am to 8.88 am (6:54:12 am to 8:52:48 am); V34:V37
# evening rush would be from: hour = 5.04 pm  to 6 pm (5:02:24 pm to 6:00:00 pm); V72:V75

# merge icarus flow data by link id
iflow <- fread(here("data/icarus/full_flow.csv"))
setnames(iflow, "V1", "id")
veh.m <- merge(veh, iflow, by = "id")
rm(veh)


# look at specific link "593279-592712-633804-633723"
# westbound 202 link, just west of 202 - 143, north of airport.
my.id <- "593279-592712-633804-633723"

# raw link length
veh[veh$id == my.id,]$length

# adjustment factor
veh[veh$id == my.id,]$adj.l.ratio

# daily vehicle flow across link
sum(iflow[id == my.id, V2:V101])
# 2017 ADOT AADT: 70,142, ~6,000 trucks

sum(iflow[id == my.id, V2:V101])/64000

# morning and evening rush hour approx veh/hr
sum(sum(iflow[id == my.id, V34:V37]))
sum(sum(iflow[id == my.id, V72:V75]))

# actual lanes: 5 (4 permenant through)
# actual estiamted capacity: 20000 veh/hr/ln = 10,000 veh/hr
# given capacity:
veh[veh$id == my.id,]$capacity

# test res = 250m
res <- 820.21  # ~250m x 250m

# total km of link
# 3281 ft = 1 km
veh[veh$id == my.id,]$length * veh[veh$id == my.id,]$adj.l.ratio / 3281

# veh heat data
pave.veh.meta <- readRDS(here("data/outputs/pavement-vehicle-heat-metadata.rds"))

# assume 8800  Wh/liter (33.3 kWh/gal)
# 1 MPG = 0.425144 km/liter or 1 gal/mi = 2.35215 liter/km
# energy lost to waste heat assumed = 0.65 (65%)
pave.veh.meta[,energy.min := (1 / MPGe.min) * 2.35215 * 8800 * 0.65] # Wh/km (Watt-hours per kilometer)
pave.veh.meta[,energy.max := (1 / MPGe.max) * 2.35215 * 8800 * 0.65] # Wh/km (Watt-hours per kilometer)

pave.veh.meta[pave.class == "highway" & SVF == 1.0, energy.max] / 24 / ((res / 3.28084)^2)


## 
# SCOTSSDALE 101 btwn CHAPARRAL and McDONNALD

# NORTH
# ADOT 2017 AADT: 88,765 vehicles, ~6,000 trucks
my.id <- "184910"
sum(iflow[id == my.id, V2:V101])
sum(iflow[id == my.id, V2:V101]) / 83000

# SOUTH
# ADOT 2017 AADT: 88,245 vehicles, ~6,000 trucks
my.id <- "184899"
sum(iflow[id == my.id, V2:V101])
sum(iflow[id == my.id, V2:V101]) / 82000

##
# PHOENIX I-17 btwn ADAMS and GRANT

# NORTH & SOUTH
# ADOT 2017 AADT: 58,151 vehicles, ~4,000 trucks
my.id <- "585472"
sum(iflow[id == my.id, V2:V101])
sum(iflow[id == my.id, V2:V101]) / 56000
my.id <- "678587"
sum(iflow[id == my.id, V2:V101])
sum(iflow[id == my.id, V2:V101]) / 56000

v.m <- as.data.table(veh.m)
lapply(v.m, class)
v.m[, adj.length := as.numeric(as.character(adj.length))]
v.m[, adj.l.ratio := as.numeric(as.character(adj.l.ratio))]

all.flow <- paste0("V",2:101)

v.m.t <- v.m[, .SD, .SDcols = c("id", all.flow)]
v.m.t[, flow := sum(.SD), .SDcols = all.flow, by = id]

v.m.t <- merge(v.m.t, v.m[, .(id, length, freespeed, capacity, adj.l.ratio)], by = "id")
v.m.t[, vmt := flow * length / 5280]

# total VMT
sum(v.m.t[, vmt])
# ~10,000 VMT/capita * ~4 million population phoenix UZA
# ~112 million VMT in phoenix estimate

# merge new data to shapefile of network and save
traffic.net <- merge(traffic.net, v.m.t[, .(id, flow, vmt, adj.l.ratio)], by = "id")
shapefile(traffic.net, here("data/outputs/shapefiles/icarus-network-flows"))
