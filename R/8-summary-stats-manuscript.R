# PAPER SUMMARY STATS

# clear space and allocate memory
gc()
memory.limit(size = 56000) 
options(scipen = 999) # prevent scientific notation when printing

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(sf)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(raster)
library(zoo, lib.loc = lib.path, quietly = T)
library(lubridate, lib.loc = lib.path, quietly = T)
library(data.table, lib.loc = lib.path, quietly = T)
library(here, lib.loc = lib.path, quietly = T)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # archive date for all used packages (besides checkpoint itself!)
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

RMSE = function(m, o){sqrt(mean((m - o)^2, na.rm = T))}

# define folder for data reterival (automaticlly take most recent folder with "run_metadata" in it)
folder <- as.data.table(file.info(list.dirs(here("data/outputs/"), recursive = F)), 
                        keep.rownames = T)[grep("run_metadata", rn),][order(ctime)][.N, rn]

# MODEL METADATA
all.valid.model.runs <- readRDS(here("data/outputs/run_metadata_20190324_185458/stats_all_model_runs.rds"))
all.model.runs.7d <- readRDS(here("data/outputs/run_metadata_20190520_171637/stats_all_model_runs.rds"))
all.model.runs.th <- readRDS(here("data/outputs/run_metadata_20190526_185113_varied_thick/stats_all_model_runs.rds"))
all.model.runs.TI <- readRDS(here("data/outputs/run_metadata_20190524_112541_varied_TI/stats_all_model_runs.rds"))

# unique dates
length(unique(all.model.runs[, end.day])) # total n
sort(unique(all.model.runs[, end.day])) # sorted

# all validation model runs RMSE
# for validation only, drop unrealistic/bad predictors and high volume pavements as they are not representative
valid.model.names <- sort(unique(all.valid.model.runs$pave.name))
valid.model.runs <- all.valid.model.runs[p.err <= 0.30 & is.finite(p.err) & RMSE(Modeled, Observed) <= 10,] # remove poor performers or NAs if there are any 
valid.model.runs <- valid.model.runs[!(pave.name %in% valid.model.names[7:12])]
valid.model.runs[, RMSE(Modeled, Observed)]
valid.model.runs[, mean(p.err, na.rm = T)]

valid.model.runs[, mean(albedo), by = pave.name]

# min, avg, max of temps at nearest node to 1.0 meter depth for all runs
all.model.runs[, .(min.T_1.0m = min(min.T_1.0m, na.rm = T),
                   avg.T_1.0m = mean(avg.T_1.0m, na.rm = T),
                   max.T_1.0m = max(max.T_1.0m, na.rm = T))]
# same but by pavement 
all.model.runs[, .(min.T_1.0m = min(min.T_1.0m, na.rm = T),
                   avg.T_1.0m = mean(avg.T_1.0m, na.rm = T),
                   max.T_1.0m = max(max.T_1.0m, na.rm = T)),
               by = c("pave.name")][order(avg.T_1.0m)]


# WEATHER
#import data
#valid.dates <- readRDS(here("data/aster/my-aster-data.rds")) # remote sensed temps at valiation sites on specified dates
#my.years <- unique(valid.dates[, year(date.time)]) # store all unique years to reterive weather data for those years
#weather.raw <- rbindlist(lapply(here(paste0("data/mesowest/", my.years, "-meso-weather-data.rds")), readRDS), fill = T) # bind all weather data for the selected years

#colnames(weather.raw)
#weather.raw[, .(mean.winspd = mean(winspd, na.rm = T)), by = hour(date.time)][order(hour)]
#weather.raw[, .(median.winspd = quantile(winspd, probs = 0.5, na.rm = T)), by = hour(date.time)][order(hour)]
#weather.raw[, .(mean.temp.c = mean(temp.c, na.rm = T)), by = hour(date.time)][order(hour)]


# PAVEMENT SURFACE
all.surface.data <- readRDS(paste0(folder, "/all_pave_surface_data.rds"))
all.surface.data.7d <- readRDS(here("data/outputs/run_metadata_20190520_171637/all_pave_surface_data.rds"))
all.surface.data.th <- readRDS(here("data/outputs/run_metadata_20190526_185113_varied_thick/all_pave_surface_data.rds"))
all.surface.data.TI <- readRDS(here("data/outputs/run_metadata_20190524_112541_varied_TI/all_pave_surface_data.rds"))

# add daytime factor
all.surface.data[, daytime := factor(ifelse(hour(date.time) %in% c(7:18), "Day", "Night"), 
                                     levels = c("Day","Night"))]

# max day reduced heat flux by for high/low thermal inertia pavements by type
all.surface.data.TI.a <- all.surface.data.TI[, .(batch.name = batch.name,
                                                 pave.name = pave.name,
                                                 out.flux = mean(q.rad + q.cnv)),
                                             by = c("date.time", "pave.name", "SVF")]  
all.surface.data.TI.a <- unique(all.surface.data.TI.a[SVF == 1.0,])

all.surface.data.TI.a[, max(out.flux), by = pave.name]
all.surface.data.TI.a[, min(out.flux), by = pave.name]

# differences in outgoing flux by thickness/pave type
all.surface.data.th.a <- all.surface.data.th[, .(batch.name = batch.name,
                                                 pave.name = pave.name,
                                                 out.flux = mean(q.rad + q.cnv)),
                                             by = c("date.time", "pave.name", "SVF")]  
all.surface.data.th.a <- unique(all.surface.data.th.a[SVF == 1.0,])

all.surface.data.th.a[, max(out.flux), by = pave.name]
all.surface.data.th.a[, min(out.flux), by = pave.name]



# calc pavement thickness (exlude subbase/subgrade)

# min/max temp linear regression to variables of interest
# define interest vars
i.vars <- c("pave.thick", "L1.albedo", "L1.emissivity", "mean.k", "mean.c", "mean.rho")
a.vars <- c("batch.name", "run.n", i.vars)
s.vars <- c("pave.name", "batch.name", "run.n", "date.time", "albedo", "SVF", "daytime", "season","q.rad", "q.cnv", "T.degC")
lm.results <- data.table("var" = i.vars)
lm.data <- merge(all.surface.data[, ..s.vars], all.model.runs[, ..a.vars], by = c("batch.name", "run.n"), allow.cartesian = T)
lm.data <- lm.data[batch.name != "Bare Ground / Desert Soil",] # exlude bare ground for analysis of pavement properties only

for(i.var in i.vars){
  i.var.id <- c("pave.name", i.var)
  # min/max surface temp by variable of interest
  max.min.t <- lm.data[, .(max.surf.t = max(T.degC, na.rm = T),
                                    min.surf.t = min(T.degC, na.rm = T)), 
                                by = i.var.id]
  # models
  lm.min <- lm(paste("min.surf.t ~", i.var), max.min.t)
  lm.max <- lm(paste("max.surf.t ~", i.var), max.min.t)
  
  lm.results[var == i.var, min.T.lwr := signif(confint(lm.min, level = 0.95)[2,1], 2)]
  lm.results[var == i.var, min.T.coef := signif(lm.min$coefficients[2], 2)]
  lm.results[var == i.var, min.T.upr := signif(confint(lm.min, level = 0.95)[2,2], 2)]
  lm.results[var == i.var, min.R.sq := signif(summary(lm.min)$r.squared, 2)]
  lm.results[var == i.var, min.p.coef := signif(summary(lm.min)$coefficients[2,4], 2)]
  
  lm.results[var == i.var, max.T.lwr := signif(confint(lm.max, level = 0.95)[2,1], 2)]
  lm.results[var == i.var, max.T.coef := signif(lm.max$coefficients[2], 2)]
  lm.results[var == i.var, max.T.upr := signif(confint(lm.max, level = 0.95)[2,2], 2)]
  lm.results[var == i.var, max.R.sq := signif(summary(lm.max)$r.squared, 2)]
  lm.results[var == i.var, max.p.coef := signif(summary(lm.max)$coefficients[2,4], 2)]
}

lm.results[,.(var, min.T.coef, min.p.coef, max.T.coef, max.p.coef)]

# flux comparisons by type
all.surface.data[, .(mean.out.flux = mean(q.rad + q.cnv)), by = c("pave.name", "batch.name", "albedo")][order(mean.out.flux)]

# convection
all.surface.data[, .(med.q.cnv = quantile(q.cnv, probs = 0.5, na.rm = T)), by = c("pave.name", "batch.name", "albedo")][order(med.q.cnv)]
all.surface.data[, .(max.q.cnv = max(q.cnv, na.rm = T)), by = c("pave.name", "batch.name", "albedo")][order(max.q.cnv)]

all.surface.data[, max(q.cnv, na.rm = T), by = c("daytime")][order(V1)]
all.surface.data[, min(q.cnv, na.rm = T), by = c("daytime")][order(V1)]

# IR
all.surface.data[, max(q.rad, na.rm = T), by = c("daytime")][order(V1)]
all.surface.data[, min(q.rad, na.rm = T), by = c("daytime")][order(V1)]

# which max out.flux
all.surface.data[,max(q.rad), by = c("pave.name", "batch.name", "albedo")][order(V1)]


# define name of run
run.name <- "metro-phx"
#run.name <- "phx-dwntwn"
#run.name <- "north-tempe"

# define resolution 
#res <- 164.042  #  ~50m x 50m
#res <- 328.084  # ~100m x 100m
res <- 820.21  # ~250m x 250m
#res <- 1640.42 # ~500m x 500m
#res <- 3280.84 # ~1000 x 1000 

# import veh hourly raster data
veh.heat <- stack(here(paste0("data/outputs/rasters/master-veh-time-heat-", run.name, "-", res / 3.28084, "m.tif")))

# import daily avg raster data
r.all <- stack(here(paste0("data/outputs/rasters/master-pave-veh-heat-", run.name, "-", res / 3.28084, "m.tif")))
names(r.all) <- readRDS(here(paste0("data/outputs/rasters/master-veh-time-heat-", run.name, "-", res / 3.28084, "m-names.rds")))

aheat <- readRDS(here("data/outputs/all-heat-time-summarized.rds"))

# quantiles of raster
quants <- lapply(1:length(names(r.all)), function(i) quantile(values(
  r.all[[i]]), c(0, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1.0), na.rm = T))
names(quants) <- names(r.all)


# ICARUS TRAFFIC + OSM NETWORK ANALYSIS

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

# freespeed quantiles
quantile(veh.m$freespeed)


# look at specific link "593279-592712-633804-633723"
# westbound 202 link, just west of 202 - 143, north of airport.
my.id <- "593279-592712-633804-633723"

# raw link length
veh.m[veh.m$id == my.id,]$length

# adjustment factor
veh.m[veh.m$id == my.id,]$adj.l.ratio

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
veh.m[veh.m$id == my.id,]$capacity

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
#lapply(v.m, class)
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



