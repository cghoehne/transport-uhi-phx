#########################################
## PREP STATS FOR RASTERIZE HEAT MAPS  ##
#########################################

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
library(zoo)
library(lubridate)
library(data.table)
library(here)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

####
# import most recent pavement model run data
bg.surface.data <- readRDS(here("data/outputs/run_metadata_20190520_171637/all_pave_surface_data.rds"))[batch.name == "Bare Ground / Desert Soil",]
pave.surface.data <- readRDS(here("data/outputs/run_metadata_20190526_185113_varied_thick/all_pave_surface_data.rds")) # surface temporal data by run for pavements
all.surface.data <- rbind(bg.surface.data, pave.surface.data) # merge bare ground and pavement simulations seperatley (bare ground is rarely rerun)
unique(all.surface.data$pave.name)
unique(all.surface.data$SVF)

# SUMMARIZE PAVE MODEL SURFACE DATA TO ESTIMATE RELEASED HEAT
# for both average day and time of day corresponding to time fidelity of traffic data
# calc flux vars (W/m2)
all.surface.data[, inc.sol := ((1 - albedo) * SVF * solar)]
all.surface.data[, ref.sol := albedo * SVF * solar]
all.surface.data[, net.flux := q.rad + q.cnv - inc.sol]
all.surface.data[, out.flux := q.rad + q.cnv]

# fix 125 AO pavement name to actual modeled
all.surface.data[pave.name == "Asphalt Overlay on PCC 125+150mm", pave.name := "Asphalt Overlay on PCC 130+150mm"]

# force to static date for date.time for easier manipulation in ggplot, will ignore date
# NOTE: all summarized surface data is filtered previously to only last day of data
all.surface.data[, date.time := as.POSIXct("2019-01-01 00:00:00 MST") + hours(hrs) + minutes(mins) + seconds(secs)] 

# create list of date.times starting at static date to replicate division of time in travel data
my.date.times <- as.POSIXct("2019-01-01 00:00:00 MST") + minutes(round((1:100 - 0.5) * 24/100*60))

# filter heat model data to only time stamps in travel model output
surface.data.f <- all.surface.data[date.time %in% my.date.times]

# summarize data by type and time 
surface.data.m <-  surface.data.f[, .(min.out.flux = min(out.flux),
                                      mean.out.flux = mean(out.flux),
                                      med.out.flux = quantile(out.flux, 0.5),
                                      max.out.flux = max(out.flux)), by = c("pave.name", "date.time", "SVF")]

# merge bare ground data as columns for easy subraction to get added heat relative to ground
#surface.data.m <- merge(surface.data[new.name != "Bare Ground / Desert Soil",],
#                            surface.data[new.name == "Bare Ground / Desert Soil",],
#                            suffixes = c("", ".b"), by = c("date.time", "SVF"))

#surface.data.m[, min.add.flux := min.out.flux - min.out.flux.b]
#surface.data.m[, mean.add.flux := mean.out.flux - mean.out.flux.b]
#surface.data.m[, med.add.flux := med.out.flux - med.out.flux.b]
#surface.data.m[, max.add.flux := max.out.flux - max.out.flux.b]

# define custom OSM roadway groupings
highway <- list()
highway$class <- c("motorway") 
highway$a.pave.type <- c("Asphalt 200mm", "Asphalt Overlay on PCC 150+200mm")
highway$c.pave.type <- c("Whitetopped Asphalt 150+200mm", "Portland Cement Concrete 300mm")

major <- list()
major$class <-  c("primary", "trunk")
major$a.pave.type <- c("Asphalt 140mm", "Asphalt Overlay on PCC 130+150mm")
major$c.pave.type <- c("Whitetopped Asphalt 100+140mm", "Portland Cement Concrete 200mm")

minor <- list()
minor$class  <-  c("secondary", "tertiary")
minor$a.pave.type <- c("Asphalt 140mm", "Asphalt Overlay on PCC 100+100mm")
minor$c.pave.type <- c("Whitetopped Asphalt 80+80mm", "Portland Cement Concrete 100mm", "Portland Cement Concrete 200mm")

local <- list()
local$class  <-  c("residential", "service", "unclassified")
local$a.pave.type <- c("Asphalt 80mm")
local$c.pave.type <- c("Whitetopped Asphalt 80+80mm", "Portland Cement Concrete 100mm")

com.park <- list()
com.park$class  <- c("asph")
com.park$a.pave.type <- c("Asphalt 80mm")
com.park$c.pave.type <- c("Whitetopped Asphalt 80+80mm", "Portland Cement Concrete 100mm", "Portland Cement Concrete 200mm")

res.park <- list()
res.park$class  <- c("conc")
res.park$a.pave.type <- c("Asphalt 80mm")
res.park$c.pave.type <- c("Portland Cement Concrete 100mm", "Portland Cement Concrete 200mm")

unpaved <- list()
unpaved$class  <- c("unpaved")
unpaved$a.pave.type <- c("Bare Dry Soil #3", "Bare Dry Soil #4", "Bare Dry Soil #5")
unpaved$c.pave.type <- c("Bare Dry Soil #3", "Bare Dry Soil #4", "Bare Dry Soil #5")

fclass.meta <- list(highway, major, minor, local, com.park, res.park, unpaved)
names(fclass.meta) <- c("highway", "major", "minor", "local", "com.park", "res.park", "unpaved")

# assign MPGe and asphalt-to-concrete ratios by new OSM roadway groupings
pave.veh.meta <- data.table(pave.class = c("highway", "major", "minor", "local", "com.park", "res.park", "unpaved"), 
                            asph.min = c(0.9, rep(0.8, 3), 0.7, 0, 0),
                            asph.max = c(rep(1, 5), 0, 0),
                            MPGe.min = c(30, 20, 15, 15, NA, NA, NA),
                            MPGe.max = c(50, 30, 25, 25, NA, NA, NA),
                            SVF = rep(unique(all.surface.data$SVF)[1], 7))
pave.veh.meta <- rbind(pave.veh.meta, pave.veh.meta)
pave.veh.meta[8:14, SVF := unique(all.surface.data$SVF)[2]]

# create summary data.table for roadway class added heat flux by time of day
pave.time <- list(pave.class = c("highway", "major", "minor", "local", "com.park", "res.park", "unpaved"),
                  date.time = my.date.times, 
                  SVF = unique(all.surface.data$SVF))
pave.time.meta <- as.data.table(expand.grid(pave.time))

# by time of day heat flux by class for pavements
for(d in my.date.times){
  for(s in unique(all.surface.data$SVF)){
    pave.time.meta[SVF == s & date.time == d & pave.class == "highway", min.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "highway", asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% highway$a.pave.type, min.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "highway", 1 - asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% highway$c.pave.type, min.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "highway", mean.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "highway", (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% highway$a.pave.type, mean.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "highway", 1 - (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% highway$c.pave.type, mean.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "highway", med.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "highway", (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% highway$a.pave.type, med.out.flux], 0.5)) +
                     (pave.veh.meta[SVF == s & pave.class == "highway", 1 - (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% highway$c.pave.type, med.out.flux], 0.5))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "highway", max.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "highway", asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% highway$a.pave.type, max.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "highway", 1 - asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% highway$c.pave.type, max.out.flux]))]
    
    pave.time.meta[SVF == s & date.time == d & pave.class == "major", min.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "major", asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% major$a.pave.type, min.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "major", 1 - asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% major$c.pave.type, min.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "major", mean.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "major", (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% major$a.pave.type, mean.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "major", 1 - (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% major$c.pave.type, mean.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "major", med.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "major", (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% major$a.pave.type, med.out.flux], 0.5)) +
                     (pave.veh.meta[SVF == s & pave.class == "major", 1 - (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% major$c.pave.type, med.out.flux], 0.5))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "major", max.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "major", asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% major$a.pave.type, max.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "major", 1 - asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% major$c.pave.type, max.out.flux]))]
    
    pave.time.meta[SVF == s & date.time == d & pave.class == "minor", min.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "minor", asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% minor$a.pave.type, min.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "minor", 1 - asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% minor$c.pave.type, min.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "minor", mean.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "minor", (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% minor$a.pave.type, mean.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "minor", 1 - (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% minor$c.pave.type, mean.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "minor", med.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "minor", (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% minor$a.pave.type, med.out.flux], 0.5)) +
                     (pave.veh.meta[SVF == s & pave.class == "minor", 1 - (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% minor$c.pave.type, med.out.flux], 0.5))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "minor", max.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "minor", asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% minor$a.pave.type, max.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "minor", 1 - asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% minor$c.pave.type, max.out.flux]))]
    
    pave.time.meta[SVF == s & date.time == d & pave.class == "local", min.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "local", asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% local$a.pave.type, min.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "local", 1 - asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% local$c.pave.type, min.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "local", mean.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "local", (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% local$a.pave.type, mean.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "local", 1 - (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% local$c.pave.type, mean.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "local", med.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "local", (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% local$a.pave.type, med.out.flux], 0.5)) +
                     (pave.veh.meta[SVF == s & pave.class == "local", 1 - (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% local$c.pave.type, med.out.flux], 0.5))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "local", max.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "local", asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% local$a.pave.type, max.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "local", 1 - asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% local$c.pave.type, max.out.flux]))]
    
    pave.time.meta[SVF == s & date.time == d & pave.class == "com.park", min.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "com.park", asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% com.park$a.pave.type, min.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "com.park", 1 - asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% com.park$c.pave.type, min.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "com.park", mean.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "com.park", (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% com.park$a.pave.type, mean.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "com.park", 1 - (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% com.park$c.pave.type, mean.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "com.park", med.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "com.park", (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% com.park$a.pave.type, med.out.flux], 0.5)) +
                     (pave.veh.meta[SVF == s & pave.class == "com.park", 1 - (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% com.park$c.pave.type, med.out.flux], 0.5))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "com.park", max.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "com.park", asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% com.park$a.pave.type, max.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "com.park", 1 - asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% com.park$c.pave.type, max.out.flux]))]
    
    pave.time.meta[SVF == s & date.time == d & pave.class == "res.park", min.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "res.park", asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% res.park$a.pave.type, min.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "res.park", 1 - asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% res.park$c.pave.type, min.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "res.park", mean.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "res.park", (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% res.park$a.pave.type, mean.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "res.park", 1 - (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% res.park$c.pave.type, mean.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "res.park", med.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "res.park", (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% res.park$a.pave.type, med.out.flux], 0.5)) +
                     (pave.veh.meta[SVF == s & pave.class == "res.park", 1 - (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% res.park$c.pave.type, med.out.flux], 0.5))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "res.park", max.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "res.park", asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% res.park$a.pave.type, max.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "res.park", 1 - asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% res.park$c.pave.type, max.out.flux]))]
    
    pave.time.meta[SVF == s & date.time == d & pave.class == "unpaved", min.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "unpaved", asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% unpaved$a.pave.type, min.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "unpaved", 1 - asph.min] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% unpaved$c.pave.type, min.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "unpaved", mean.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "unpaved", (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% unpaved$a.pave.type, mean.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "unpaved", 1 - (asph.min + asph.max)/2] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% unpaved$c.pave.type, mean.out.flux]))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "unpaved", med.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "unpaved", (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% unpaved$a.pave.type, med.out.flux], 0.5)) +
                     (pave.veh.meta[SVF == s & pave.class == "unpaved", 1 - (asph.min + asph.max)/2] * quantile(surface.data.m[SVF == s & date.time == d & pave.name %in% unpaved$c.pave.type, med.out.flux], 0.5))]
    pave.time.meta[SVF == s & date.time == d & pave.class == "unpaved", max.out.flux := 
                     (pave.veh.meta[SVF == s & pave.class == "unpaved", asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% unpaved$a.pave.type, max.out.flux])) +
                     (pave.veh.meta[SVF == s & pave.class == "unpaved", 1 - asph.max] * mean(surface.data.m[SVF == s & date.time == d & pave.name %in% unpaved$c.pave.type, max.out.flux]))]
    }
}

pave.time.meta[SVF == 1.0 & pave.class == "highway", mean(min.out.flux)]
pave.time.meta[SVF == 1.0 & pave.class == "highway", mean(mean.out.flux)]
pave.time.meta[SVF == 1.0 & pave.class == "highway", mean(max.out.flux)]

# because low SVF simulated pavements have very low but highly sensitive outgoing heat flux dependent on the obs weather conditions,
# force the low SVF out.flux to a mean for all time periods to avoid noise in weather data
pave.time.meta[SVF == 0.1, min.out.flux := mean(min.out.flux), by = "pave.class"]
pave.time.meta[SVF == 0.1, mean.out.flux := mean(mean.out.flux), by = "pave.class"]
pave.time.meta[SVF == 0.1, med.out.flux := mean(med.out.flux), by = "pave.class"]
pave.time.meta[SVF == 0.1, max.out.flux := mean(max.out.flux), by = "pave.class"]

# merge bare ground data as columns for easy subraction to get added heat relative to ground
pave.time.meta.u <- merge(pave.time.meta,
                          pave.time.meta[pave.class == "unpaved",],
                          suffixes = c("", ".u"), by = c("date.time", "SVF"))
pave.time.meta.u[, min.add.flux := min.out.flux - min.out.flux.u]
pave.time.meta.u[, mean.add.flux := mean.out.flux - mean.out.flux.u]
pave.time.meta.u[, med.add.flux := med.out.flux - med.out.flux.u]
pave.time.meta.u[, max.add.flux := max.out.flux - max.out.flux.u]

# merge summary data 
pave.veh.meta <- merge(pave.veh.meta, pave.time.meta.u[, .(mean.day.out.flux.lwr = mean(min.out.flux),
                                                           mean.day.out.flux.upr = mean(max.out.flux)),
                                                       by = c("pave.class", "SVF")], by = c("pave.class", "SVF"))

# test plots
plot(pave.time.meta[SVF == 1.0 & pave.class == "highway", date.time], 
     pave.time.meta[SVF == 1.0 & pave.class == "highway", mean.out.flux], 
     type="l", col="black", ylab = "Mean Heat Flux (W/m2)", xlab = "Time of Day") #, ylim = c(0, 80)
lines(pave.time.meta[SVF == 1.0 & pave.class == "major", date.time], 
      pave.time.meta[SVF == 1.0 & pave.class == "major", mean.out.flux], col="red")
lines(pave.time.meta[SVF == 1.0 & pave.class == "minor", date.time], 
      pave.time.meta[SVF == 1.0 & pave.class == "minor", mean.out.flux], col="orange")
lines(pave.time.meta[SVF == 1.0 & pave.class == "local", date.time], 
      pave.time.meta[SVF == 1.0 & pave.class == "local", mean.out.flux], col="yellow")
lines(pave.time.meta[SVF == 1.0 & pave.class == "com.park", date.time], 
      pave.time.meta[SVF == 1.0 & pave.class == "com.park", mean.out.flux], col="blue")
lines(pave.time.meta[SVF == 1.0 & pave.class == "res.park", date.time], 
      pave.time.meta[SVF == 1.0 & pave.class == "res.park", mean.out.flux], col="green")
lines(pave.time.meta[SVF == 1.0 & pave.class == "unpaved", date.time], 
      pave.time.meta[SVF == 1.0 & pave.class == "unpaved", mean.out.flux], col="gray")

plot(pave.time.meta.u[SVF == 0.1 & pave.class == "highway", date.time], 
     pave.time.meta.u[SVF == 0.1 & pave.class == "highway", mean.add.flux], 
     type="l", col="black", ylab = "Mean Heat Flux (W/m2)", xlab = "Time of Day", ylim = c(0, 10)) #
lines(pave.time.meta.u[SVF == 0.1 & pave.class == "major", date.time], 
      pave.time.meta.u[SVF == 0.1 & pave.class == "major", mean.add.flux], col="red")
lines(pave.time.meta.u[SVF == 0.1 & pave.class == "minor", date.time], 
      pave.time.meta.u[SVF == 0.1 & pave.class == "minor", mean.add.flux], col="orange")
lines(pave.time.meta.u[SVF == 0.1 & pave.class == "local", date.time], 
      pave.time.meta.u[SVF == 0.1 & pave.class == "local", mean.add.flux], col="yellow")
lines(pave.time.meta.u[SVF == 0.1 & pave.class == "com.park", date.time], 
      pave.time.meta.u[SVF == 0.1 & pave.class == "com.park", mean.add.flux], col="blue")
lines(pave.time.meta.u[SVF == 0.1 & pave.class == "res.park", date.time], 
      pave.time.meta.u[SVF == 0.1 & pave.class == "res.park", mean.add.flux], col="green")


# save output data
saveRDS(fclass.meta, here("data/outputs/pavement-class-summary.rds"))
saveRDS(pave.veh.meta, here("data/outputs/pavement-vehicle-heat-metadata.rds"))
saveRDS(pave.time.meta.u, here("data/outputs/pavement-heat-time-metadata.rds"))
