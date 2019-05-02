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
library(data.table)
library(here)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

####
# import most recent pavement model run data
# define folder for data reterival (automaticlly take most recent folder with "run_metadata" in it)
folder <- as.data.table(file.info(list.dirs(here("data/outputs/"), recursive = F)), 
                        keep.rownames = T)[grep("run_metadata", rn),][order(ctime)][.N, rn]
all.model.runs <- readRDS(paste0(folder, "/stats_all_model_runs.rds")) # meta data by run
all.surface.data <- readRDS(paste0(folder, "/all_pave_surface_data.rds")) # surface temporal data by run
unique(all.surface.data$pave.name)
unique(all.surface.data$SVF)

# define custom OSM roadway groupings
highway <- list()
highway$class <- c("motorway") 
highway$a.pave.type <- c("Asphalt #5", "Asphalt Overlay on PCC #5")
highway$c.pave.type <- c("Whitetopped Asphalt #5", "Portland Cement Concrete #5")

major <- list()
major$class <-  c("primary", "trunk")
major$a.pave.type <- c("Asphalt #4", "Asphalt Overlay on PCC #4")
major$c.pave.type <- c("Whitetopped Asphalt #4", "Portland Cement Concrete #4")

minor <- list()
minor$class  <-  c("secondary", "tertiary")
minor$a.pave.type <- c("Asphalt #3", "Asphalt Overlay on PCC #3", "Asphalt #4", "Asphalt Overlay on PCC #4")
minor$c.pave.type <- c("Whitetopped Asphalt #3", "Portland Cement Concrete #3", "Whitetopped Asphalt #4", "Portland Cement Concrete #4")

local <- list()
local$class  <-  c("residential", "service", "unclassified")
local$a.pave.type <- c("Asphalt #3", "Asphalt Overlay on PCC #3")
local$c.pave.type <- c("Whitetopped Asphalt #3", "Portland Cement Concrete #3")

com.park <- list()
com.park$class  <- c("asph")
com.park$a.pave.type <- c("Asphalt #3")
com.park$c.pave.type <- c("Whitetopped Asphalt #3", "Portland Cement Concrete #3")

res.park <- list()
res.park$class  <- c("conc")
res.park$a.pave.type <- c("Asphalt #3")
res.park$c.pave.type <- c("Portland Cement Concrete #3")

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


# SUMMARIZE PAVE MODEL SURFACE DATA TO ESTIMATE RELEASED HEAT
# calc flux vars (W/m2)
all.surface.data[, inc.sol := ((1 - albedo) * SVF * solar)]
all.surface.data[, ref.sol := albedo * SVF * solar]
all.surface.data[, net.flux := -inc.sol + q.rad + q.cnv]

all.surface.data[,.(mean.day.net.flux = mean(net.flux, na.rm = T),
                   mean.day.out.flux = mean(q.rad + q.cnv)),
                 by = "batch.name" ]

all.surface.data[,.(mean.day.net.flux = mean(net.flux, na.rm = T),
                    mean.day.out.flux = mean(q.rad + q.cnv)),
                 by = c("batch.name","daytime")][order(-mean.day.out.flux)]

all.surface.data[,.(mean.net.flux = mean(net.flux, na.rm = T),
                    mean.out.flux = mean(q.rad + q.cnv)),
                 by = c("batch.name","hrs")][order(-mean.out.flux)]

# CALCULATE MEAN DAILY RELEASED (OUT) SENSIBLE PAVEMENT HEAT ENERGY FACTORS (W/M^2)
for(s in unique(all.surface.data$SVF)){
  # highway
  pave.veh.meta[SVF == s & pave.class == "highway", mean.day.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% highway$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% highway$c.pave.type, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "highway", mean.day.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% highway$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% highway$c.pave.type, mean(q.rad + q.cnv)]]
  # major
  pave.veh.meta[SVF == s & pave.class  == "major", mean.day.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% major$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% major$c.pave.type, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "major", mean.day.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% major$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% major$c.pave.type, mean(q.rad + q.cnv)]]
  # minor
  pave.veh.meta[SVF == s & pave.class  == "minor", mean.day.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% minor$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% minor$c.pave.type, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "minor", mean.day.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% minor$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% minor$c.pave.type, mean(q.rad + q.cnv)]]
  # local
  pave.veh.meta[SVF == s & pave.class  == "local", mean.day.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% local$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% local$c.pave.type, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "local", mean.day.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% local$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% local$c.pave.type, mean(q.rad + q.cnv)]]
  # commerical parking lots
  pave.veh.meta[SVF == s & pave.class  == "com.park", mean.day.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% com.park$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% com.park$c.pave.type, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "com.park", mean.day.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% com.park$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% com.park$c.pave.type, mean(q.rad + q.cnv)]]
  # residential parking (driveways)
  pave.veh.meta[SVF == s & pave.class  == "res.park", mean.day.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% res.park$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% res.park$c.pave.type, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "res.park", mean.day.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% res.park$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% res.park$c.pave.type, mean(q.rad + q.cnv)]]
  # unpaved
  pave.veh.meta[SVF == s & pave.class  == "unpaved", mean.day.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% unpaved$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% unpaved$c.pave.type, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "unpaved", mean.day.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% unpaved$a.pave.type, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% unpaved$c.pave.type, mean(q.rad + q.cnv)]]
  
  # CALCULATE 5PM DAILY RELEASED (OUT) SENSIBLE PAVEMENT HEAT ENERGY FACTORS (W/M^2)
  # highway
  pave.veh.meta[SVF == s & pave.class  == "highway", mean.5pm.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% highway$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% highway$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "highway", mean.5pm.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% highway$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% highway$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  # major
  pave.veh.meta[SVF == s & pave.class  == "major", mean.5pm.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% major$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% major$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "major", mean.5pm.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% major$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% major$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  # minor
  pave.veh.meta[SVF == s & pave.class  == "minor", mean.5pm.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% minor$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% minor$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "minor", mean.5pm.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% minor$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% minor$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  # local
  pave.veh.meta[SVF == s & pave.class  == "local", mean.5pm.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% local$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% local$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "local", mean.5pm.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% local$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% local$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  # commerical parking lots
  pave.veh.meta[SVF == s & pave.class  == "com.park", mean.5pm.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% com.park$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% com.park$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "com.park", mean.5pm.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% com.park$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% com.park$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  # residential parking (driveways)
  pave.veh.meta[SVF == s & pave.class  == "res.park", mean.5pm.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% res.park$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% res.park$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "res.park", mean.5pm.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% res.park$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% res.park$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  # unpaved
  pave.veh.meta[SVF == s & pave.class  == "unpaved", mean.5pm.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% unpaved$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% unpaved$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "unpaved", mean.5pm.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% unpaved$a.pave.type & hrs == 17, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% unpaved$c.pave.type & hrs == 17, mean(q.rad + q.cnv)]]
  
  # CALCULATE 8AM DAILY RELEASED (OUT) SENSIBLE PAVEMENT HEAT ENERGY FACTORS (W/M^2)
  # highway
  pave.veh.meta[SVF == s & pave.class  == "highway", mean.8am.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% highway$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% highway$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "highway", mean.8am.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% highway$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% highway$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  # major
  pave.veh.meta[SVF == s & pave.class  == "major", mean.8am.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% major$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% major$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "major", mean.8am.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% major$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% major$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  # minor
  pave.veh.meta[SVF == s & pave.class  == "minor", mean.8am.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% minor$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% minor$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "minor", mean.8am.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% minor$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% minor$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  # local
  pave.veh.meta[SVF == s & pave.class  == "local", mean.8am.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% local$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% local$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "local", mean.8am.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% local$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% local$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  # commerical parking lots
  pave.veh.meta[SVF == s & pave.class  == "com.park", mean.8am.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% com.park$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% com.park$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "com.park", mean.8am.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% com.park$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% com.park$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  # residential parking (driveways)
  pave.veh.meta[SVF == s & pave.class  == "res.park", mean.8am.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% res.park$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% res.park$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "res.park", mean.8am.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% res.park$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% res.park$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  # unpaved
  pave.veh.meta[SVF == s & pave.class  == "unpaved", mean.8am.out.flux.lwr := (asph.min * all.surface.data[SVF == s & pave.name %in% unpaved$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.min) * all.surface.data[SVF == s & pave.name %in% unpaved$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
  pave.veh.meta[SVF == s & pave.class  == "unpaved", mean.8am.out.flux.upr := (asph.max * all.surface.data[SVF == s & pave.name %in% unpaved$a.pave.type & hrs == 8, mean(q.rad + q.cnv)]) + 
                  (1 - asph.max) * all.surface.data[SVF == s & pave.name %in% unpaved$c.pave.type & hrs == 8, mean(q.rad + q.cnv)]]
}

pave.veh.meta

# save output data
saveRDS(fclass.meta, here("data/outputs/pavement-class-summary.rds"))
saveRDS(pave.veh.meta, here("data/outputs/pavement-vehicle-heat-metadata.rds"))
