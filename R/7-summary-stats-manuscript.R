# PAPER SUMMARY STATS

# clear space and allocate memory
gc()
memory.limit(size = 56000) 

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(zoo, lib.loc = lib.path, quietly = T)
library(lubridate, lib.loc = lib.path, quietly = T)
library(data.table, lib.loc = lib.path, quietly = T)
library(here, lib.loc = lib.path, quietly = T)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # archive date for all used packages (besides checkpoint itself!)
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

# define folder for data reterival (automaticlly take most recent folder with "run_metadata" in it)
folder <- as.data.table(file.info(list.dirs(here("data/outputs/"), recursive = F)), 
                        keep.rownames = T)[grep("run_metadata", rn),][order(ctime)][.N, rn]
# MODEL METADATA
all.model.runs <- readRDS(paste0(folder, "/stats_all_model_runs.rds"))

# load layer profile data and add info to model run metadata
for(id in all.model.runs[, batch.id]){
  p.i <- readRDS(here(paste0("data/outputs/layer-profiles-", id,".rds"))) 
  assign(paste0("layer.profiles.", id), p.i)
}

# add layer profile data to model run metadata
for(pave.i in unique(all.model.runs[, pave.name])){
  id <- unique(all.model.runs[pave.name == pave.i, batch.id])
  pave.id <- which(names(get(paste0("layer.profiles.", id))) == pave.i)
  my.profile <- get(paste0("layer.profiles.", id))[[pave.id]]
  
  # store differing layer specs in model run meta data
  for(my.layer in 1:my.profile[, .N]){
    vars <- paste0("L", my.layer, ".", colnames(my.profile))
    all.model.runs[pave.name == pave.i, (vars) := my.profile[my.layer,]]
    
    # also calc total pavement thickness (exlude last subgrade layer)
    # and mean pave parameters of k, rho, and c for the non subgrade layers
    all.model.runs[pave.name == pave.i, pave.thick := 
                     sum(my.profile[layer != "subgrade", thickness], na.rm = T)]
    all.model.runs[pave.name == pave.i, mean.k := 
                     sum(my.profile[layer != "subgrade", k], na.rm = T)]
    all.model.runs[pave.name == pave.i, mean.c := 
                     sum(my.profile[layer != "subgrade", rho], na.rm = T)]
    all.model.runs[pave.name == pave.i, mean.rho := 
                     sum(my.profile[layer != "subgrade", c], na.rm = T)]
  }
}

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
valid.dates <- readRDS(here("data/aster/my-aster-data.rds")) # remote sensed temps at valiation sites on specified dates
my.years <- unique(valid.dates[, year(date.time)]) # store all unique years to reterive weather data for those years
weather.raw <- rbindlist(lapply(here(paste0("data/mesowest/", my.years, "-meso-weather-data.rds")), readRDS), fill = T) # bind all weather data for the selected years

#colnames(weather.raw)
weather.raw[, .(mean.winspd = mean(winspd, na.rm = T)), by = hour(date.time)][order(hour)]
weather.raw[, .(median.winspd = quantile(winspd, probs = 0.5, na.rm = T)), by = hour(date.time)][order(hour)]
weather.raw[, .(mean.temp.c = mean(temp.c, na.rm = T)), by = hour(date.time)][order(hour)]


# PAVEMENT SURFACE
all.surface.data <- readRDS(paste0(folder, "/all_pave_surface_data.rds"))
#colnames(all.surface.data)

# add daytime factor
all.surface.data[, daytime := factor(ifelse(hour(date.time) %in% c(7:18), "Day", "Night"), 
                                     levels = c("Day","Night"))]

# calc pavement thickness (exlude subbase/subgrade)

# min/max temp linear regression to variables of interest
# define interest vars
i.vars <- c("pave.thick", "L1.albedo", "L1.emissivity", "mean.k", "mean.c", "mean.rho")
a.vars <- c("pave.name", i.vars)
lm.results <- data.table("var" = i.vars)
lm.data <- merge(all.surface.data, all.model.runs[, ..a.vars], by = c("pave.name"), allow.cartesian = T)
lm.data <- lm.data[batch.id != "BG",] # exlude bare ground for analysis of pavement properties only

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
