# 1D model based on fundamental energy balance to calculate the pavement near-surface temperatures
# based on model outlined in Gui et al. (2007) [1]

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
#.libPaths(paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1"))
library(mailR, lib.loc = lib.path)
library(insol, lib.loc = lib.path)
library(zoo, lib.loc = lib.path)
library(lubridate, lib.loc = lib.path)
library(data.table, lib.loc = lib.path)
library(here, lib.loc = lib.path)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # archive date for all used packages (besides checkpoint itself!)
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 


################################
# DEFINE BATCH RUN ID and NAME #
################################
batch.n <- 4 # choose index for batch run type

batches <- data.table(id = c("A", "C", "WA", "OC", "BG"),
                         name = c("Asphalt Pavements", 
                                  "Concrete Pavements", 
                                  "Whitetopped Asphalt Pavements",
                                  "Asphalt Overlays on PCC Pavements",
                                  "Bare Ground / Desert Soil"))

# create output folder name with run info
out.folder <- here(paste0("data/outputs/1D-heat-model-runs/", 
                     format(strptime(script.start, format = "%Y-%m-%d %H:%M:%S"), format = "%Y%m%d_%H%M%S"), 
                     "_model_outputs_", batches[batch.n, id], "/"))
dir.create(here("data/outputs/1D-heat-model-runs"), showWarnings = FALSE)
dir.create(out.folder, showWarnings = FALSE)

# load layer profiles as list of data.tables (previously defined)
layer.profiles <- readRDS(here(paste0("data/outputs/layer-profiles-", batches[batch.n, id],".rds"))) 

# define validation site location IDs to pull weather data from the nearest weather site to corresponding layer profile
if(batches[batch.n, id] == "BG"){layer.sites <- c("B2", "B2", "B2") # BARE GROUND also B2 good
} else if(batches[batch.n, id] == "C") {layer.sites <- c("C1", "C1", "C1") # CONCRETE also C1 good
} else if(batches[batch.n, id] == "WA") {layer.sites <- c("C4", "C4", "C4") # WHITETOPPED ASPHALT also C4 good
} else if(batches[batch.n, id] == "OC") {layer.sites <- c("A6", "A6", "A6") # ASPHALT OVERLAY CONCRETE also A6 good
} else if(batches[batch.n, id] == "A") {layer.sites <- c("A6", "A6", "A6")} # ASPHALT also A3 good

# load validation site data 
valid.dates <- readRDS(here("data/outputs/aster/aster-data-my.rds")) # remote sensed temps at valiation sites on specified dates
valid.dates <- valid.dates[!(date(date.time) %in% date(c("2007-06-26","2013-10-30")))] # drop bad dates **temporary**
my.sites <- fread(here("data/validation_sites.csv")) # other validation sites info 

# create list of models to run with varied inputs to check sentivity/error
# first values in each vector will correspond to a refrence run for RMSE calcs where appropriate
models <- list(run.n = c(0), # dummy run number (replace below)
               nodal.spacing = c(10),# nodal spacing in millimeters
               i.top.temp = c(33.5), # starting top boundary layer temperature in deg C
               i.bot.temp = c(33.5), # starting bottom boundary layer temperature in deg C. ASSUMED TO BE CONSTANT 
               time.step = c(30), # time step in seconds
               #pave.length = c(200), # characteristic length of pavement in meters
               SVF = c(1, 0.5), # sky view factor  0.5, 
               layer.profile = 1:length(layer.profiles), # for each layer.profile, create a profile to id
               end.day = unique(valid.dates[, date(date.time)]), # date on which to end the simulation (at midnight)
               n.days = c(3) # number of days to simulate 
)

model.runs <- as.data.table(expand.grid(models)) # create all combinations in model inputs across profiles
model.runs$run.n <- seq(from = 1, to = model.runs[,.N], by = 1) # create run number id
model.runs[,`:=`(run.time = 0, RMSE = 0, CFL_fail = 0, broke.t = 0)] # create output model run summary variables
model.runs[, ref := min(run.n), by = layer.profile] # create refrence model for each unique layer profile (defaults to first scenario of parameters)
model.runs[, depth := rep(sapply(1:length(layer.profiles), function (x) sum(layer.profiles[[x]]$thickness)), each = model.runs[layer.profile == 1, .N])]
model.runs[, batch.name := batches[batch.n, name]]
model.runs[, batch.id := batches[batch.n, id]] 
for(a in 1:length(layer.profiles)){ # record other layer profile properties in model.runs
  model.runs[layer.profile == a, albedo := layer.profiles[[a]]$albedo[1]] 
  model.runs[layer.profile == a, emissivity := layer.profiles[[a]]$emissivity[1]]
  model.runs[layer.profile == a, pave.name := names(layer.profiles)[a]]
  model.runs[layer.profile == a, valid.site := layer.sites[a]]
}

# LOAD & FILTER WEATHER DATA 
my.years <- unique(valid.dates[, year(date.time)]) # store all unique years to reterive weather data for those years
weather.raw <- rbindlist(lapply(here(paste0("data/mesowest/", my.years, "-meso-weather-data.rds")), readRDS), fill = T) # bind all weather data for the selected years
weather.raw <- weather.raw[!is.na(temp.c) & !is.na(dewpt.c) ] #& !is.na(solar) & !is.na(winspd),] # make sure only to select obs w/o NA of desired vars
weather.raw[is.na(winspd), winspd := 0] # force NA windspeeds to zero as this is the limiting weather observation

# pre-trim weather data to only the dates that we will be using weather data from (defined by model.runs$end.day and model.runs$n.days
all.dates <- unique(do.call("c", mapply(function(x,y) seq(date(x) - days(y - 1), by = "day", length.out = y + 1), model.runs$end.day, model.runs$n.days, SIMPLIFY = F)))
weather.raw <- weather.raw[date(date.time) %in% all.dates,]

# import station metadata and calculate average number of observations of critical weather parameters
stations.raw <- unique(rbindlist(lapply(here(paste0("data/mesowest/", my.years, "-meso-station-data.rds")), readRDS))) # load corresonding years of station metadata

# merge latitude from station data for solar rad estimation
weather.raw <- merge(weather.raw, stations.raw[,.(station.name,lat,lon)], by = "station.name", all.x = T, allow.cartesian = T) 

# create empty object and file to store error messages
my.errors <- NULL
run.log <- file(paste0(out.folder,"/run_log.txt"), open = "a")

# BEGIN MODEL LOGIC
for(run in 1:model.runs[,.N]){  # 
  tryCatch({  # catch and print errors, avoids stopping model runs 
    
    # SETUP FOR MODEL
    
    # store model start time, print in console and add to log file
    start.time <- Sys.time()
    cat(paste0("started run ", run, " at ", start.time, " (", round(difftime(start.time, script.start, units = "min"), ifelse(run == 1, 2, 0))," mins since start script) \n"))
    cat(paste0("started run ", run, " at ", start.time, " (", round(difftime(start.time, script.start, units = "min"), ifelse(run == 1, 2, 0))," mins since start script) \n"), file = run.log, append = T)
    
    # set time run broke as NA
    model.runs[run.n == run, broke.t := NA]
    
    # first clear up space for new run
    rm(list=setdiff(ls(), c("run", "model.runs", "layer.profiles", "script.start", "pave.time.ref", "stations.raw",
                            "weather.raw", "create.layer", "start.time", "my.errors", "my.sites", "out.folder", "run.log")))
    gc()
    t.start <- Sys.time() # start model run timestamp
    
    # store number of days for simultion
    day.n <- model.runs[run.n == run, n.days]
    
    # store desired dates and validation site data for this run
    my.dates <- seq.Date(date(model.runs[run.n == run, end.day]) - days(day.n) + 1, date(model.runs[run.n == run, end.day]) + 1, "day")
    my.site <- my.sites[Location == model.runs$valid.site[run],]
    
    # trim weather data to relevant dates for this run (just before and past end date.time)
    weather <- weather.raw[date.time >= force_tz(min(my.dates) - seconds(1), tz =  tz(weather.raw$date.time[1])) & 
                           date.time <= force_tz(max(my.dates) + hours(1), tz =  tz(weather.raw$date.time[1])) ,]
  
    # trim station data to available stations during desired dates
    stations <- stations.raw[station.name %in% weather$station.name,]
    
    # calculate earth surface distance in km between chosen validation site and available weather stations
    stations[, my.site.dist := (GCdistance(my.site[, lat], my.site[, lon], stations$lat, stations$lon)) / 1000 ] # GCdistance reports in meters
    
    # calculate mean and sd on rounded hour for quality control checks
    weather[, date.time.round := as.POSIXct(round.POSIXt(date.time, "hours"))]
    weather[, avg.hr.t := mean(temp.c, na.rm = T), by = "date.time.round"]
    weather[, sd.hr.t := sd(temp.c, na.rm = T), by = "date.time.round"]
    weather[, avg.hr.s := mean(solar, na.rm = T), by = "date.time.round"]
    weather[, sd.hr.s := sd(solar, na.rm = T), by = "date.time.round"]

    
    # we create another qcflag b/c qcflag in MESOWEST is only for whole station date range
    weather[, qcflag2 := 1]
    weather[between(temp.c, avg.hr.t - sd.hr.t*2, avg.hr.t + sd.hr.t*2), qcflag2 := 0]
    weather[between(solar, avg.hr.s - sd.hr.s*2, avg.hr.t + sd.hr.t*2), qcflag2 := 0]

    # calculate the remaining obs per day for desired time period
    for(s in 1:stations[,.N]){ 
      s.name <- stations[s, station.name]
      if(weather[station.name == s.name, .N] == 0){next}
      stations[s, n.solar.day := weather[station.name == s.name & !is.na(solar), .N] / day.n]
      stations[s, n.dewpt.day := weather[station.name == s.name & !is.na(dewpt.c), .N] / day.n]
      stations[s, n.tempc.day := weather[station.name == s.name & !is.na(temp.c), .N] / day.n]
      stations[s, max.gap.hrs := max(difftime(min(weather[station.name == s.name, date.time]), # difftime btwn min date and first obs 
                                              force_tz(min(my.dates) + seconds(1), 
                                                       tz =  tz(weather.raw$date.time[1])), units = "hours"),
                                     difftime(max(weather[station.name == s.name, date.time]), # difftime btwn max date and last obs
                                              force_tz(max(my.dates) + seconds(1), 
                                                       tz =  tz(weather.raw$date.time[1])), units = "hours"),
                                     difftime(weather[station.name == s.name, date.time], # difftimes btwn all other vars
                                              weather[station.name == s.name, shift(date.time)], 
                                              units = "hours"), na.rm = T)]
      stations[s, p.flag := weather[station.name == s.name & qcflag2 == 1, .N] 
               / (weather[station.name == s.name & !is.na(solar) & !is.na(temp.c), .N] + 1)]
    }

    # filter to only stations with an average of one observation per hour during desired dates,
    # with no gap in observations greater than 2 hours, and
    # with good quality data (low percent of data falling outside expected range)
    my.stations <- stations[n.dewpt.day >= 24 & n.tempc.day >= 24 & p.flag <= 0.05 & max.gap.hrs < 2,] # n.solar.day >= 24 & 
    
    # if there are no stations that satisfy the above constraints, skip run and store error msgs
    if(my.stations[,.N] == 0){
      assign("my.errors", c(my.errors, 
                            paste("skipped run", run, "because too few weather obs \n")), 
             envir = .GlobalEnv) # store error msg
      cat(paste("skipped run", run, "because too few weather obs \n"),
          file = run.log, append = T)
      next} # skip to next run if there are fewer than 12 obs a day
    
    
    
    # determine which of eligible stations are the closest to the validation site
    my.station <- my.stations[my.site.dist == min(my.site.dist, na.rm = T), station.name]

    # finally, trim weather to station that is chosen
    weather <- weather[station.name == my.station,]
  
    # store station.name and distance in model.run metadata and my.sites validation data
    model.runs[run.n == run, station.name := my.station]
    model.runs[run.n == run, station.dist.km := my.stations[station.name == my.station, my.site.dist]]
    my.sites[, station.name := my.station]
    my.sites[, station.dist.km := my.stations[station.name == my.station, my.site.dist]]
    
    # store layer dt
    layer.dt <- layer.profiles[[model.runs$layer.profile[run]]]
    layer.dt[, rho.c := rho * c] # volumetric heat capacity (J/(m3 degK)); [1] concrete: ~2.07E6; ashpalt ~1.42E6 [6]  9.63E5 [8]
    
    # input pavement layer thickness data
    x <- sum(layer.dt$thickness)  # model depth (m);  GUI ET AL: 3.048m [1]
    delta.x <- model.runs[run.n == run, nodal.spacing] / 1000 # chosen nodal spacing within pavement (m). default is 12.7mm (0.5in)
    delta.t <- model.runs[run.n == run, time.step] # store time step sequence (in units of seconds)
    
    # create n layers and boundaries as input defines
    n.layers <- nrow(layer.dt)
    for(layer.i in 1:n.layers){ # we multiply by 1000 to make sure mod (%%) works correctly in create.layer
      
      # layer specifications
      thickness <- round(layer.dt$thickness[layer.i] * 1000, 0)
      name <- layer.dt$layer[layer.i]
      start.depth <- round((sum(layer.dt[1:layer.i, thickness]) - layer.dt$thickness[layer.i]) * 1000, 0)
      nodal.spacing <- delta.x * 1000
      
      # if last layer, add 2 extra nodes, the last node will be deleted right before simulation begins 
      # to ensure seemless end to material (no bottom boundary)
      if(layer.i == max(n.layers)){thickness <- thickness + (nodal.spacing * 2)}
      
      # both top and bottom interface nodes of layer occurs on delta.x location:
      if(start.depth %% nodal.spacing == 0 & (start.depth + thickness) %% nodal.spacing == 0){
        layer.n <- data.table("x" = seq(start.depth + nodal.spacing, start.depth + thickness - nodal.spacing, nodal.spacing), "layer" = name)
        
        # top interface node occurs on delta.x location but not bottom interface:
      } else if (start.depth %% nodal.spacing == 0 & (start.depth + thickness) %% nodal.spacing != 0){
        layer.n <- data.table("x" = seq(start.depth + nodal.spacing, start.depth + (floor(thickness/nodal.spacing) * nodal.spacing), nodal.spacing), "layer" = name)
        
        # bottom interface node occurs on delta.x location but not top interface:
      } else if (start.depth %% nodal.spacing != 0 & (start.depth + thickness) %% nodal.spacing == 0){
        layer.n <- data.table("x" = seq(start.depth + (start.depth %% nodal.spacing), start.depth + thickness - nodal.spacing, nodal.spacing), "layer" = name)
        
        # neither top nor bottom interface nodes of layer occurs on delta.x location:
      } else {
        layer.n <- data.table("x" = seq(start.depth, start.depth + thickness, nodal.spacing), "layer" = name)
      }
      
      layer.n[, x := x / 1000] # x back to mm (divide by 1000)
      assign(paste0("layer.",layer.i), layer.n)
      
      # creat boundary
      boundary.n <- data.table(x = sum(layer.dt$thickness[1:layer.i]) - layer.dt$thickness[layer.i], layer = "boundary")
      
      assign(paste0("boundary.",layer.i), boundary.n)
    }
    
    # create closely spaced nodes near surface to improve accuracy for near surface interaction
    # if layer 1 thickness is less than 25 mm, use different characterization
    # until 25 mm depth (0.025 m), start using predifed nodal spacing (recommended again is 12.5mm)
    #if(layer.dt[1, thickness] >= 0.025){
    #  layer.1 <- rbind(data.table(x = c(0.001,0.002,0.003,0.005,0.010,0.015,0.025), layer = layer.1$layer[2]), layer.1[x > 0.025])
    #} else {
    #  layer.1 <- rbind(data.table(x = c(0.001,0.002,0.003,0.004,0.005,0.0075), layer = layer.1$layer[2]), layer.1[x > 0.0075])
    #}
    
    # create ordered list of boundaries and layers (no bottom boundary created)
    my.layers <- c()
    for(layer.i in 1:n.layers){
      my.layers <- c(my.layers, paste0("boundary.",layer.i), paste0("layer.",layer.i))
    }
    
    # bind all boundaries/layers together
    p.data <- rbindlist(mget(my.layers))
    
    # create timestep columns (in this case, column of pavement temps by depth calculated every 2 mins for 24 hours)
    # start from time 0 to max time which is absolute time difference in weather data 
    # add + 1 timestep at end to calculated timestep - 1 in model which is last weather obs & delete timestep n (uncalc'd) at end
    t.step <- seq(from = 0, # from time zero
                  to = as.numeric(difftime(max(weather$date.time), min(weather$date.time), units = "secs")) + delta.t, 
                  by = delta.t) 
    p.n <- length(t.step) # store final time step n
    
    # intitalize data.table for store model output data of pavement heat transfer time series modeling
    pave.time <- as.data.table(data.frame("time.s" = rep(t.step, each = nrow(p.data)),
                                          "node" = rep(0:(nrow(p.data)-1), p.n),
                                          "depth.m" = rep(p.data$x, p.n),
                                          "layer" = rep(p.data$layer, p.n),
                                          "T.K" = rep(seq(from = model.runs[run.n == run, i.top.temp] + 273.15, # initial surface temp in K
                                                          to = model.runs[run.n == run, i.bot.temp] + 273.15, # initial bottom boundary temp in K
                                                          length.out = p.n)))) # surface temp in K from the pavement to 3m)
    
    # static parameters for pavement heat transfer
    albedo <- model.runs[run.n == run, albedo] #layer.dt$albedo[1] #  albedo (dimensionless) [1]; can be: 1 - epsilon for opaque objects
    epsilon <- model.runs[run.n == run, emissivity] #  emissivity (dimensionless) [1]; can be: 1 - albedo for opaque objects
    sigma <- 5.67*10^(-8) #  Stefan-Boltzmann constant (W/(m2*K4); [1]
    SVF <- model.runs[run.n == run, SVF] #layer.dt$SVF[1]	# sky view factor (dimensionless [0,1]). assume 1.0: pavement is completely visible to sky
    
    # L in (m) is the characteristic length of the pavement at the test site taken as the ratio of
    #L <- model.runs[run.n == run, pave.length] # the slab length in the direction of the wind to the perimeter
    L <- my.site[, max.pave.len]
    model.runs[run.n == run, pave.length := L]
    # assume horizontal & flat, width b and infinite length L, hotter than the environment -> L = b/2
    # L could vary, but minimum for a pavement should be 2 lanes, or about ~10 meters
    
    # derive key parameters of air (sky temp, dry bulb temp, and convective heat transfer of air) which may vary depending on atmospheric conditions
    weather[, time.s := as.numeric(difftime(weather$date.time, weather$date.time[1], units = "secs"))] # time.s to match with iterations in weather data (assuming first obs is time zero)
    weather[, winspd := winspd * 0.44704] # convert to m/s from mi/h (for h.inf calc)
    weather[, T.inf := temp.c + 273.15] #  atmospheric dry-bulb temperature (K);
    weather[, T.sky := T.inf * (((0.004 * dewpt.c) + 0.8)^0.25)] # sky temperature (K), caluclated using equation (2)
    weather[, v.inf := 1.47E-6 * (T.inf^(3/2)) / (T.inf + 113)] # kinematic viscosity of air (m2/s) for T.inf btwn 100 and 800 K [9]
    weather[, k.inf := ((1.5207E-11) * (T.inf^3)) - ((4.8574E-08) * (T.inf^2)) + ((1.0184E-04) * (T.inf)) - 3.9333E-04] # thermal conductivity of air in W/(m*degK) [0.02, 0.03] (for normal air temps) [2]
    elev <- ifelse(length(my.stations[station.name == my.station, elevation]) == 0, 500, my.stations[station.name == my.station, elevation]) # assume 500 m elevation if no elevation data
    p.air.dry <- (-8.5373296284E-09 * (elev^3)) + (5.2598726265E-04 * (elev^2)) - (1.1912694417E+01 * elev) + 1.0136022009E+05 # estimate of dry air pressure (Pa) via elevation (see air-emperical-fit.xlsx for fit)
    weather$p.wat.vap <- (100 - 5 * (weather$temp.c - weather$dewpt.c)) * (6.1078 * (10^(7.5 * weather$temp.c / (weather$temp.c + 237.3)))) # RH (approx) * saturation vapor pressure
    weather$p.air.hum <- (p.air.dry / 287.058 / weather$T.inf) + (weather$p.wat.vap / 461.495 / weather$T.inf)  # density of humid air (kg/m3)
    weather[, c.p.air := (2E-04 * (T.inf^2)) - (7E-02 * T.inf) + 1.008E+03] # specific heat capacity of air (J/(kg*degK)(see air-emperical-fit.xlsx for emperical fit
    weather[, Pr.inf := 1.6380e-05 * p.air.hum * c.p.air / k.inf] # Prandtl number, should be 0.70 - 0.72 for air between 0 and 100 C at 0 to 10 bar
    weather[, h.inf := 0.664 * (k.inf * (Pr.inf ^ 0.3) * (v.inf ^ -0.5) * (L ^ -0.5) * (winspd ^ 0.5))] # convective heat transfer coefficient in W/(m2*degK) [0.5, 1000]
    
    # iterpolate between required observational weather data to create needed weather data at each timestep
    # first create new data.table for a single node with all obervational weather data matched by timestep (timesteps match)
    # then linearlly approximate between the weather observations for all NA timesteps using zoo::na.approx
    # finally merge to orginal data.table to have all weather observations present such that missing ones are interplotated between
    aprx <- merge(pave.time[node == 0], weather[, .(time.s, T.sky, T.inf, dewpt.c, solar, h.inf)], by = "time.s", all.x = T)
    aprx[, `:=` (T.sky = na.approx(T.sky, 1:.N, na.rm = F), # sky temperature in K
                 T.inf = na.approx(T.inf, 1:.N, na.rm = F), # atmospheric dry-bulb temperature in K
                 dewpt.c = na.approx(dewpt.c, 1:.N, na.rm = F), # dew point in C
                 solar = na.approx(solar, 1:.N, na.rm = F), # solar radiation in W/m2
                 h.inf = na.approx(h.inf, 1:.N, na.rm = F))]  # wind speed in m/s
    pave.time <- merge(pave.time, aprx[, .(time.s, T.sky, T.inf, dewpt.c, solar, h.inf)], by = "time.s", all.x = T)
    
    # calculate solar radiation (insolation) at validation site coords
    # assume elevation is nearest stations elevation or otherwise 500m (from calc above)
    jday <- JD(min(weather$date.time) + seconds(pave.time[node == 0,time.s])) # JD() always calcs julian day respective to UTC time
    zen <- sunpos(sunvector(jday, latitude = my.site$lat, longitude = my.site$lon, timezone = 0))[,"zenith"]
    rh <- 100 * (exp((17.625 * pave.time[node == 0, dewpt.c])/(243.04 + pave.time[node == 0, dewpt.c])) / exp((17.625 * pave.time[node == 0, T.inf - 273.15])/(243.04 + pave.time[node == 0, T.inf - 273.15]))) 
    sol.est <- insolation(zenith = zen, jd = jday, height = elev, visibility = 15, RH = rh, tempK = pave.time[node == 0, T.inf], O3 = 0.00275, alphag = albedo)
    
    # add insolation to pave.time data
    pave.time[node == 0, insol := (sol.est[,1] + sol.est[,2])]

    # add k and rho.c
    pave.time <- merge(pave.time, layer.dt[,.(layer,k,rho.c)], by = "layer", all.x = T, allow.cartesian = T) 
    
    # add boundary/interface thermal contact resistance (R.c)
    boundary.nodes <- pave.time[layer == "boundary" & time.s == 0 & node != 0, node]
    for(b in 1:length(boundary.nodes)){
      pave.time[node == boundary.nodes[b], R.c := layer.dt$R.c.top[b]]
    }
    
    # calcluate the upper and lower layer thermal conductivity (used for boundary conditions)
    # and change in x from the upper and lower node (e.g. x.up is the change in x btwn the current node and the node right above)
    # and remerge to main data
    up.dn <- pave.time[time.s == 0]
    up.dn <- up.dn[order(node)] # make nodes are ordered correctly for shifting properly
    up.dn[, k.up := shift(k, type = "lag")]
    up.dn[, k.dn := shift(k, type = "lead")]
    up.dn[, x.up := abs(shift(depth.m, type = "lag") - depth.m)]
    up.dn[, x.dn := abs(shift(depth.m, type = "lead") - depth.m)]
    pave.time <- merge(pave.time, up.dn[, .(node, k.up, k.dn, x.up, x.dn)], by = "node", all.x = T, allow.cartesian = T)
    
    # also create/store a few other things
    pave.time[, date.time := weather$date.time[1] + seconds(time.s)] # add date.time column 
    pave.time[time.s != 0, T.K := NA] # only initial temps are assumed, else NA
    pave.time[, layer := as.character(layer)] # to avoid issues w/ factors
    
    # get rid of extra node at bottom
    pave.time <- pave.time[node != max(node)]
    
    # trim to exactly date length
    pave.time <- pave.time[date.time >= min(my.dates) & date.time <= max(my.dates),]
    
    # MODEL HEAT TRANSFER OF PAVEMENT
    # iterate through from time p to time p.n and model pavement heat transfer at surface, boundary/interface, and interior nodes 
    for(p in 1:(p.n-1)){ # state at time p is used to model time p+1, so stop and p-1 to get final model output at time p
      
      # print progress
      if(p%%1000 == 0){
        
        # print to console
        cat("r =", run, "/", model.runs[,.N], paste0("(", signif(run/model.runs[,.N], 2) * 100,"%)"),"|",
            "p =", p, "/", p.n, paste0("(", signif(p/p.n, 2) * 100,"%)"), "|",  
            round(difftime(Sys.time(), start.time, units = "min"), 0),"mins run", "|",
            round(difftime(Sys.time(), script.start, units = "min"), 0),"mins script \n")
        
        # add to log file
        cat("r =", run, "/", model.runs[,.N], paste0("(", signif(run/model.runs[,.N], 2) * 100,"%)"),"|",
            "p =", p, "/", p.n, paste0("(", signif(p/p.n, 2) * 100,"%)"), "|",  
            round(difftime(Sys.time(), start.time, units = "min"), 0),"mins run", "|",
            round(difftime(Sys.time(), script.start, units = "min"), 0),"mins script \n",
            file = run.log, append = T)
      }
      
      # if this is the first time step, we loop over time p == 1 and the initial conditions until we
      # reach a stable and converged initial condition of pavement temps at all depths 
      # this is done because if the initial defined pavement temperature is not at/near an equilibrium
      # with the weather conditions, the initial flux graidents at the surface
      # will cause severe oscillations and the model will not be able convergence
      # if it takes more than 1000 times and they aren't converging, end loop to avoid inf looping if no feasible solution
      # it will otherwise break after one run if the nodes at p and p+1 have converged
      # therefore allowing all p+2 and beyond calculations to always loop only once (intital and p+1 shouldn't change)
      
      for(z in 1:200){
        
        # for timestep p, calc the surface heat transfer parameters (surface radiative coefficient, infrared radiation & convection)
        pave.time[time.s == t.step[p] & node == 0, h.rad := SVF * epsilon * sigma * ((T.K^2) + (T.sky^2)) * (T.K + T.sky)]  # radiative heat transfer coefficient in W/(m2*degK) 
        pave.time[time.s == t.step[p] & node == 0, q.rad := SVF * epsilon * sigma * ((T.K^4) - (T.sky^4))] # infrared radiation heat transfer at surfaceW/m2
        pave.time[time.s == t.step[p] & node == 0, q.cnv := h.inf * (T.K - T.inf)] # convection heat transfer W/m2
        
        # store these values for surface for all layers to calc CFL later
        pave.time[time.s == t.step[p], h.rad := pave.time[time.s == t.step[p] & node == 0, h.rad]]
        pave.time[time.s == t.step[p], q.rad := pave.time[time.s == t.step[p] & node == 0, q.rad]]
        pave.time[time.s == t.step[p], q.cnv := pave.time[time.s == t.step[p] & node == 0, q.cnv]]
        
        
        # for p == 1, update about surface temp and progress
        if(p == 1 & (z == 1 | z %% 10)){
          
          cat("run =", run, "|", "z =", z, "|",  # print update in console
              "node 0:", signif(pave.time[time.s == t.step[p] & node == 0, T.K - 273.15], 3), "|",
              "node 5:", signif(pave.time[time.s == t.step[p] & node == 5, T.K - 273.15], 3), "|",
              "node 10:", signif(pave.time[time.s == t.step[p] & node == 10, T.K - 273.15], 3), "|",
              "h.rad:", signif(pave.time[time.s == t.step[p] & node == 0, h.rad], 3), "|",
              "q.rad:", signif(pave.time[time.s == t.step[p] & node == 0, q.rad], 3), "|",
              "q.cnv:", signif(pave.time[time.s == t.step[p] & node == 0, q.cnv], 3), "|",
              "C \n")
          
          cat("run =", run, "|", "z =", z, "|",  # append update in log file
              "node 0:", signif(pave.time[time.s == t.step[p] & node == 0, T.K - 273.15], 3),"C \n", 
              file = run.log, append = T)
        }
        
        # store a temporary data.table at this timestep to store and transfer all node calculations to master data.table.
        # we do this because the 'shift' function is troublesome within data.table subsets so this is safer and possibly quicker
        tmp <- pave.time[time.s == t.step[p]] 
        
        # for timestep p+1, calc the surface pavement temperature based on parameters at timestep p
        # i.e. surface node s (node = 0), eqn 10 in [1]
        pave.time[time.s == t.step[p+1] & node == 0, T.K := tmp[node == 0, T.K] +  # T.K at node 0 is pavement surface temp (at time p+1)
                    ((((1 - albedo) * SVF * tmp[node == 0, insol]) # incoming solar radiation (after albedo reflection, and shading)
                      - tmp[node == 0, q.cnv]  # minus convective heat transfer at surface
                      - tmp[node == 0, q.rad] # minus outgoing longwave/infrared radiation at surface
                      + (layer.dt$k[1] * (tmp[node == 1, T.K] - tmp[node == 0, T.K]) / delta.x)) # plus conduction
                     * (2 * delta.t / (layer.dt$rho.c[1] * delta.x)))] # all multiplied by the proportional change in temp per change in depth 
        
        # calculate interior nodes at current timestep p (non-boundary/non-interface nodes) and store into timestep p+1
        tmp[, T.C.i := # even nodal spacing
              (k * delta.t / rho.c / (((tmp[, x.up] + tmp[, x.dn]) / 2)^2)
               * (tmp[, shift(T.K, type = "lag")]  # T at node m-1 (above) and p
                  + tmp[, shift(T.K, type = "lead")] # T at node m+1 (below) and p
                  - 2 * tmp[, T.K])) + tmp[, T.K]] # T at node m and p
        
        pave.time[time.s == t.step[p+1] & node != 0 & node != max(node), T.K := 
                    tmp[node != 0 & node != max(node), T.C.i]]
        
        pave.time[time.s == t.step[p+1] & node == max(node), T.K := pave.time[time.s == t.step[p+1] & node == max(node)-1, T.K]] # make last node m-1 T
        
        # calculate the heat flux at time p for all nodes (use for estimating boundary temps)
        tmp[, up.flux := # heat flux to/from above node m-1 in w/m2; negative is downward flux (into m from m+1), positive is upward (out of m to m-1)
              ((k / x.up) * (tmp[, T.K] # T at node m and p
                             - tmp[, shift(T.K, type = "lag")]))]  # T at node m-1 (above) and p
        
        tmp[, dn.flux := # heat flux from below node m+1 in w/m2; negative is downward flux (out of m to m+1), positive is upward (out of m to m-1)
              ((k / x.dn) * (tmp[, T.K] # T at node m and p
                             - tmp[, shift(T.K, type = "lead")]))] # T at node m+1 (below) and p
        
        # fix infinite resutls from dividing by 0 in dual boundary node cases to NA,
        # so that flux through the boundary is only provide once in each direction (just in case)
        tmp[!is.finite(up.flux), up.flux := NA]
        tmp[!is.finite(dn.flux), dn.flux := NA]
        
        # store heat fluxes into pave.time at time p (with special for surface node)
        pave.time[time.s == t.step[p], h.flux.up := tmp[, up.flux]]
        pave.time[time.s == t.step[p], h.flux.dn := tmp[, dn.flux]]
        
        # at surface, calc net flux, 
        pave.time[time.s == t.step[p] & node == 0, h.flux.up := -((1 - albedo) * tmp[node == 0, insol]) + tmp[node == 0, q.rad] + tmp[node == 0, q.cnv] ] # heat flux between air and surface
        pave.time[time.s == t.step[p] & node == 0, q.sol.rfl := albedo * tmp[node == 0, insol]]
        pave.time[time.s == t.step[p] & node == 0, h.flux.dn := tmp[, shift(up.flux, type = "lead")][1]] # conduction from node m+1 (below node)
        
        # calculate boundary/interface nodes at current timestep p by solving for both cases of R.c zero and non-zero then choose correct values
        # because boundary/interface is treated as one node, need to calc both interface temperatures (upper and lower layer T's)
        # for temp at boundary/interface node, just use average of upper and lower T 
        
        # solution for non-zero layer resistance (R.c != 0, therfore upper and lower boundary temps are different)
        tmp[, T.C.b.l_n0 :=
              ((R.c * (tmp[, shift(dn.flux, type = "lead")] - tmp[, shift(up.flux, type = "lag")])) # flux through interface; negative is downward flux. lead = m+1
               + tmp[, shift(T.K, type = "lag")] # lag = m-1 (above node)
               - (tmp[, shift(T.K, type = "lead")] # lead = m+1 (below node)
                  * (k.dn * x.up / k.up / x.dn))) 
            * k.up * x.dn / ((k.up * x.dn) - (k.dn * x.up))]
        
        # solve for other node at interface with R.c
        tmp[, T.C.b.u_n0 := (R.c * (tmp[, shift(dn.flux, type = "lead")] - tmp[, shift(up.flux, type = "lag")])) + tmp[, shift(T.C.b.l_n0, type = "lag")]] 
        
        # solution for zero layer resistance (R.c == 0 therefore upper layer interface T == lower layer interface T)
        tmp[, T.C.b :=
              (((k.up / k.dn) * (x.dn / x.up) * tmp[, shift(T.K, type = "lag")]) 
               + tmp[, shift(T.K, type = "lead")])
            / (1 + ((k.up / k.dn) * (x.dn / x.up)))]
        
        # store any non-zero R.c interface temps (assume single boundary node is the average of the upper and lower layer interface temps)
        pave.time[time.s == t.step[p+1] & node %in% boundary.nodes & R.c != 0, T.K := tmp[node %in% boundary.nodes & R.c != 0, (T.C.b.l_n0 + T.C.b.u_n0) / 2]]
        
        # store any zero R.c interface temps
        pave.time[time.s == t.step[p+1] & node %in% boundary.nodes & R.c == 0, T.K := tmp[node %in% boundary.nodes & R.c == 0, T.C.b]]
        
        # check finite difference solving method satisfied the Courant-Friedrichs-Lewy (CFL) condition for stability from p to p+1
        pave.time[time.s == t.step[p], CFL := (rho.c * (((x.up + x.dn) / 2)^2)) / (2 * (h.rad * delta.x + h.inf * ((x.up + x.dn) / 2) + k))] # in seconds
        pave.time[time.s == t.step[p] & delta.t <= CFL, CFL_fail := 0] # no fail
        pave.time[time.s == t.step[p] & delta.t > CFL, CFL_fail := 1] # yes fail
        
        # break z loop early if non-finite temps detected (error msgs are handled outside of z loop, breaking p loop) 
        if(any(!is.finite(pave.time[time.s == t.step[p], T.K]))){break}
        
        # also break if p != 1 (not initial condition covergance anymore)
        if(p != 1){break}
        
        # break before 200 iterations if the first and second timestep pavement temperatures at each node have converged (within 1/10 deg K of each other) 
        # after 100 iterations, relax converge constraint to 1 degree K
        tol <- ifelse(z < 100, 1, 0)
        # as the initial (p) and p+1 conditions won't change once first converged, this loop breaks always after 1 iteration for all times after p+1
        if(all(is.finite(pave.time[time.s == t.step[2], T.K])) & # all pave temps at p==2 also need to be finite (b/c below check doesn't work for all NAs)
           all(round(pave.time[time.s == t.step[1], T.K], tol) == 
               round(pave.time[time.s == t.step[2], T.K], tol), 
               na.rm = T) == T){
          # store convergence msg and print
          cat("initial conditions converged for run", run, "at", "z =", z, "with", "surface temp:", # append update in log file
              signif(pave.time[time.s == t.step[p] & node == 0, T.K - 273.15], 3),"C \n", 
              file = run.log, append = T)
          cat("initial conditions converged for run", run, "at", "z =", z, "with", "surface temp:", # append update in log file
              signif(pave.time[time.s == t.step[p] & node == 0, T.K - 273.15], 3),"C \n")
          break}
        
        # for the first time step only re-adjust the initial pavement temps
        # to ensure convergence before of intital state before solving past the initial timestep of model
        # we do this after the check to break because if the initial condition has converged, this isn't needed
        if(p == 1){
          pave.time[time.s == t.step[1], T.K := pave.time[time.s %in% t.step[1:2], mean(T.K), by = node][,V1] ] # mean of t1 and 12 T.K
          #pave.time[time.s == t.step[1], T.K := pave.time[time.s == t.step[2], T.K] ]
        }
        
      } # end z loop that repeats only when times p=1 and p=2 haven't converged
      
      # if within this timestep, T.K at p+1 ended up non-finite due to an error or convergance issue, 
      # stop and skip to next run (to prevent wasted time) and store error msgs
      if(any(!is.finite(pave.time[time.s == t.step[p+1], T.K]))){
        model.runs[run.n == run, broke.t := t.step[p]] # store time model stopped
        assign("my.errors", c(my.errors, paste("stopped model run",run,"at", model.runs[run.n == run, broke.t],"secs due to non-finite temperatures detected \n")), envir = .GlobalEnv) # store error msg
        cat(paste("stopped model run",run,"at", model.runs[run.n == run, broke.t],
                  "secs due to non-finite temperatures detected \n"))
        cat(paste("stopped model run",run,"at", model.runs[run.n == run, broke.t],
                  "secs due to non-finite temperatures detected \n"), 
            file = run.log, append = T)
        break
      } 
      
      # if within this timestep, the CFL condition fails anywhere, break the loop (to prevent wasted time)
      if(sum(pave.time[time.s == t.step[p], CFL_fail], na.rm = T) > 0){
        model.runs[run.n == run, broke.t := t.step[p]] # store time model stopped
        assign("my.errors", c(my.errors, paste("stopped model run",run,"at", model.runs[run.n == run, broke.t],"secs due to CFL stability condition not satisfied \n")), envir = .GlobalEnv) # store error msg
        cat(paste("stopped model run", run,"at", model.runs[run.n == run, broke.t],
                  "secs due to CFL stability condition not satisfied \n"),
            file = run.log, append = T)
        break
        } 

    } # end time step p, go to time p+1
    
    # if there was issues causing early break, skip to next run
    if(any(!is.finite(pave.time[time.s == t.step[p+1], T.K]))){next}
    if(sum(pave.time[time.s == t.step[p], CFL_fail], na.rm = T) > 0){next}
    
    # end of run housekeeping
    pave.time[, T.degC := T.K - 273.15] # create temp in deg C from Kelvin
    pave.time <- pave.time[time.s != t.step[p.n]] # remove the last time step
    pave.time[, delta.T.mean := T.degC - mean(T.degC, na.rm = T), by = node] # mean change in temp by node, use for RMSE
    saveRDS(pave.time, paste0(out.folder,"/run_",run,"_output.rds")) # save model iteration R object
    model.runs[run.n == run, run.time := as.numeric(round(difftime(Sys.time(),t.start, units = "mins"),2))] # store runtime of model iteration in minutes
    
    # if the run is a reference run (first run of a unique layer profile scheme), 
    # store it as the reference run for other scenarios under the same layer profile scheme
    if(run == model.runs[run.n == run, ref]){
      pave.time.ref <- pave.time
    }
    
    # if run isn't a reference run, then store RSME compared to the appropriate ref run
    # note that RMSE is mean centered temperature across all nodes and timesteps.
    if(run != model.runs[run.n == run, ref]){ # if the model isn't the refrence run
      model.runs[run.n == run, RMSE := sqrt(mean((pave.time[pave.time.ref, .(time.s,depth.m,delta.T.mean), on = c("time.s","depth.m")][!is.na(delta.T.mean), delta.T.mean] - 
                                                    pave.time.ref[pave.time, .(time.s,depth.m,delta.T.mean), on = c("time.s","depth.m")][!is.na(delta.T.mean), delta.T.mean])^2))]
    } # end RMSE calcs

  }, error = function(e){ # catch and print/store errors
    
    cat("ERROR:",conditionMessage(e), "\n") # print error msg in console
    cat("ERROR:",conditionMessage(e), "\n", file = run.log, append = T) # store error msg in log
    assign("my.errors", c(my.errors, conditionMessage(e)), envir = .GlobalEnv)}) # store error message in list 
  
} # end run, go to next run

# export data (also in RDS format to prevent issues with formatting)
write.csv(model.runs, paste0(out.folder,"/model_runs_metadata.csv"), row.names = F) # output model run metadata
write.csv(my.sites, paste0(out.folder,"/validation_sites.csv"), row.names = F)
saveRDS(model.runs, paste0(out.folder,"/model_runs_metadata.rds"))
saveRDS(my.sites, paste0(out.folder,"/validation_sites.rds")) # output model run metadata
saveRDS(layer.profiles, paste0(out.folder,"/layer_profiles.rds"))
saveRDS(weather.raw, paste0(out.folder,"/weather_data.rds"))

# load email creds and construct msg to notify you by email the script has finished
my.email <- as.character(fread(here("email.txt"), header = F)[1]) 
my.pass <- as.character(fread(here("email.txt"), header = F)[2])
msg <- paste0("R model run complete on ", Sys.info()[4]," at ", Sys.time(), ". Model run length: ", round(difftime(Sys.time(),script.start, units = "mins"),0)," mins.")
cat(msg, file = run.log, append = T)
close(run.log)
send.mail(from = my.email,
          to = my.email,
          subject = msg,
          body = paste0(out.folder, "/run_log.txt"),
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = my.email, passwd = my.pass, ssl = T),
          authenticate = T,
          send = T)
msg

##################
### References ###
##################

# [1] https://doi.org/10.1061/(ASCE)0899-1561(2007)19:8(683)

# [2] http://bouteloup.pierre.free.fr/lica/phythe/don/air/air_k_plot.pdf

# [3] https://www.maricopa.gov/3769/Weather-Sensor-Data

# [4] https://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html

# [5] http://www-mdp.eng.cam.ac.uk/web/library/enginfo/aerothermal_dvd_only/aero/fprops/propsoffluids/node5.html

# [6] https://doi.org/10.1016/1352-2310(94)00140-5

# [7] https://www.fhwa.dot.gov/pavement/sustainability/articles/pavement_thermal.cfm

# [8] https://www.roads.maryland.gov/OMT/pdguide0718.pdf

# [9] https://apps.dtic.mil/dtic/tr/fulltext/u2/a072053.pdf


#plot(pave.time[node == 0, .(date.time, T.degC)],type="l",col="red")
#plot(pave.time[node == max(node), .(date.time, T.degC)],type="l",col="red")
#plot(pave.time[node == max(node)-1, .(date.time, T.degC)],type="l",col="red")

# define function to calculate distance in kilometers between two lat/lon points
#earth.dist <- function (long1, lat1, long2, lat2){
#  rad <- pi/180
#  a1 <- lat1 * rad
#  a2 <- long1 * rad
#  b1 <- lat2 * rad
#  b2 <- long2 * rad
#  dlon <- b2 - a2
#  dlat <- b1 - a1
#  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
#  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
#  R <- 6378.145
#  d <- R * c
#  return(d)
#}