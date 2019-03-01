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
  library(checkpoint, quietly = T)
  }

# load all other dependant packages from the local repo
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(mailR, lib.loc = lib.path, quietly = T)
library(zoo, lib.loc = lib.path, quietly = T)
library(lubridate, lib.loc = lib.path, quietly = T)
library(data.table, lib.loc = lib.path, quietly = T)
library(here, lib.loc = lib.path, quietly = T)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # archive date for all used packages (besides checkpoint itself!)
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

## SPECIFY MODEL RUN SCENARIOS & MATERIAL PARAMETERS 

# specify layer profiles as list of data.tables 
# note that for 2 touching layers to be unique, they must have no thermal contact resistance (R.c == 0) 
# if there thermal conductivies are equivalent (k)
layer.profiles <- list(
  data.table( # low volume single pave HMA 
    layer = c("surface","subgrade"),
    thickness = c(0.1, 1.4), # layer thickness (m)
    k = c(1.21, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2238, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(921, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.10,NA), # surface albedo (dimensionless)
    emissivity = c(0.93,NA), # emissivity (dimensionless)
    #SVF = c(0.5,NA), # sky view factor
    R.c.top = c(NA,0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # PCC whitetopping bonded on HMA (lower density roads)
    layer = c("surface","base","subgrade"),
    thickness = c(0.1, 0.1, 1.3), # layer thickness (m)
    k = c(2.24, 1.21, 1.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(2240, 2382, 1500), # layer density (kg/m3)
    c = c(900, 920, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.3,NA,NA), # surface albedo (dimensionless)
    emissivity = c(0.95,NA,NA), # emissivity (dimensionless)
    #SVF = c(0.5,NA,NA), # sky view factor
    R.c.top = c(NA,0,0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Ordinary PCC + some additives (med density)
    layer = c("surface","subgrade"),
    thickness = c(0.3, 1.2), # layer thickness (m)
    k = c(1.16, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 1500), # layer density (kg/m3)
    c = c(990, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.3,NA), # surface albedo (dimensionless)
    emissivity = c(0.95,NA), # emissivity (dimensionless)
    #SVF = c(0.5,NA), # sky view factor
    R.c.top = c(NA,0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # major arterial HMA rebonded (OGFC 20mm on 280mm DGHMA)
    layer = c("surface","base","subgrade"),
    thickness = c(0.02, 0.28, 1.2), # layer thickness (m)
    k = c(0.841, 1.21, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2080, 2467, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(921, 921, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25,NA,NA), # surface albedo (dimensionless)
    emissivity = c(0.89,NA,NA), # emissivity (dimensionless)
    #SVF = c(0.5,NA,NA), # sky view factor
    R.c.top = c(NA,0,0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names corresponding to the validation site location IDs
# this will pull weather data from the nearest weather site with data to the validaiton site specfied
names(layer.profiles) <- c("A1","A3","C1","M1") 

# load validation site data 
valid.dates <- readRDS(here("data/best-aster-dates.rds")) # remote sensed temps at valiation sites on specified dates
my.sites <- fread(here("data/validation_sites.csv")) # other validation sites info 
setnames(my.sites, "X", "lon")
setnames(my.sites, "Y", "lat")

# create list of models to run with varied inputs to check sentivity/error
# first values in each vector will correspond to a refrence run for RMSE calcs where appropriate
models <- list(run.n = c(0), # dummy run number (replace below)
               nodal.spacing = c(12.5),# nodal spacing in millimeters
               n.iterations = c(2), # number of iterations to repeat each model run
               i.top.temp = c(33.5), # starting top boundary layer temperature in deg C
               i.bot.temp = c(33.5), # starting bottom boundary layer temperature in deg C. ASSUMED TO BE CONSTANT 
               time.step = c(30), # time step in seconds
               pave.length = c(40), # characteristic length of pavement in meters
               #albedo = c(0.2,0.3), # surface albedo
               SVF = c(1), # sky view factor
               layer.profile = 1:length(layer.profiles), # for each layer.profile, create a profile to id
               end.day = valid.dates[, date(date.time)], # date on which to end the simulation (at midnight)
               n.days = c(3) # number of days to simulate 
)

model.runs <- as.data.table(expand.grid(models)) # create all combinations in model inputs across profiles
model.runs$run.n <- seq(from = 1, to = model.runs[,.N], by = 1) # create run number id
model.runs[,`:=`(run.time = 0, RMSE = 0, CFL_fail = 0, broke.t = NA)] # create output model run summary variables
model.runs[, ref := min(run.n), by = layer.profile] # create refrence model for each unique layer profile (defaults to first scenario of parameters)
model.runs[, depth := rep(sapply(1:length(layer.profiles), function (x) sum(layer.profiles[[x]]$thickness)), each = model.runs[layer.profile == 1, .N])]
for(a in 1:length(layer.profiles)){ # record other layer profile properties in model.runs
  model.runs[layer.profile == a, albedo := layer.profiles[[a]]$albedo[1]] 
  model.runs[layer.profile == a, emissivity := layer.profiles[[a]]$emissivity[1]]
  model.runs[layer.profile == a, valid.site := names(layer.profiles)[a]]
}

# estimated model runs time(s)
#coeff <- c(1.551634368,-0.039871215,0.079319245,-0.035625977,0.246614492,0.862178686)
#est.run.time <- exp(coeff[1] + coeff[2]*model.runs$nodal.spacing + coeff[3]*model.runs$n.iterations + coeff[4]*model.runs$time.step + coeff[5]*model.runs$depth + coeff[6]*model.runs$n.days)
#est.run.time <- model.runs$n.iterations * model.runs$n.days * model.runs$depth / model.runs$time.step / model.runs$nodal.spacing
#est.run.time <- (432.39 * est.run.time) + 0.7298   #(516.43 * est.run.time) + 33.065 # linear regression on parameters predicting acutal run time
#for(a in 1:model.runs[,.N]){cat("run",model.runs$run.n[a],":",round(sum(est.run.time[a]), 0), "mins (", round(sum(est.run.time[a])/60, 2)," hrs) \n")}
#paste0("Estimated run time for all ",model.runs[,.N], " runs: ", round(sum(est.run.time), 0), " mins (", round(sum(est.run.time)/60, 2)," hrs)")


# LOAD & FILTER WEATHER DATA 
my.years <- unique(valid.dates[, year(date.time)]) # store all unique years to reterive weather data for those years
weather.raw <- rbindlist(lapply(here(paste0("data/mesowest/", my.years, "-meso-weather-data.rds")), readRDS)) # bind all weather data for the selected years
weather.raw <- weather.raw[!is.na(solar) & !is.na(temp.c) & !is.na(dewpt.c) ] #& !is.na(winspd),] # make sure only to select obs w/o NA of desired vars
weather.raw[is.na(winspd), winspd := 0] # set NA windspeeds to zero to prevent errors

# trim weather data to only the dates that we will be using weather data from (defined by model.runs$end.day and model.runs$n.days
my.dates <- unique(do.call("c", mapply(function(x,y) seq(date(x) - days(y - 1), by = "day", length.out = y), model.runs$end.day, model.runs$n.days, SIMPLIFY = F)))
weather.raw <- weather.raw[date(date.time) %in% my.dates,]

# import station metadata and calculate average number of observations of critical weather parameters
stations <- rbindlist(lapply(here(paste0("data/mesowest/", my.years, "-meso-station-data.rds")), readRDS)) # load corresonding years of station metadata
for(s in 1:nrow(stations)){ # calculate the non-NA obs per day for important weather variables
  s.name <- stations[s, station.name]
  day.n <- length(unique(date(weather.raw[station.name == s.name, date.time]))) # number of unqiue days
  stations[s, n.solar.day := weather.raw[station.name == s.name & !is.na(solar), .N] / day.n]
  stations[s, n.dewpt.day := weather.raw[station.name == s.name & !is.na(solar), .N] / day.n]
  stations[s, n.tempc.day := weather.raw[station.name == s.name & !is.na(solar), .N] / day.n]
  stations[s, n.days.obs := day.n]
}

# filter out stations with no or poor data coverage (need at least 1 obs per day)
stations <- stations[is.finite(n.solar.day)  & is.finite(n.tempc.day)  # need required parameters to be finite
                     & n.solar.day > 0 & n.tempc.day > 0 # and positive non-zero
                     & n.days.obs == (length(unique(model.runs$end.day)) * max(model.runs$n.days)),] # and at least one observation per day for all desired dates

# define function to calculate distance in kilometers between two lat/lon points
earth.dist <- function (long1, lat1, long2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

# calculate earth surface distance in km between chosen validation sites and available weather stations
stations[, (my.sites$Location) := as.numeric()] # initialize columns
for(b in 1:length(my.sites$Location)){
  stations[, (my.sites$Location)[b] := earth.dist(my.sites[b, lon], my.sites[b, lat], stations$lon, stations$lat) ]
}

# determine which stations are the closest station to each site
min.stations <- unique(melt(stations[, .SD, .SDcols = c("station.name",paste0(my.sites$Location))], id.vars = "station.name", variable.name = "Location", value.name = "dist.km"))
min.stations <- min.stations[, .SD[which.min(dist.km)], by = Location]
my.sites <- merge(min.stations, my.sites, by = "Location", all = T)
my.sites <- merge(my.sites, stations[, .(station.name, elevation)], by = "station.name", all.x = T)

# funcion to create layers by layer specifications
create.layer <- function(thickness, name, start.depth, nodal.spacing){ # create a layer with defined *thickness* and *name*
  
  # interface node occurs on delta.x location:
  if(thickness/nodal.spacing == round(thickness/nodal.spacing)){
    data.table("x" = seq(start.depth, start.depth + thickness - nodal.spacing, nodal.spacing), "layer" = name)[1, layer := "boundary"]
    
    # interface node occurs off delta.x location:
  } else {
    data.table("x" = seq(start.depth, start.depth + floor(thickness/nodal.spacing) * nodal.spacing, nodal.spacing), "layer" = name)[1, layer := "boundary"]
  }
}

# create empty object to store error messages
my.errors <- NULL

# BEGIN MODEL LOGIC
for(run in 1:model.runs[,.N]){#         nrow(model.runs)  
  tryCatch({  # catch and print errors, avoids stopping model runs 
    
    # SETUP FOR MODEL
    
    # first clear up space for new run
    rm(list=setdiff(ls(), c("run", "model.runs", "layer.profiles", "script.start", "pave.time.ref", "weather",
                            "weather.raw", "create.layer", "my.errors", "my.sites")))
    gc()
    t.start <- Sys.time() # start model run timestamp
    
    # trim weather data to number of days specified
    my.date <- date(model.runs$end.day[run])
    #my.station <- names(which.max(table(my.sites$station.name))) # most commonly close station for all validation sites
    my.station <- my.sites[Location == model.runs$valid.site[run], station.name]
    model.runs$station.name[run] <- my.station
    weather <- weather.raw[date.time >= (my.date - days(model.runs$n.days[run])) & # date.time ends on day of end.day 
                           date(date.time) <= my.date & # ends n days later
                           station.name == my.station] # at station nearest specified validation site
    
    if(weather[,.N] < 12 * model.runs$n.days[run]){
      assign("my.errors", c(my.errors, paste("stopped at run", run, "because too few weather obs")), envir = .GlobalEnv) # store error msg
      break} # break the run if there are fewer than 12 obs a day
    
    # store layer dt
    layer.dt <- layer.profiles[[model.runs$layer.profile[run]]]
    layer.dt[, rho.c := rho * c] # volumetric heat capacity (J/(m3 degK)); [1] concrete: ~2.07E6; ashpalt ~1.42E6 [6]  9.63E5 [8]
    
    # input pavement layer thickness data
    x <- sum(layer.dt$thickness)  # model depth (m);  GUI ET AL: 3.048m [1]
    delta.x <- model.runs$nodal.spacing[run] / 1000 # chosen nodal spacing within pavement (m). default is 12.7mm (0.5in)
    delta.t <- model.runs$time.step[run] # store time step sequence (in units of seconds)
  
    # create n layers as input defines
    n.layers <- nrow(layer.dt)
    for(layer.i in 1:n.layers){
      layer.n <- create.layer(thickness = layer.dt$thickness[layer.i], 
                              name = layer.dt$layer[layer.i], 
                              start.depth = sum(layer.dt$thickness[1:layer.i]) - layer.dt$thickness[layer.i],
                              nodal.spacing = delta.x)
      assign(paste0("layer.",layer.i), layer.n)
    }
  
    # create closely spaced nodes near surface to improve accuracy for near surface interaction
    # until 25 mm depth (0.025 m), start using predifed nodal spacing (recommended again is 12.5mm)
    layer.1 <- rbind(layer.1[1], data.table(x = c(0.001,0.002,0.003,0.005,0.010,0.015,0.025), layer = layer.1$layer[2]), layer.1[x > 0.025])
    
    # bind all layers together
    p.data <- rbindlist(mget(paste0("layer.", 1:n.layers)))

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
                                          "T.K" = rep(seq(from = model.runs$i.top.temp[run] + 273.15, # initial surface temp in K
                                                          to = model.runs$i.bot.temp[run] + 273.15, # initial bottom boundary temp in K
                                                          length.out = p.n)))) # surface temp in K from the pavement to 3m)
    
    # static parameters for pavement heat transfer
    albedo <- model.runs$albedo[run] #layer.dt$albedo[1] #  albedo (dimensionless) [1]; can be: 1 - epsilon for opaque objects
    epsilon <- model.runs$emissivity[run] #  emissivity (dimensionless) [1]; can be: 1 - albedo for opaque objects
    sigma <- 5.67*10^(-8) #  Stefan-Boltzmann constant (W/(m2*K4); [1]
    SVF <- model.runs$SVF[run] #layer.dt$SVF[1]	# sky view factor (dimensionless [0,1]). assume 1.0: pavement is completely visible to sky

    # L in (m) is the characteristic length of the pavement at the test site taken as the ratio of
    L <- model.runs$pave.length[run] # the slab length in the direction of the wind to the perimeter
    # assume horizontal & flat, width b and infinite length L, hotter than the environment -> L = b/2
    # L could vary, but minimum for a pavement should be 2 lanes, or about ~10 meters
    
    # derive key parameters of air (sky temp, dry bulb temp, and convective heat transfer of air) which may vary depending on atmospheric conditions
    weather[, time.s := as.numeric(difftime(weather$date.time, weather$date.time[1], units = "secs"))] # time.s to match with iterations in weather data (assuming first obs is time zero)
    weather[, winspd := winspd * 0.44704] # convert to m/s from mi/h (for h.inf calc)
    weather[, T.inf := temp.c + 273.15] #  atmospheric dry-bulb temperature (K);
    weather[, T.sky := T.inf * (((0.004 * dewpt.c) + 0.8)^0.25)] # sky temperature (K), caluclated using equation (2)
    weather[, v.inf := 1.47E-6 * (T.inf^(3/2)) / (T.inf + 113)] # kinematic viscosity of air (m2/s) for T.inf btwn 100 and 800 K [9]
    weather[, k.inf := ((1.5207E-11) * (T.inf^3)) - ((4.8574E-08) * (T.inf^2)) + ((1.0184E-04) * (T.inf)) - 3.9333E-04] # thermal conductivity of air in W/(m*degK) [0.02, 0.03] (for normal air temps) [2]
    elev <- ifelse(length(unique(my.sites[station.name == my.station, elevation])) == 0, 500, unique(my.sites[station.name == my.station, elevation])) # assume 500 m elevation if no elevation data
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
    aprx <- merge(pave.time[node == 0], weather[, .(time.s, T.sky, T.inf, solar, h.inf)], by = "time.s", all.x = T)
    aprx[, `:=` (T.sky = na.approx(T.sky, 1:.N, na.rm = F), # sky temperature in K
                 T.inf = na.approx(T.inf, 1:.N, na.rm = F), # atmospheric dry-bulb temperature in K
                 solar = na.approx(solar, 1:.N, na.rm = F), # solar radiation in W/m2
                 h.inf = na.approx(h.inf, 1:.N, na.rm = F))]  # wind speed in m/s
    pave.time <- merge(pave.time, aprx[, .(time.s, T.sky, T.inf, solar, h.inf)], by = "time.s", all.x = T)
    
    # add k and rho.c
    pave.time <- merge(pave.time, layer.dt[,.(layer,k,rho.c)], by = "layer", all.x = T, allow.cartesian = T) 
    
    # add boundary/interface thermal contact resistance (R.c)
    boundary.nodes <- pave.time[layer == "boundary" & time.s == 0, node]
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

    # MODEL HEAT TRANSFER OF PAVEMENT
    
    # iterate through from time p to time p.n and model pavement heat transfer at surface, boundary/interface, and interior nodes 
    iterations <- 1:max(model.runs$n.iterations) # always do max iterations but save intermeidate runs if desired
    for(iteration in iterations){
      
      start.time <- Sys.time()
      
      for(p in 1:(p.n-1)){ # state at time p is used to model time p+1, so stop and p-1 to get final model output at time p
        
        if(p%%100 == 0){cat("r =",run,":","p =",p,":", "i =", iteration,":", round(difftime(Sys.time(), start.time, units = "secs"), 2),"secs \n")}
        
        # if this is the first time step, we loop over time p == 1 and the initial conditions until we
        # reach a stable initial condition of pavement temps at depth to ensure the model will converge 
        # this is done because if the initial pavement temperature is not at/near an equilibrium
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
          
          # store a temporary data.table at this timestep to store and transfer all node calculations to master data.table.
          # we do this because the 'shift' function is troublesome within data.table subsets so this is safer and possibly quicker
          tmp <- pave.time[time.s == t.step[p]] 
          
          # for timestep p+1, calc the surface pavement temperature based on parameters at timestep p
          # i.e. surface node s (node = 0), eqn 10 in [1]
          pave.time[time.s == t.step[p+1] & node == 0, T.K := tmp[node == 0, T.K] +  # T.K at node 0 is pavement surface temp (at time p+1)
                      ((((1 - albedo) * tmp[node == 0, solar]) # incoming solar radiation (after albedo reflection)
                        - tmp[node == 0, q.cnv]  # minus convective heat transfer at surface
                        - tmp[node == 0, q.rad] # minus outgoing longwave/infrared radiation at surface
                        + (layer.dt$k[1] * (tmp[node == 1, T.K] - tmp[node == 0, T.K]) / delta.x)) # plus conduction
                       * (2 * delta.t / (layer.dt$rho.c[1] * delta.x)))] # all multiplied by the proportional change in temp per change in depth 
          
          # calculate interior nodes at current timestep p (non-boundary/non-interface nodes) and store into timestep p+1
          tmp[, T.C.i := 
                ((k * delta.t / (rho.c * (delta.x^2))) 
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
          pave.time[time.s == t.step[p] & node == 0, h.flux.up := ((1 - albedo) * tmp[node == 0, solar]) - q.rad[p] - q.cnv[p]] # incoming solar radiation minus outgoing convection and radiation
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
          pave.time[time.s == t.step[p], CFL := (rho.c * (delta.x^2)) / (2 * (h.rad * delta.x + h.inf * delta.x + k))] # in seconds
          pave.time[time.s == t.step[p] & delta.t <= CFL, CFL_fail := 0] # no fail
          pave.time[time.s == t.step[p] & delta.t > CFL, CFL_fail := 1] # yes fail
          
          # break before 200 iterations if the first and second timestep pavement temperatures at each node have converged (within 1/100 of each other)
          # as the initial (p) and p+1 conditions won't change once first converged, this loop breaks always after 1 iteration for all times after p+1
          if(all(is.finite(pave.time[time.s == t.step[2], T.K])) & # all pave temps at p==2 also need to be finite (b/c below check doesn't work for all NAs)
             all(round(pave.time[time.s == t.step[1], T.K], 1) == round(pave.time[time.s == t.step[2], T.K], 1), na.rm = T) == T){break}
          
          # for the first time step only re-adjust the initial pavement temps
          # to ensure convergence before of intital state before solving past the initial timestep of model
          # we do this after the check to break because if the initial condition has converged, this isn't needed
          if(p == 1){
            pave.time[time.s == t.step[1], T.K := pave.time[time.s %in% t.step[1:2], mean(T.K), by = node][,V1] ] # mean of t1 and 12 T.K
            #pave.time[time.s == t.step[1], T.K := pave.time[time.s == t.step[2], T.K] ]
          } else { break } # if not first time step, always only go through each time step once
          
        } # end z loop that repeats only when times p=1 and p=2 haven't converged
        
        # if within this timestep, T.K at p+1 ended up non-finite due to an error or convergance issue, break the loop (to prevent wasted time)
        if(any(!is.finite(pave.time[time.s == t.step[p+1], T.K]))){
          model.runs$broke.t[run] <- t.step[p] # store time model stopped
          assign("my.errors", c(my.errors, paste("stopped model run",run,"at",model.runs$broke.t[run],"secs due to non-finite temperatures detected")), envir = .GlobalEnv) # store error msg
          break
        } 
        
        # if within this timestep, the CFL condition fails anywhere, break the loop (to prevent wasted time)
        if(sum(pave.time[time.s == t.step[p], CFL_fail], na.rm = T) > 0){
          model.runs$broke.t[run] <- t.step[p] # store time model stopped
          assign("my.errors", c(my.errors, paste("stopped model run",run,"at",model.runs$broke.t[run],"secs due to CFL stability condition not satisfied")), envir = .GlobalEnv) # store error msg
          break
        } 
          
      } # end time step p, go to time p+1
      
      # also break the iterations loop too (another iteration is unlikely to fix unstability b/c parameters for this run need changing)
      if(!is.na(model.runs$broke.t[run])){break}
      
      # if the iteration is not the last iteration and all pavement temp values in the timestep N days in the future are finite, then
      # store these pavement temp values from the future time step as the initial pavement temps for the new iteration 
      # because these are likely to be more accurate than the chosen initials
      if(iteration != max(iterations) & 
         all(is.finite(pave.time[date.time == min(date.time) + days(model.runs$n.days[run]), T.K]))){
        pave.time[time.s == 0, T.K := pave.time[date.time == max(date.time), T.K] ]
        
      } else { # otherwise, for model iteration where output is desired (usually max iteration), some housekeeping:
        
        pave.time[, T.degC := T.K - 273.15] # create temp in deg C from Kelvin
        pave.time <- pave.time[time.s != t.step[p.n]] # remove the last time step (not calculated). p.n-1 occurs on the final weather obs
        pave.time[, delta.T.mean := T.degC - mean(T.degC, na.rm = T), by = node] # mean change in temp by node, use for RMSE
        dir.create(here("data/outputs/1D-heat-model-runs/"), showWarnings = FALSE) # creates output folder if it doesn't already exist
        saveRDS(pave.time, here(paste0("data/outputs/1D-heat-model-runs/run_",run,"_output.rds"))) # save model iteration R object
        model.runs$run.time[run] <- as.numeric(round(difftime(Sys.time(),t.start, units = "mins"),2)) # store runtime of model iteration in minutes
        
        # if the run is a reference run (first run of a unique layer profile scheme), store it as the reference run for other scenarios under the same layer profile scheme
        if(run == model.runs[run.n == run, ref]){
          pave.time.ref <- pave.time
        }
        
        # if run isn't a reference run, then store RSME compared to the appropriate ref run
        # note that RMSE is mean centered temperature across all nodes and timesteps.
        if(run != model.runs[run.n == run, ref]){ # if the model isn't the refrence run
          model.runs$RMSE[run] <- sqrt(mean((pave.time[pave.time.ref, .(time.s,depth.m,delta.T.mean), on = c("time.s","depth.m")][!is.na(delta.T.mean), delta.T.mean] - 
                                               pave.time.ref[pave.time, .(time.s,depth.m,delta.T.mean), on = c("time.s","depth.m")][!is.na(delta.T.mean), delta.T.mean])^2))
        } # end RMSE calcs
        
      } # end the 'end of iteration' steps
      
    } # end model iteration and continue to next 
    
    }, error = function(e){ # catch and print/store errors
      
      cat("ERROR:",conditionMessage(e), "\n") # print error msg in console
      assign("my.errors", c(my.errors, conditionMessage(e)), envir = .GlobalEnv)}) # store error message in list 
  
} # end run, go to next run

write.csv(model.runs, here("data/outputs/1D-heat-model-runs/model_runs_metadata.csv"), row.names = F) # output model run metadata
write.csv(my.sites, here("data/outputs/1D-heat-model-runs/validation_sites.csv"), row.names = F)
saveRDS(layer.profiles, here("data/outputs/1D-heat-model-runs/layer_profiles.rds"))

# load email creds and construct msg to notify you by email the script has finished
my.email <- as.character(fread(here("email.txt"), header = F)[1]) 
my.pass <- as.character(fread(here("email.txt"), header = F)[2])
msg <- paste0("Completed model run on ", Sys.info()[4]," at ", Sys.time(), ". Model run took ", round(difftime(Sys.time(),script.start, units = "mins"),0)," minutes to complete.")
my.errors <- paste0(my.errors,". ")
msg <- paste(msg,"ERRORS:",paste(my.errors, collapse =""))
send.mail(from = my.email,
          to = my.email,
          subject = "R Script Finished",
          body = msg,
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
