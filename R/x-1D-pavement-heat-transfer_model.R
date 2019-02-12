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
# useful for evaluating  mulitple types of pavement scenarios and model senstivity in a batch of simulations

# specify layer profiles as list of data.tables 
# note that for 2 touching layers to be unique, they must have no thermal contact resistance (R.c == 0) 
# if there thermal conductivies are equivalent (k)
layer.profiles <- list(
  data.table( # light weight asphalt layer 1
    layer = c("surface","base","subgrade"),
    thickness = c(0.1, 0.1, 2.8), # layer thickness (m)
    k = c(1.21, 1.21, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2238, 2238, 1500), # layer density (kg/m3)
    c = c(921, 921, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.17,NA,NA), # surface albedo (dimensionless)
    R.c.top = c(NA,0,0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # light weight asphalt layer 1
    layer = c("surface","base","subgrade"),
    thickness = c(0.1, 0.1, 2), # layer thickness (m)
    k = c(0.841, 2.590, 0.4), # layer thermal conductivity (W/(m*degK)) 
    rho = c(1686, 2000, 1500), # layer density (kg/m3)
    c = c(921, 921, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.17,NA,NA), # surface albedo (dimensionless)
    R.c.top = c(NA,0,0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # normal weight asphalt layer 1,
    layer = c("surface","base","subgrade"),
    thickness = c(0.075, 0.10, 2), # layer thickness (m)
    k = c(2.1, 1.21, 0.4), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2585, 2000, 1400), # layer density (kg/m3)
    c = c(921, 921, 1200), # layer specific heat (J/(kg*degK)
    albedo = c(0.17,NA,NA), # surface albedo (dimensionless)
    R.c.top = c(NA,0,0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # bitumen layer 1, aggregate layer 2 (S.C. Some` et al. 2012)
    layer = c("surface","base","subgrade"),
    thickness = c(0.1, 0.2, 2.5), # layer thickness (m)
    k = c(0.2, 2.590, 0.4), # layer thermal conductivity (W/(m*degK)) 
    rho = c(1094, 1111, 1400), # layer density (kg/m3)
    c = c(921, 921, 1200), # layer specific heat (J/(kg*degK)
    albedo = c(0.17,NA,NA), # surface albedo (dimensionless)
    R.c.top = c(NA,5.0E-4,0) # thermal contact resistance at top boundary of layer (dimensionless) 
  )
)

# The specific heat of dense-graded asphalt and concrete are very similar [7]

# for Phoenix City streets, the following are provided as minimum thicknesses for asphaltic concrete and base materials:
# Arterial: 152.4 mm (6in)
# Major Collector Street: 114.3 mm (4.5 in)
# Residential Collector Street: 76.2 mm (3 in)
# Local Street: 50.8 mm (2 in)

# Base materials may not be required for full depth asphaltic concrete design. 
# However, if base materials are required, then the minimum thickness will be: 152.4 mm (6in) for all base material types

# Design structural number (SN) can be converted to thickness of various flexible pavement layers by using structural layer coefficients.
# In the absence of specific values, the following structural coefficients are recommended: 
# Asphaltic concrete 0.39
# Aggregate base 0.12
# Select material 0.11
# Cement treated base 0.27
# Bituminous Treated Base 0.31 

# create list of models to run with varied inputs to check sentivity/error
# first values (will be first scenario in list of model runs) is the replication from Gui et al. [1] 
# this is assumed to be the model run we check error against to compare model preformance
models <- list(run.n = c(0), # dummy run number (replace below)
               nodal.spacing = c(12.5,10),# nodal spacing in millimeters
               n.iterations = c(5,20), # number of iterations to repeat each model run
               i.top.temp = c(33.5), # starting top boundary layer temperature in deg C
               i.bot.temp = c(33.5), # starting bottom boundary layer temperature in deg C. ASSUMED TO BE CONSTANT 
               time.step = c(120), # time step in seconds
               pave.length = c(10,75), # characteristic length of pavement in meters
               layer.profile = 1:length(layer.profiles), # for each layer.profile, create a profile to id 
               n.days = c(7) # number of days to simulate 
)

model.runs <- as.data.table(expand.grid(models)) # create all combinations in model inputs across profiles
model.runs$run.n <- seq(from = 1, to = model.runs[,.N], by = 1) # create run number id
model.runs[,`:=`(run.time = 0, RMSE = 0, CFL_fail = 0)] # create output model run summary variables
model.runs[, ref := min(run.n), by = layer.profile] # create refrence model for each unique layer profile (defaults to first scenario of parameters)
paste0("Estimated run time for all ",model.runs[,.N], " runs: ", round(sum(
       model.runs$n.iterations * # number of iterations
       model.runs$n.days *  # number of days to simulate
       model.runs$layer.profile * sapply(1:length(layer.profiles), # depth of pavement
                                         function (x) sum(layer.profiles[[x]]$thickness)) *
       model.runs$time.step /
       model.runs$nodal.spacing) / 1420, digits = 2),
       " hrs")

# LOAD & FILTER WEATHER DATA 
#weather <- readRDS(here("data/outputs/temp/sample-weather-data.rds")) # sample 3 day period of weather data
#weather <- readRDS(here("data/outputs/temp/2017-weather-data.rds")) # all weather data from cleaning script
weather.raw <- rbindlist(list(readRDS(here("data/outputs/2017-weather-data-1.rds")), # all weather data saved to repo
                          readRDS(here("data/outputs/2017-weather-data-2.rds")),
                          readRDS(here("data/outputs/2017-weather-data-3.rds"))))
weather.raw <- weather.raw[station.name == "City of Glendale" & source == "MCFCD" & month == "Jun",] # choose station for desired period of time
weather.raw <- weather.raw[!is.na(solar) & !is.na(temp.c) & !is.na(dewpt.c) & !is.na(windir),] # make sure only to select obs with no NA of desired vars

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

# BEGIN MODEL LOGIC
for(run in 1:model.runs[,.N]){ #      nrow(model.runs)  
  tryCatch({  # catch and print errors, avoids stopping model runs 
    
    # SETUP FOR MODEL
    
    # first clear up space for new run
    rm(list=setdiff(ls(), c("run","model.runs","layer.profiles","script.start","pave.time.ref","weather","weather.raw","create.layer")))
    gc()
    t.start <- Sys.time() # start model run timestamp
    
    # trim weather data to number of days specified
    weather <- weather.raw[date.time <= (min(date.time) + days(model.runs$n.days[run]))] 
    
    # store layer dt
    layer.dt <- layer.profiles[[model.runs$layer.profile[run]]]
    layer.dt[, rho.c := rho * c] # volumetric heat capacity (J/(m3 degK)); [1] concrete: ~2.07E6; ashpalt ~1.42E6 [6]
    
    # input pavement layer thickness data
    x <- sum(layer.dt$thickness)  # model depth (m);  GUI ET AL: 3.048m [1]
    delta.x <- model.runs$nodal.spacing[run]/1000 # chosen nodal spacing within pavement (m). default is 12.7mm (0.5in)
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
  
    # bind all layers together
    p.data <- rbindlist(mget(paste0("layer.",1:n.layers)))

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
                                          "T.K" = rep(seq(from = model.runs$i.top.temp[run]+273.15, # initial surface temp in K
                                                          to = model.runs$i.bot.temp[run]+273.15, # initial bottom boundary temp in K
                                                          length.out = p.n)))) # surface temp in K from the pavement to 3m)
    
    # static parameters for pavement heat transfer
    albedo <- layer.dt$albedo[1] #  albedo (dimensionless) [1]; can be: 1 - epsilon for opaque objects
    epsilon <- 0.8 #  emissivity (dimensionless) [1]; can be: 1 - albedo for opaque objects
    sigma <- 5.67*10^(-8) #  Stefan-Boltzmann constant (W/(m2*K4); [1]
    SVF <- 1	# sky view factor (dimensionless [0,1]). assume 1.0: pavement is completely visible to sky
    Pr.inf <- 0.7085 #  Prandtl number (dimensionless) [0.708, 0.719] (50 to -50 degC range)
    
    # L in (m) is the characteristic length of the pavement at the test site taken as the ratio of
    L <- model.runs$pave.length[run] # the slab length in the direction of the wind to the perimeter
    # assume horizontal & flat, width b and infinite length L, hotter than the environment -> L = b/2
    # L could vary, but minimum for a pavement should be 2 lanes, or about ~10 meters
    
    # parameters that vary by pavement layer
    alpha <- 4.0  # thermal diffusivity (m^2/s), typically range from 2 to 12; [1]. 

    # calculate parameters that vary by time/weather
    weather$time.s <- as.numeric(difftime(weather$date.time, weather$date.time[1], units = "secs")) # time.s to match with iterations in weather data (assuming first obs is time zero)
    weather$winspd <- weather$winspd * 0.44704 # convert to m/s from mi/h
    weather$T.inf <- weather$temp.c + 273.15 #  atmospheric dry-bulb temperature (K);
    weather$T.sky <- weather$T.inf * (((0.004 * weather$dewpt.c) + 0.8)^0.25) # sky temperature (K), caluclated using equation (2)
    weather$v.inf <- (16.92E-6) * ((weather$temp.c / 40)^0.7) # emperical fit for kinematic viscosity of air using refrence of 30 deg C where v.inf is 15.98E-6 m2/s  [4],[5]
    weather$k.inf <- ((1.5207E-11) * (weather$T.inf^3)) - ((4.8574E-08) * (weather$T.inf^2)) + ((1.0184E-04) * (weather$T.inf)) - 3.9333E-04 # Ref [2] # thermal conductivity of air in W/(m2*degK)
    weather$h.inf <- 0.664 * (weather$k.inf * (Pr.inf ^ 0.3) * (weather$v.inf ^ -0.5) * (L ^ -0.5) * (weather$winspd ^ 0.5)) # convective heat transfer coefficient in W/(m2*degK)
    
    # iterpolate between all observation data to create weather data at each timestep
    # first create new data.table for a single node with all obervational weather data matched by timestep (timesteps match)
    # then linearlly approximate between the weather observations for all NA timesteps using zoo::na.approx
    # finally merge to orginal data.table to have all weather observations present such that missing ones are interplotated between
    aprx <- merge(pave.time[node == 0], weather[, .(time.s, T.sky, T.inf, solar, winspd)], by = "time.s", all.x = T)
    aprx[, `:=` (T.sky = na.approx(T.sky, 1:.N, na.rm = F), # sky temperature in K
                 T.inf = na.approx(T.inf, 1:.N, na.rm = F), # atmospheric dry-bulb temperature in K
                 solar = na.approx(solar, 1:.N, na.rm = F), # solar radiation in W/m2
                 winspd = na.approx(winspd, 1:.N, na.rm = F))]  # wind speed in m/s
    pave.time <- merge(pave.time, aprx[, .(time.s, T.sky, T.inf, solar, winspd)], by = "time.s", all.x = T)
    
    # estimate non-linear parameters
    pave.time[, v.inf := (16.92E-6) * (((T.inf - 273.15) / 40)^0.7)] #  kinematic viscosity of air [4], [5]; range: [10E-6,14E-6]
    pave.time[, k.inf := ((1.5207E-11) * (T.inf^3)) - ((4.8574E-08) * (T.inf^2)) + ((1.0184E-04) * (T.inf)) - 3.9333E-04] # thermal conductivity of air in W/(m*degK) [0.02, 0.03] (for normal air temps) [2]
    pave.time[, h.inf := 0.664 * (k.inf * (Pr.inf ^ 0.3) * (v.inf ^ -0.5) * (L ^ -0.5) * (winspd ^ 0.5))] # convective heat transfer coefficient in W/(m2*degK) [0.5, 1000]
    
    # add k and rho.c
    pave.time <- merge(pave.time, layer.dt[,.(layer,k,rho.c)], by = "layer", all.x = T) 
    
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
    pave.time <- merge(pave.time, up.dn[, .(node, k.up, k.dn, x.up, x.dn)], by = "node", all.x = T)
    
    # also create/store a few other things
    pave.time[, date.time := weather$date.time[1] + seconds(time.s)] # add date.time column
    pave.time[time.s != 0, T.K := NA] # only initial temps are assumed, else NA
    pave.time[, layer := as.character(layer)] # to avoid issues w/ factors
    
    # parameters at surface that change by timestep
    T.s <- vector(mode = "numeric", length = p.n) # pavement suface temp in K
    h.rad <- vector(mode = "numeric", length = p.n) # radiative heat transfer coefficient in W/(m2*degK) 
    q.rad <- vector(mode = "numeric", length = p.n) # infrared radiation heat transfer at surfaceW/m2
    q.cnv <- vector(mode = "numeric", length = p.n) # convection heat transfer W/m2
    
    # MODEL HEAT TRANSFER OF PAVEMENT
    
    # iterate through from time p to time p.n and model pavement heat transfer at surface, boundary/interface, and interior nodes 
    iterations <- 1:model.runs$n.iterations[run] # 10 iteration cycles in [1] was found to produce the most accurate results
    for(iteration in iterations){
      for(p in 1:(p.n-1)){ # state at time p is used to model time p+1, so stop and p-1 to get final model output at time p
        
        # start by resetting the very bottom node to its starting temperature
        # we assume that all material below our bottom boundary is always cooler or at equilibrium with our bottom node
        # and in this way it acts as a heat sink: any heat that would cross this boundary disipates in the depths below
        # we do this first to ensure time p+1 calculates using this condition of a constant bottom boundary
        pave.time[time.s == t.step[p] & node == max(node), T.K := model.runs$i.bot.temp[run] + 273.15]
        
        # for the current timestep p, store the pavement surface temperature (in K) 
        T.s[p] <- pave.time[time.s == t.step[p] & node == 0, T.K]
        
        # for timestep p, calc the surface radiative coefficient
        h.rad[p] <- SVF * epsilon * sigma * ((T.s[p]^2) + (pave.time[time.s == t.step[p] & node == 0, T.sky]^2)) * (T.s[p] + pave.time[time.s == t.step[p] & node == 0, T.sky]) # radiative heat transfer coefficient in W/(m2*degK) 
                
        # for timestep p, calc the surface heat transfer parameters (infrared radiation & convection)
        q.rad[p] <- SVF * epsilon * sigma * ((T.s[p]^4) - (pave.time[time.s == t.step[p] & node == 0, T.sky]^4)) # infrared radiation heat transfer at surfaceW/m2
        q.cnv[p] <- pave.time[time.s == t.step[p] & node == 0, h.inf] * (T.s[p] - pave.time[time.s == t.step[p] & node == 0, T.inf]) # convection heat transfer W/m2
      
        # store a temporary data.table at this timestep to store and transfer all node calculations to master data.table.
        # we do this because the 'shift' function is troublesome within data.table subsets so this is safer
        tmp <- pave.time[time.s == t.step[p]] 
        
        # for timestep p+1, calc the surface pavement temperature based on parameters at timestep p
        # i.e. surface node s (node = 0), eqn 10 in [1]
        pave.time[time.s == t.step[p+1] & node == 0, T.K := T.s[p] +  # T.K at node 0 is pavement surface temp (at time p+1)
                    ((((1 - albedo) * tmp[node == 0, solar]) # incoming solar radiation (after albedo reflection)
                      - q.cnv[p]  # minus convective heat transfer at surface
                      - q.rad[p]  # minus outgoing longwave/infrared radiation at surface
                      + (layer.dt$k[1] * (tmp[node == 1, T.K] - T.s[p]) / delta.x)) # plus conduction
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
      
      } # end time step p, go to time p+1
      
      # if the iteration is not the last iteration and all pavement temp values in the timestep N days in the future are finite, then
      # store these pavement temp values from the future time step as the initial pavement temps for the new iteration
      if(iteration != max(iterations) & 
         all(is.finite(pave.time[date.time == min(date.time) + days(model.runs$n.days[run]), T.K]))){
        pave.time[time.s == 0, T.K := pave.time[date.time == min(date.time) + days(model.runs$n.days[run]), T.K]]
      } 
      
    } # end model iteration and continue to next 
    
    # once model iteration are complete, some housekeeping:
    pave.time[, T.degC := T.K - 273.15] # create temp in deg C from Kelvin
    pave.time <- pave.time[time.s != t.step[p.n]] # remove the last time step (not calculated). timesteps were given such that p.n-1 is the final weather obs
    pave.time[, delta.T.mean := T.degC - mean(T.degC, na.rm = T), by = node] # Temp at time and node difference from 3 day mean, use for RMSE
    dir.create(here("data/outputs/1D-heat-model-runs/"), showWarnings = FALSE) # creates output folder if it doesn't already exist
    saveRDS(pave.time, here(paste0("data/outputs/1D-heat-model-runs/run_",run,"_output.rds"))) # save model iteration R object
    model.runs$run.time[run] <- as.numeric(round(difftime(Sys.time(),t.start, units = "mins"),2)) # store runtime of model iteration in minutes
    
    # if the run is a reference run (first run of a unique layer profile scheme), store it as the reference run for other scenarios under the same layer profile scheme
    if(run == model.runs[run.n == run, ref]){
      pave.time.ref <- pave.time
    }
    
    # if run isn't a reference run, then store RSME compared to the appropriate ref run
    if(run != model.runs[run.n == run, ref]){ # if the model isn't the refrence run
      model.runs$RMSE[run] <- sqrt(mean((pave.time[pave.time.ref, .(time.s,depth.m,delta.T.mean), on = c("time.s","depth.m")][!is.na(delta.T.mean), delta.T.mean] - 
                                           pave.time.ref[pave.time, .(time.s,depth.m,delta.T.mean), on = c("time.s","depth.m")][!is.na(delta.T.mean), delta.T.mean])^2))
    } # note that RMSE is mean centered temperature across all nodes and timesteps.
    
    # check finite difference solving method satisfied the Courant-Friedrichs-Lewy (CFL) condition for stability
    # and store if all instances are satisfied in the model.runs metadata
    # in other words, this checks that delta.t and delta.x are sufficently small such that the solving method is stable/reliable
    # delta.t must be less than or equal to the t.delta.min values to satisfy CFL condition
    # so it is not if it is greater than the minimum observed t.delta.min values
    h.inf <- pave.time[node == 0, h.inf]
    for(layer.i in 1:n.layers){
      v <- vector(mode = "numeric", length = length(h.rad)) # first create empty vectors to store of min allowed change in time (RHS of eqn 13 from [1])
      for(a in 1:length(h.rad)){ # calc values for CFL check for each timestep (RHS eqn. 13 in [1])
        v[a] <- (layer.dt$rho.c[layer.i] * (delta.x^2)) / (2 * (h.rad[a] * delta.x + h.inf[a] * delta.x + layer.dt$k[layer.i])) # in seconds
        }
      layer.dt[layer.i, min.delta.t := min(v, na.rm = T)]
      layer.dt[layer.i, p.fail.CFL := round(sum(ifelse(delta.t <= v, 1, 0), na.rm = T) / sum(length(v), na.rm = T), 2)]
    }
    if(delta.t > min(layer.dt$min.delta.t, na.rm = T)){
      model.runs$CFL_fail[run] <- 1
      } 
    # end CFL condition checking
    
    }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")}) # print error message if model run had error
} # end run, go to next run

write.csv(model.runs, here("data/outputs/1D-heat-model-runs/model_runs_metadata.csv"), row.names = F) # output model run metadata
saveRDS(layer.profiles, here("data/outputs/1D-heat-model-runs/layer_profiles.rds"))

# load email creds and construct msg to notify you by email the script has finished
my.email <- as.character(fread(here("email.txt"), header = F)[1]) 
my.pass <- as.character(fread(here("email.txt"), header = F)[2])
msg <- paste0("Completed model run on ", Sys.info()[4]," at ", Sys.time(), ". Model run took ", round(difftime(Sys.time(),script.start, units = "mins"),0)," minutes to complete.")
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



#plot(pave.time[node == 0, .(date.time, T.degC)],type="l",col="red")
#plot(pave.time[node == max(node), .(date.time, T.degC)],type="l",col="red")
#plot(pave.time[node == max(node)-1, .(date.time, T.degC)],type="l",col="red")