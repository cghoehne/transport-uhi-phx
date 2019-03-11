# 1D model based on fundamental energy balance to calculate the pavement temperatures
# validation for specific asphalt lab testing outlined in Hassn et al. 2015 http://dx.doi.org/10.1016/j.matdes.2015.11.116

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
layer.profiles <- list(
  data.table( # parking lot asphalt w/ small tracks, med qualtiy subsoil
    layer = c("aspahlt", "acrylic"),
    thickness = c(0.05, 0.0105), # layer thickness (m)
    k = c(1.16, 0.2), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2371.67, 1180), # layer density (kg/m3) 2382 (base from infravation)
    c = c(963.70, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.05, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA), # emissivity (dimensionless)
    #SVF = c(0.5,NA), # sky view factor
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # PCC whitetopping bonded on HMA (lower density roads)
    layer = c("aspahlt", "acrylic"),
    thickness = c(0.05, 0.01), # layer thickness (m)
    k = c(0.96, 0.2), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2186.75, 1180), # layer density (kg/m3)
    c = c(957.77, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.05, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA), # emissivity (dimensionless)
    #SVF = c(0.5,NA,NA), # sky view factor
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Ordinary PCC + some additives (med density)
    layer = c("aspahlt", "acrylic"),
    thickness = c(0.05, 0.01), # layer thickness (m)
    k = c(0.92, 0.2), # layer thermal conductivity (W/(m*degK)) 
    rho = c( 2092.65, 1180), # layer density (kg/m3)
    c = c(953.03, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.05, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA), # emissivity (dimensionless)
    #SVF = c(0.5,NA), # sky view factor
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # major arterial HMA rebonded (OGFC 20mm on 280mm DGHMA)
    layer = c("aspahlt", "acrylic"),
    thickness = c(0.05, 0.01), # layer thickness (m)
    k = c(0.90, 0.2), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2004.70, 1180), # layer density (kg/m3) 2382 (base from infravation)
    c = c(947.11, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.05, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA), # emissivity (dimensionless)
    #SVF = c(0.5,NA,NA), # sky view factor
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # low traffic 5 layer road HMA rebonded (DFG x 3)
    layer = c("aspahlt", "acrylic"),
    thickness = c(0.05, 0.01), # layer thickness (m)
    k = c(0.82, 0.2), # layer thermal conductivity (W/(m*degK)) 
    rho = c(1906.10, 1180), # layer density (kg/m3) 2382 (base from infravation)
    c = c(945.92, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.05, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA), # emissivity (dimensionless)
    #SVF = c(0.5,NA,NA), # sky view factor
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# name profiles
names(layer.profiles) <- c("5% air voids", 
                           "13.2% air voids", 
                           "17.4% air voids", 
                           "21.5% air voids", 
                           "25.3% air voids")

# create list of models to run with varied inputs to check sentivity/error
# first values in each vector will correspond to a refrence run for RMSE calcs where appropriate
models <- list(run.n = c(0), # dummy run number (replace below)
               nodal.spacing = c(4),# nodal spacing in millimeters
               n.iterations = c(1), # number of iterations to repeat each model run
               i.top.temp = c(25), # starting top boundary layer temperature in deg C
               i.bot.temp = c(25), # starting bottom boundary layer temperature in deg C. ASSUMED TO BE CONSTANT 
               time.step = c(15), # time step in seconds
               pave.length = c(10), # characteristic length of pavement in meters
               #albedo = c(0.2,0.3), # surface albedo
               SVF = c(1), # sky view factor
               layer.profile = 1:length(layer.profiles), # for each layer.profile, create a profile to id
               #end.day = valid.dates[, date(date.time)], # date on which to end the simulation (at midnight)
               end.day = NA, # date on which to end the simulation (at midnight)
               n.days = c(1) # number of days to simulate 
)

model.runs <- as.data.table(expand.grid(models)) # create all combinations in model inputs across profiles
model.runs$run.n <- seq(from = 1, to = model.runs[,.N], by = 1) # create run number id
model.runs[,`:=`(run.time = 0, RMSE = 0, CFL_fail = 0, broke.t = NA)] # create output model run summary variables
model.runs[, ref := min(run.n), by = layer.profile] # create refrence model for each unique layer profile (defaults to first scenario of parameters)
model.runs[, depth := rep(sapply(1:length(layer.profiles), function (x) sum(layer.profiles[[x]]$thickness)), each = model.runs[layer.profile == 1, .N])]
for(a in 1:length(layer.profiles)){ # record other layer profile properties in model.runs
  model.runs[layer.profile == a, albedo := layer.profiles[[a]]$albedo[1]] 
  model.runs[layer.profile == a, emissivity := layer.profiles[[a]]$emissivity[1]]
  model.runs[layer.profile == a, pave.name := names(layer.profiles)[a]]
  #model.runs[layer.profile == a, valid.site := layer.sites[a]]
}


# 4x 250 W lamps centered in 70 x 70 mm grid over asphalt
# 306 × 306 mm asphalt surface area
# simplify lamps as 1x 1000 W at center from 730 mm
# calc using with inverse square law
area <- 0.306^2 # meters squared
dist <- 0.730 # meters
cov.area <- pi * (dist^2) # meters squared area of coverage each lamp at 730 mm disance from surface
power <- 250 # watts 
tot.intensity <- (power / area) / (dist^2) # watts 
pave.intensity <- tot.intensity * area / cov.area # intensity in watts that reachs all of pave surface per lamp

# create empty object to store error messages
my.errors <- NULL

# create output folder name as script start time
out.folder <-paste0("data/outputs/1D-heat-model-runs/",
                    format(strptime(script.start, format = "%Y-%m-%d %H:%M:%S"), format = "%Y%m%d_%H%M%S"),
                    "_model_outputs_LAB/")
dir.create(here(out.folder), showWarnings = FALSE)

# BEGIN MODEL LOGIC
for(run in 1:model.runs[,.N]){  #
  tryCatch({  # catch and print errors, avoids stopping model runs 
    
    # SETUP FOR MODEL
    
    # first clear up space for new run
    rm(list=setdiff(ls(), c("run", "model.runs", "layer.profiles", "script.start", "pave.time.ref",
                             "create.layer", "my.errors", "pave.intensity", "area", "out.folder")))
    gc()
    t.start <- Sys.time() # start model run timestamp

    # store layer dt
    layer.dt <- layer.profiles[[model.runs$layer.profile[run]]]
    layer.dt[, rho.c := rho * c] # volumetric heat capacity (J/(m3 degK)); [1] concrete: ~2.07E6; ashpalt ~1.42E6 [6]  9.63E5 [8]
    
    # input pavement layer thickness data
    x <- sum(layer.dt$thickness)  # model depth (m);  GUI ET AL: 3.048m [1]
    delta.x <- model.runs$nodal.spacing[run] / 1000 # chosen nodal spacing within pavement (m). default is 12.7mm (0.5in)
    delta.t <- model.runs$time.step[run] # store time step sequence (in units of seconds)
  
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
      thickness <- thickness + (nodal.spacing * 2)
      
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
    
    # create ordered list of boundaries and layers (no bottom boundary created)
    my.layers <- c()
    for(layer.i in 1:n.layers){
      my.layers <- c(my.layers, paste0("boundary.",layer.i), paste0("layer.",layer.i))
    }
    
    # bind all boundaries/layers together
    p.data <- rbindlist(mget(my.layers))

    # create timestep columns
    t.step <- seq(from = 0, # from time zero
                  to = as.numeric(seconds(days(model.runs$n.days[run]))) + delta.t, 
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
    
    pave.time[, `:=` (solar = 4 * pave.intensity / area, temp.c = 25, dewpt.c = 9, winspd = 0)]
    pave.time[, date.time := as.POSIXct(as.Date("2019-01-01") + seconds(time.s))]
    
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
    pave.time[, time.s := as.numeric(difftime(pave.time$date.time, pave.time$date.time[1], units = "secs"))] # time.s to match with iterations in pave.time data (assuming first obs is time zero)
    pave.time[, winspd := winspd * 0.44704] # convert to m/s from mi/h (for h.inf calc)
    pave.time[, T.inf := temp.c + 273.15] #  atmospheric dry-bulb temperature (K);
    pave.time[, T.sky := T.inf * (((0.004 * dewpt.c) + 0.8)^0.25)] # sky temperature (K), caluclated using equation (2)
    pave.time[, v.inf := 1.47E-6 * (T.inf^(3/2)) / (T.inf + 113)] # kinematic viscosity of air (m2/s) for T.inf btwn 100 and 800 K [9]
    pave.time[, k.inf := ((1.5207E-11) * (T.inf^3)) - ((4.8574E-08) * (T.inf^2)) + ((1.0184E-04) * (T.inf)) - 3.9333E-04] # thermal conductivity of air in W/(m*degK) [0.02, 0.03] (for normal air temps) [2]
    elev <- 46 # elevation of Nottingham UK (assumed location of lab)
    p.air.dry <- (-8.5373296284E-09 * (elev^3)) + (5.2598726265E-04 * (elev^2)) - (1.1912694417E+01 * elev) + 1.0136022009E+05 # estimate of dry air pressure (Pa) via elevation (see air-emperical-fit.xlsx for fit)
    pave.time$p.wat.vap <- (100 - 5 * (pave.time$temp.c - pave.time$dewpt.c)) * (6.1078 * (10^(7.5 * pave.time$temp.c / (pave.time$temp.c + 237.3)))) # RH (approx) * saturation vapor pressure
    pave.time$p.air.hum <- (p.air.dry / 287.058 / pave.time$T.inf) + (pave.time$p.wat.vap / 461.495 / pave.time$T.inf)  # density of humid air (kg/m3)
    pave.time[, c.p.air := (2E-04 * (T.inf^2)) - (7E-02 * T.inf) + 1.008E+03] # specific heat capacity of air (J/(kg*degK)(see air-emperical-fit.xlsx for emperical fit
    pave.time[, Pr.inf := 1.6380e-05 * p.air.hum * c.p.air / k.inf] # Prandtl number, should be 0.70 - 0.72 for air between 0 and 100 C at 0 to 10 bar
    pave.time[, h.inf := 0.664 * (k.inf * (Pr.inf ^ 0.3) * (v.inf ^ -0.5) * (L ^ -0.5) * (winspd ^ 0.5))] # convective heat transfer coefficient in W/(m2*degK) [0.5, 1000]
    
    # delete unnecessary columns
    pave.time[, c("v.inf","k.inf","p.wat.vap","p.air.hum","c.p.air","Pr.inf"):=NULL]
    
    # add k and rho.c
    pave.time <- merge(pave.time, layer.dt[,.(layer,k,rho.c)], by = "layer", all.x = T, allow.cartesian = T) 
    
    # add interior boundary/interface thermal contact resistance (R.c)
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
    pave.time[time.s != 0, T.K := NA] # only initial temps are assumed, else NA
    pave.time[, layer := as.character(layer)] # to avoid issues w/ factors
    #pave.time[node == max(node)-1,]

    # let time = 0 have lights off (solar = 0 so that model converges to stable initial condition)
    pave.time[time.s == 0, solar := 0]
    
    # get rid of extra node at bottom
    pave.time <- pave.time[node != max(node)]
    
    # MODEL HEAT TRANSFER OF PAVEMENT
    
    # iterate through from time p to time p.n and model pavement heat transfer at surface, boundary/interface, and interior nodes 
    iterations <- 1:max(model.runs$n.iterations) # always do max iterations but save intermeidate runs if desired
    for(iteration in iterations){
      
      start.time <- Sys.time()
      
      for(p in 1:(p.n-1)){ # state at time p is used to model time p+1, so stop and p-1 to get final model output at time p
        
        # print progress
        if(p%%100 == 0){cat("r =", run, "/", model.runs[,.N], paste0("(", signif(run/model.runs[,.N], 2) * 100,"%)"),"|",
                            "p =", p, "/", p.n, paste0("(", signif(p/p.n, 2) * 100,"%)"), "|",  
                            round(difftime(Sys.time(), start.time, units = "min"), 0),"mins run", "|",
                            round(difftime(Sys.time(), script.start, units = "min"), 0),"mins script \n")}
        
        # if this is the first time step, we loop over time p == 1 and the initial conditions until we
        # reach a stable and converged initial condition of pavement temps at all depths 
        for(z in 1:1){
          
          if(z%%2 == 0){cat("z =", z, "|", "Surface Temp:", signif(pave.time[time.s == t.step[p] & node == 0, T.K - 273.15], 3),"C \n")}
          
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
          pave.time[time.s == t.step[p+1] & node == 0, T.K := tmp[node == 0, T.K] -  # T.K at node 0 is pavement surface temp (at time p+1)
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
          
          pave.time[time.s == t.step[p+1] & node != 0, T.K := 
                      tmp[node != 0, T.C.i]]
          
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
          
          # break before 200 iterations if the first and second timestep pavement temperatures at each node have converged (within 1/10 deg K of each other) 
          # after 20 iterations, relax converge constraint to 1 degree K
          tol <- ifelse(z < 100, 1, 0)
          # as the initial (p) and p+1 conditions won't change once first converged, this loop breaks always after 1 iteration for all times after p+1
          if(all(is.finite(pave.time[time.s == t.step[2], T.K])) & # all pave temps at p==2 also need to be finite (b/c below check doesn't work for all NAs)
             all(round(pave.time[time.s == t.step[1], T.K], tol) == round(pave.time[time.s == t.step[2], T.K], tol), na.rm = T) == T){break}
          
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
        saveRDS(pave.time, paste0(out.folder,"/run_",run,"_output.rds")) # save model iteration R object
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

# export data (also in RDS format to prevent issues with formatting)
write.csv(model.runs, paste0(out.folder,"/model_runs_metadata.csv"), row.names = F) # output model run metadata
saveRDS(model.runs, paste0(out.folder,"/model_runs_metadata.rds"))
saveRDS(layer.profiles, paste0(out.folder,"/layer_profiles.rds"))

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
