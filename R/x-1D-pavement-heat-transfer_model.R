# 1D model based on fundamental energy balance to calculate the pavement near-surface temperatures
# based on model outlined in Gui et al. (2007) [1]

# ** It is recommended to use Microsoft Open R (v3.5.1) for improved performance without compromsing compatibility **

# clear space and allocate memory
gc()
memory.limit(size = 56000) 
script.start <- Sys.time() # start script timestamp

# in case we need to revert or packages are missing
#if (!require("here")){install.packages("here")}
#if (!require("checkpoint")){install.packages("checkpoint")}

# load checkpoint package to insure you call local package dependcies
library(checkpoint)
#checkpoint("2019-01-17") # static checkpoint

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here::here(), # calls here package
           verbose = T) 
#checkpointRemove(Sys.Date() - 1, allUntilSnapshot = TRUE, here::here()) # this removes all previous checkpoints before today

# load dependant packages
library(zoo)
library(lubridate)
library(data.table)
library(here)


## SPECIFY MODEL RUN SCENARIOS (use for evaluating sensitivity or mulitple types of pavement scenarios)

# create list of models to run with varied inputs to check sentivity/error
# first values (will be first scenario in list of model runs) is the replication from Gui et al. [1] 
# this is assumed to be the model run we check error against to compare model preformance
models <- list(run.n = c(0), # dummy run number (replace below)
               nodal.spacing = c(12.5),# nodal spacing in millimeters
               n.iterations = c(5), # number of iterations to repeat each model run; 1,2,5,10,25
               i.top.temp = c(40,30), # starting top boundary layer temperature in deg C
               i.bot.temp = c(30,40), #     starting bottom boundary layer temperature in deg C
               time.step = c(120), # time step in seconds
               pave.length = c(5), # characteristic length of pavement in meters
               pave.depth = c(3.048,0.610),#  , 3.048 pavement depth (after which it is soil/ground)
               run.time = c(0), # initialize model run time (store at end of run)
               RMSE = c(0), # initialize model root mean square error (store at end of run)
               CFL_fail = c(0) # initialize CFL condition fail fraction of obs 
)

model.runs <- as.data.table(expand.grid(models)) # create all combinations of the above varied inputs
model.runs$run.n <- seq(from = 1, to = model.runs[,.N], by = 1)
model.runs <- model.runs[i.top.temp == i.bot.temp,] # keep scenarios where the top starting temp is higher than bot temp
model.runs[,.N] # total runs

# read in sample weather data 
#weather <- readRDS(here("data/outputs/temp/sample-weather-data.rds")) # 3 day period of weather data at 15 min
weather <- readRDS(here("data/outputs/temp/2017-weather-data.rds"))
weather <- weather[station.name == "City of Glendale" & source == "MCFCD" & month == "Jun" & # choose station for desired period of time
                   !is.na(solar) & !is.na(temp.c) & !is.na(dewpt.c) & !is.na(windir),] # make sure only to select obs with no NA of desired vars
#weather <- weather[date.time <= (min(date.time) + days(12))] # trim to 12 days long

# read in reference pavement model run if repeating runs
#pave.time.ref <- readRDS(here(paste0("data/outputs/1D-heat-model-runs/20190121/run_1_output.rds")))

# BEGIN MODEL
for(run in 1:model.runs[,.N]){ #      nrow(model.runs)  
  tryCatch({  # catch and print errors, avoids stopping model run 
    
    # first clear up space for new run
    rm(list=setdiff(ls(), c("run","model.runs","script.start","pave.time.ref","weather")))
    gc()
    t.start <- Sys.time() # start model run timestamp
    
    # input pavement layer thickness data
    x <- model.runs$pave.depth[run]  #  ground depth (m);  GUI ET AL: 3.048m
    delta.x <- model.runs$nodal.spacing[run]/1000 # chosen nodal spacing within pavement (m). default is 12.7mm (0.5in)
    
    thickness <- c("surface" = 0.1, "base" = 0.1, "subgrade" = 0) # 0.1m surface thickness, 0.1m base thickness
    thickness[3] <- x - thickness[1] - thickness[2]  
    thickness.df <- as.data.table(data.frame("layer" = names(thickness), 
                                             "u.b" = c(0, thickness[1],  thickness[1] + thickness[2]),
                                             "l.b" = c(0 + thickness[1], thickness[1] + thickness[2], thickness[1] + thickness[2] + thickness[3])))
    
    # create matrix of pavement temperatures at depth (x) with layer id (layer), start w/ only temp at time step zero (t0)
    p.data <- as.data.table(data.frame("x" = seq(0, x, delta.x),
                                       "layer" = "surface"))
    
    # name layers appropriately
    for(l in names(thickness)){
      p.data[x <= thickness.df[layer == l, l.b] & x >= thickness.df[layer == l, u.b], layer := l]
    }
    
    # insert layer transition nodes by splitting pavement matrix by layer, name these transitions layer == "boundary"
    p.data[,layer := factor(layer, levels = c("surface","base","subgrade"))]
    s <- split(p.data, f = p.data$layer)
    p.data <- rbind(s[[1]], # surface layer; surface node to first boundary/interface node
                    s[[1]][.N], # boundary/interface node
                    s[[2]], # base layer; first boundary/interface node to second boundary/interface node
                    s[[2]][.N],
                    s[[3]]) # subgrade layer; second boundary/interface node to third boundary/interface node
    p.data[s[[1]][,.N]+1, `:=`(x = thickness.df$l.b[1], layer = "boundary")] # rename first boundary/interface node
    p.data[s[[1]][,.N]+1+s[[2]][,.N]+1, `:=`(x = thickness.df$l.b[2], layer = "boundary")] # rename second boundary/interface node
    p.data[1,2] <- "boundary" # make surface node a boundary too
    
    # create timestep columns (in this case, column of pavement temps by depth calculated every 2 mins for 24 hours)
    # start from time 0 to max time which is absolute time difference in weather data 
    # add + 1 timestep at end to calculated timestep - 1 in model which is last weather obs & delete timestep n (uncalc'd) at end
    by.seq <- model.runs$time.step[run] # store time step sequence every 2 mins (in units of seconds)
    t.step <- seq(from = 0, # from time zero
                  to = as.numeric(difftime(max(weather$date.time), min(weather$date.time), units = "secs")) + by.seq, 
                  by = by.seq) 
    p.n <- length(t.step) # store final time step n
    delta.t <- t.step[2] - t.step[1] # change in time step (constant)
    
    # intitalize data.table for store model output data of pavement heat transfer time series modeling
    pave.time <- as.data.table(data.frame("time.s" = rep(t.step, each = nrow(p.data)),
                                          "node" = rep(0:(nrow(p.data)-1), p.n),
                                          "depth.m" = rep(p.data$x, p.n),
                                          "layer" = rep(p.data$layer, p.n),
                                          "T.K" = rep(seq(from = model.runs$i.top.temp[run]+273.15, # initial surface temp in K
                                                          to = model.runs$i.bot.temp[run]+273.15, # initial bottom boundary temp in K
                                                          length.out = p.n)))) # surface temp in K from the pavement to 3m)
    
    # static parameters for pavement heat transfer
    alpha <- 4.0  # thermal diffusivity (m^2/s), typically range from 2 to 12; [1]
    albedo <- 0.17 #  albedo (dimensionless) [1]; can be: 1 - epsilon for opaque objects
    epsilon <- 0.8 #  emissivity (dimensionless) [1]; can be: 1 - albedo for opaque objects
    sigma <- 5.67*10^(-8) #  Stefan-Boltzmann constant (W/(m2*K4); [1]
    SVF <- 1	# sky view factor (dimensionless [0,1]). assume 1.0: pavement is completely visible to sky
    Pr.inf <- 0.7085 #  Prandtl number (dimensionless) [0.708, 0.719] (50 to -50 degC range)
    L <- model.runs$pave.length[run] #  characteristic length of pavement (m);
    
    # parameters that vary by pavement layer
    k	<- c("surface" = 1.21, "base" = 1.21, "subgrade" = 1.00) #  thermal conductivity (W/(m2*degK))
    rho <- c("surface" = 2238, "base" = 2238, "subgrade" = 1500) #  pavement density (kg/m3);
    c	<- c("surface" = 921, "base" = 921, "subgrade" = 1900) # specific heat (J/(kg*degK);
    rho.c <- rho * c #  volumetric heat capacity (J/(m3 degK)); [1] concrete: ~2.07E6; ashpalt ~1.42E6 [6]
    
    # calculate parameters that vary by time/weather
    weather$time.s <- as.numeric(difftime(weather$date.time, weather$date.time[1], units = "secs")) # time.s to match with iterations in weather data (assuming first obs is time zero)
    weather$winspd <- weather$winspd * 0.44704 # convert to m/s from mi/h
    weather$T.inf <- weather$temp.c + 273.15 #  atmospheric dry-bulb temperature (K);
    weather$T.sky <- weather$T.inf * (((0.004 * weather$dewpt.c) + 0.8)^0.25) # sky temperature (K), caluclated using equation (2)
    weather$v.inf <- (16.92E-6) * ((weather$temp.c / 40)^0.7) # emperical fit for kinematic viscosity of air using refrence of 30 deg C where v.inf is 15.98E-6 m2/s  [4],[5]
    weather$k.inf <- ((1.5207E-11) * (weather$T.inf^3)) - ((4.8574E-08) * (weather$T.inf^2)) + ((1.0184E-04) * (weather$T.inf)) - 3.9333E-04 # Ref [2] # thermal conductivity of air in W/(m2*degK)
    weather$h.inf <- 0.664 * (weather$k.inf * (Pr.inf ^ 0.3) * (weather$v.inf ^ -0.5) * (L ^ -0.5) * (weather$winspd ^ 0.5)) # convective heat transfer coefficient in W/(m2*degK)
    
    
    ## MODEL HEAT TRANSFER OF PAVEMENT
    
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
    
    # calcluate the upper and lower layer thermal conductivity (used for boundary conditions)
    # and change in x from the upper and lower node (e.g. x.up is the change in x btwn the current node and the node right above)
    # and remerge to main data
    up.dn <- pave.time[time.s == 0]
    up.dn[, k.up := k[shift(layer, type = "lag")]]
    up.dn[, k.dn := k[shift(layer, type = "lead")]]
    up.dn[, x.up := abs(shift(depth.m, type = "lag") - depth.m)]
    up.dn[, x.dn := abs(shift(depth.m, type = "lead") - depth.m)]
    pave.time <- merge(pave.time, up.dn[, .(node, k.up, k.dn, x.up, x.dn)], by = "node", all.x = T)
    
    # also create/store a few other things
    pave.time[, k.m := k[layer]] # thermal conductivity by layer 
    pave.time[, rho.c.m := rho.c[layer]] # voumetric heat capacity
    pave.time[, date.time := weather$date.time[1] + seconds(time.s)] # add date.time column
    pave.time[, T.degC := T.K - 273.15] # create temp in deg C from Kelvin
    T.s <- vector(mode = "numeric", length = p.n) # pavement suface temp in K
    
    # iterate through from time p to time p.n and model pavement heat transfer at surface, boundary/interface, and interior nodes 
    iterations <- 1:model.runs$n.iterations[run] # 10 iteration cycles in [1] was found to produce the most accurate results
    for(iteration in iterations){
      for(p in 1:(p.n-1)){ # state at time p is used to model time p+1, so stop and p-1 to get final model output at time p
        
        # for timestep p, store the pavement surface temperature at timestep p (in K)
        T.s[p] <- pave.time[time.s == t.step[p] & node == 0, T.K]
        
        # for timestep p, calc the radiative coefficient and outgoing infared radiation based on parameters at timestep p
        pave.time[time.s == t.step[p], h.rad := SVF * epsilon * sigma * ((T.s[p]^2) + (T.sky^2)) * (T.s[p] + T.sky)] # radiative heat transfer coefficient in W/(m2*degK) 
        pave.time[time.s == t.step[p], q.rad := SVF * epsilon * sigma * ((T.s[p]^4) - (T.sky^4))] # outgoing infared radiation W/m2
        pave.time[time.s == t.step[p], q.cnv := h.inf * (T.s[p] - T.inf)] # outgoing infared radiation W/m2
        
        # store a temporary data.table at this timestep to store and transfer all node calculations to master data.table .
        # we do this because the 'shift' function does not work as needed within data.table subsets, 
        # i.e. it does not refrence the immediately above or below node/timestep when subsetting in data.table.
        # this way 'shift' works as expected because the subset data.table is only the relevant ordered data for the current timestep.
        tmp <- pave.time[time.s == t.step[p]] 
        
        # for timestep p+1, calc the surface pavement temperature based on parameters at timestep p
        # i.e. surface node s (node = 0), eqn 10 in [1]
        
        pave.time[time.s == t.step[p+1] & node == 0, T.K := T.s[p] +  # T.K at node 0 is pavement surface temp (at time p+1)
                    ((((1 - albedo) * tmp[node == 0, solar]) # incoming solar radiation (after albedo reflection)
                      - (tmp[node == 0, q.cnv])  # minus convective heat transfer at surface
                      - (tmp[node == 0, q.rad])  # minus outgoing longwave/infrared radiation at surface
                      + (k["surface"] * (tmp[node == 1, T.K] - T.s[p]) / delta.x)) # plus conduction
                     * (2 * delta.t / (rho.c["surface"] * delta.x)))] # all multiplied by the proportional change in temp per change in depth 
        
        # calculate boundary/interface nodes at timestep p+1
        tmp[, T.C.b :=  
              (((k.up / k.dn) * (x.dn / x.up) * tmp[, shift(T.K, type = "lag")]) 
               + tmp[, shift(T.K, type = "lead")])
            / (1 + ((k.up / k.dn) * (x.dn / x.up)))]
        
        # store only the boundary temps at time p+1 from boundary calc in output pave.time data
        pave.time[time.s == t.step[p+1] & node != 0 & layer == "boundary", T.K := tmp[node != 0 & layer == "boundary", T.C.b]] 
        
        # calculate interior nodes at timestep p+1 (non-boundary/non-interface nodes)
        tmp[, T.C.i := 
              ((k.m * delta.t / (rho.c.m * (delta.x^2))) 
               * (tmp[, shift(T.K, type = "lag")]  # T at node m-1 and p
                  + tmp[, shift(T.K, type = "lead")] # T at node m+1 and p
                  - 2 * tmp[, T.K])) + tmp[, T.K]] # T at node m and p
        
        tmp[, h.flux := # heat flux in w/m2
              ((2 * k.m / delta.x) * (tmp[, shift(T.K, type = "lag")]  # T at node m-1 and p
                     + tmp[, shift(T.K, type = "lead")] # T at node m+1 and p
                     - 2 * tmp[, T.K])) ]
        
        # store only the interior node temps at time p+1 from interior calc in output pave.time data
        pave.time[time.s == t.step[p+1] & node != 0 & node != max(node) & layer != "boundary", T.K := tmp[node != 0 & node != max(node) & layer != "boundary", T.C.i]]
        pave.time[time.s == t.step[p+1] & node != 0 & node != max(node) & layer != "boundary", heat.flux := tmp[node != 0 & node != max(node) & layer != "boundary", h.flux]]
        
      } # end time step p, go to time p+1
      
      # if the iteration is not the last iteration, then
      # store the final time step pavement temps as the initial temps for new run
      if(iteration != max(iterations)){
        pave.time[time.s == 0, T.K := pave.time[time.s == max(time.s), T.K]]
      }
      
    } # end model iteration and continue to next 
    
    # once model iteration are complete, some housekeeping:
    pave.time[, T.degC := T.K - 273.15] # create temp in deg C from Kelvin
    pave.time <- pave.time[time.s != t.step[p.n]] # remove the last time step (not calculated). timesteps were given such that p.n-1 is the final weather obs
    pave.time[, delta.T.mean := T.degC - mean(T.degC, na.rm = T), by = node] # Temp at time and node difference from 3 day mean, use for RMSE
    saveRDS(pave.time, here(paste0("data/outputs/1D-heat-model-runs/run_",run,"_output.rds"))) # save model iteration R object
    model.runs$run.time[run] <- as.numeric(round(difftime(Sys.time(),t.start, units = "mins"),2)) # store runtime of model iteration in minutes
    
    # if the run is the first run (defualt to validate other runs against), store it as the reference run
    if(run == 1){
      pave.time.ref <- pave.time
    }
    
    # if run isn't first run, then store RSME compared to run 1 (validated run for comparison)
    if(run != 1){ # if the model isn't the refrence run
      model.runs$RMSE[run] <- sqrt(mean((pave.time[pave.time.ref, .(time.s,depth.m,delta.T.mean), on = c("time.s","depth.m")][!is.na(delta.T.mean), delta.T.mean] - 
                                           pave.time.ref[pave.time, .(time.s,depth.m,delta.T.mean), on = c("time.s","depth.m")][!is.na(delta.T.mean), delta.T.mean])^2))
    } # note that RMSE is mean centered temperature across all nodes and timesteps.
    
    # check finite difference solving method satisfied the Courant-Friedrichs-Lewy (CFL) condition for stability
    # and store if all instances are satisfied in the model.runs metadata
    # in other words, this checks that delta.t and delta.x are sufficently small such that the solving method is stable/reliable
    
    # store variables as vector
    h.rad <- pave.time[, h.rad] # radiative heat transfer coefficient in W/(m2*degK) 
    h.inf <- pave.time[, h.inf] # convective heat transfer coefficient in W/(m2*degK)
    
    # first create empty vectors to store of min allowed change in time (RHS of eqn 13 from [1])
    t.delta.min.L1 <- vector(mode = "numeric", length = length(h.rad))
    t.delta.min.L2 <- vector(mode = "numeric", length = length(h.rad))
    t.delta.min.L3 <- vector(mode = "numeric", length = length(h.rad))
    
    # calc values for CFL check for each timestep (RHS eqn. 13 in [1])
    for(a in 1:length(h.rad)){
      t.delta.min.L1[a] <- (rho.c["surface"] * (delta.x^2)) / (2 * (h.rad[a] * delta.x + h.inf[a] * delta.x + k["surface"])) # in seconds
      t.delta.min.L2[a] <-  (rho.c["base"] * (delta.x^2)) / (2 * (h.rad[a] * delta.x + h.inf[a] * delta.x + k["base"])) # in seconds
      t.delta.min.L3[a] <- (rho.c["subgrade"] * (delta.x^2)) / (2 * (h.rad[a] * delta.x + h.inf[a] * delta.x + k["subgrade"])) # in seconds
    }
    
    # delta.t must be less than or equal to the t.delta.min values to satisfy CFL condition
    # so it is not if it is greater than the minimum observed t.delta.min values
    # if so calculate the fraction of non-satifiying instances
    if(delta.t > min(pmin(t.delta.min.L1, t.delta.min.L2, t.delta.min.L3))){
      model.runs$CFL_fail[run] <- round(sum(ifelse(delta.t <= t.delta.min.L1, 1, 0), 
                                            ifelse(delta.t <= t.delta.min.L2, 1, 0), 
                                            ifelse(delta.t <= t.delta.min.L3, 1, 0)) / sum(length(t.delta.min.L1),
                                                                                           length(t.delta.min.L1),
                                                                                           length(t.delta.min.L3)), 2)
    } # end CFL condition checking
    
    }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")}) # print error message if model run had error
} # end run, go to next run

write.csv(model.runs, here("data/outputs/1D-heat-model-runs/model_runs_metadata.csv"), row.names = F) # output model run metadata
paste0("Completed model run at ", Sys.time(), ". Model run took ", round(difftime(Sys.time(),script.start, units = "mins"),0)," minutes to complete.") # paste total script time


##################
### References ###
##################

# [1] https://doi.org/10.1061/(ASCE)0899-1561(2007)19:8(683)

# [2] http://bouteloup.pierre.free.fr/lica/phythe/don/air/air_k_plot.pdf

# [3] https://www.maricopa.gov/3769/Weather-Sensor-Data

# [4] https://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html

# [5] http://www-mdp.eng.cam.ac.uk/web/library/enginfo/aerothermal_dvd_only/aero/fprops/propsoffluids/node5.html

# [6] https://doi.org/10.1016/1352-2310(94)00140-5
