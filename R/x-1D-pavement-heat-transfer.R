# 1D model based on fundamental energy balance to calculate the pavement near-surface temperatures
# based on model outlined in Gui et al. (2007) [1]

# clear space and allocate memory
gc()
memory.limit(size = 56000) 
t.start <- Sys.time() # start script timestamp

# list of all dependant packages
list.of.packages <- c("tidyverse",
                      "zoo",
                      "lubridate",
                      "data.table",
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
invisible(lapply(list.of.packages, library, character.only = T)) # invisible() just hides printing stuff in console

# load windows fonts and store as string
windowsFonts(Century=windowsFont("TT Century Gothic"))
windowsFonts(Times=windowsFont("TT Times New Roman"))
my.font <- "Century"

## MODEL RUN SPECFICATIONS

# create list of models to run with varied inputs to check sentivity/error
# first values (will be first scenario in list of model runs) is the replication from Gui et al. [1] 
# this is assumed to be the model run we check error against to compare model preformance
models <- list(nodal.spacing = c(12.5,25),# nodal spacing in millimeters
               n.iterations = c(10,30), # number of iterations to repeat each model run for
               i.top.temp = c(40), # starting top boundary layer temperature in deg C
               i.bot.temp = c(30), # starting bottom boundary layer temperature in deg C
               time.step = c(120,60,30), # time step in seconds
               pave.length = c(5,40), # characteristic length of pavement in meters
               run.time = c(0), # initialize model run time (store at end of run)
               RMSE = c(0) # initialize model root mean square error (store at end of run)
)

model.runs <- expand.grid(models) # create all combinations of the above varied inputs
nrow(model.runs) # total runs

for(run in 1:nrow(model.runs) ){ #
  tryCatch({  # catch and print errors, avoids stopping model run
    
    t.start <- Sys.time() # start model run timestamp
    # first clear up space for new run
    rm(list=setdiff(ls(), c("run","model.runs","t.start","my.font")))
    gc()
    
    # read in sample weather data 
    weather <- readRDS(here("data/outputs/temp/sample-weather-data.rds"))
    
    ## PAVEMENT SPECIFICATIONS [1]
    
    # HMA:
    # surface: 0.0625 - 0.123 m 
    # base: 0.05 - 0.150 m
    # subgrade: soil
    
    # PCC:
    # surface: 0.0625 - 0.123 m 
    # base: 0.075 - 0.350 m
    # subgrade: soil
    
    # input pavement layer thickness data
    x <- 3.048 #  ground depth (m);
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
    p.data <- rbind(s[[1]],
                    s[[1]][.N],
                    s[[2]],
                    s[[2]][.N],
                    s[[3]])
    p.data[s[[1]][,.N]+1, `:=`(x = thickness.df$l.b[1], layer = "boundary")]
    p.data[s[[1]][,.N]+1+s[[2]][,.N]+1, `:=`(x = thickness.df$l.b[2], layer = "boundary")]
    p.data[1,2] <- "boundary" # make surface a boudnary too
    
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
    albedo <- 0.17 #  albedo (dimensionless); [1]
    epsilon <- 0.8 #  emissivity (dimensionless); [1]
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
        
        # store only the interior node temps at time p+1 from interior calc in output pave.time data
        pave.time[time.s == t.step[p+1] & node != 0 & node != max(node) & layer != "boundary", T.K := tmp[node != 0 & node != max(node) & layer != "boundary", T.C.i]]
        
      }
      # store the final time step pavement temps as the initial temps for new run
      pave.time[time.s == 0, T.K := pave.time[time.s == max(time.s), T.K]]
    }
    
    pave.time[, T.degC := T.K - 273.15] # create temp in deg C from Kelvin
    pave.time <- pave.time[time.s != t.step[p.n]] # remove the last time step (not calculated). timesteps were given such that p.n-1 is the final weather obs
    
    
    ## replicate Gui et al. Fig. 2 
  
    # depth temp
    my.node <- 1
    p1.data <- pave.time[node == my.node & time.s <= t.step[p]]
    min.x <- min(p1.data$date.time, na.rm = T)
    max.x <- ceiling_date(max(weather$date.time, na.rm = T), unit = "hours")
    min.y <- 20 #signif(min(p1.data[, T.degC]) - (0.1 * diff(range(p1.data[, T.degC]))),4)
    max.y <- 80 #ceiling(signif(max(p1.data[, T.degC]) + (0.1 * diff(range(p1.data[, T.degC]))),4))
    
    p1 <- (ggplot(data = p1.data) # weather
           + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
           + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
           + geom_point(aes(y = T.degC, x = date.time)) #date.time
           + geom_point(aes(y = T.degC, x = date.time), data = p1.data[date.time %in% weather$date.time], color = "red")
           #+ geom_point(aes(y = T.sky - 273.15, x = time.s), data = weather, color = "blue") # points of sky temp
           #+ geom_line(aes(y = T.degC, x = time.s), color = "grey50", linetype = 2, size = 0.75) # line of temp
           + scale_x_datetime(expand = c(0,0), limits = c(min.x,max.x), date_breaks = "6 hours")
           + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,5))
           + ggtitle(paste0("Modeled ", p1.data[, depth.m][1]*1000,"mm Pavement Temperature"))
           + labs(x = "Time of Day", y = "Temperature (deg C)") # "Time of Day"
           + theme_minimal()
           + theme(text = element_text(family = my.font, size = 12),
                   plot.margin = margin(t = 10, r = 20, b = 10, l = 40, unit = "pt"),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                   axis.title.x = element_text(vjust = 0.3),
                   plot.title = element_text(hjust = 0.75)))
    
    # save model run output
    ggsave(paste0("run_",run,"_1D-modeled-", round(p1.data[, depth.m][1]*1000,0),"mm-pave-temp.png"), p1, 
           device = "png", path = here("figures/1D-heat-model-runs"), scale = 1, width = 6.5, height = 5, dpi = 300, units = "in")
    
    saveRDS(pave.time, here(paste0("data/outputs/1D-heat-model-runs/run_",run,"_output.rds"))) # save model iteration R object
    model.runs$run.time[run] <- as.numeric(round(difftime(Sys.time(),t.start, units = "mins"),1)) # store runtime of model iteration in minutes
    
    # if run isn't first run, then store RSME compared to run 1 (validated run for comparison)
    if(run != 1){
      pave.time.ref <- readRDS(here("data/outputs/1D-heat-model-runs/run_1_output.rds"))
      model.runs$RMSE[run] <- sqrt(mean((pave.time[pave.time.ref, .(time.s,depth.m,T.degC), on = c("time.s","depth.m")][!is.na(T.degC), T.degC] - 
                                           pave.time.ref[pave.time, .(time.s,depth.m,T.degC), on = c("time.s","depth.m")][!is.na(T.degC), T.degC])^2))
    }
    
    
    }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")}) # print error message if run had error
} # end run, go to next run




p0.data <- pave.time[node == 0 & time.s <= t.step[p]]
min.x <- min(p0.data$date.time, na.rm = T)
max.x <- ceiling_date(max(weather$date.time, na.rm = T), unit = "hours")
min.y <- signif(min(p0.data[, T.degC], na.rm = T) - (0.1 * diff(range(p0.data[, T.degC], na.rm = T))),4)
max.y <- ceiling(signif(max(p0.data[, T.degC], na.rm = T) + (0.1 * diff(range(p0.data[, T.degC], na.rm = T))),4))

# for including air temps, instead use:
#min.y <- pmin(signif(min(p0.data[, T.degC], na.rm = T) - (0.1 * diff(range(p0.data[, T.degC], na.rm = T))),4), min(weather$temp.c), na.rm = T) 
#max.y <- signif(max(p0.data[, T.degC], na.rm = T) + (0.1 * diff(range(p0.data[, T.degC], na.rm = T))),4) 

p0 <- (ggplot(data = p0.data) 
       + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border: (x,y) (xend,yend)
       + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border: (x,y) (xend,yend)
       + geom_point(aes(y = T.degC, x = date.time))
       + geom_point(aes(y = T.degC, x = date.time), data = p0.data[date.time %in% weather$date.time], color = "red")
       #+ geom_point(aes(y = temp.c, x = date.time), data = weather, color = "blue") # for inlcuding air temp on plot
       + scale_x_datetime(expand = c(0,0), limits = c(min.x,max.x), date_breaks = "6 hours")
       + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y))
       + ggtitle("Modeled Surface Pavement Temperature")
       + labs(x = "Time of Day", y = "Temperature (deg C)") # "Time of Day"
       + theme_minimal()
       + theme(text = element_text(family = my.font, size = 12),
               plot.margin = margin(t = 10, r = 20, b = 10, l = 40, unit = "pt"),
               axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
               axis.title.x = element_text(vjust = 0.3),
               plot.title = element_text(hjust = 0.75)))

p0
ggsave("1D-modeled-surface-pave-temp_new.png", p0, 
       device = "png", path = here("figures"), scale = 1, width = 6.5, height = 5, dpi = 300, units = "in")







# depth temps
min.x <- signif(min(pave.time[, T.degC]) - (0.1 * diff(range(pave.time[, T.degC]))),4)
max.x <- signif(max(pave.time[, T.degC]) + (0.1 * diff(range(pave.time[, T.degC]))),4)
min.y <- 0 # 0 depth
max.y <- max(pave.time[, depth.m])

my.date <- ymd(pave.time$date.time[1])
p.depth <- (ggplot(data = pave.time) # weather
       + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
       + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
       + geom_line(aes(y = depth.m, x = T.degC, color = "1pm"), data = pave.time[date.time == paste(my.date,"13:00:00")], size = 1.5) # 1pm temperatures at depth
       + geom_line(aes(y = depth.m, x = T.degC, color = "5pm"), data = pave.time[date.time == paste(my.date, "17:00:00")], size = 1.5) # 5pm temperatures at depth
       + geom_line(aes(y = depth.m, x = T.degC, color = "9pm"), data = pave.time[date.time == paste(my.date, "21:00:00")], size = 1.5) # 9pm temperatures at depth
       #+ geom_point(aes(y = depth.m, x = T.degC), data = pave.time[date.time == paste(my.date,"13:00:00")], name = "1pm", shape = 1) # 1pm temperatures at depth
       #+ geom_point(aes(y = depth.m, x = T.degC), data = pave.time[date.time == paste(my.date, "17:00:00")], name = "5pm", shape = 2) # 5pm temperatures at depth
       #+ geom_point(aes(y = depth.m, x = T.degC), data = pave.time[date.time == paste(my.date, "21:00:00")], name = "9pm", shape = 3) # 9pm temperatures at depth
       #+ geom_point(aes(y = T.sky - 273.15, x = time.s), data = weather, color = "blue") # sky temp
       #+ geom_line(aes(y = T.degC, x = time.s), color = "grey50", linetype = 2, size = 0.75) #date.time
       #+ scale_x_continuous(expand = c(0,0), limits = c(min.x,max.x))
       + scale_x_continuous(expand = c(0,0), limits = c(min.x,max.x), position = "top")
       + scale_y_reverse(expand = c(0,0), limits = c(max.y,min.y))
       + ggtitle(paste0("Modeled Pavement Temperature at Depth"))
       + labs(x = "Temperature (deg C)", y = "Depth (m)") # "Time of Day"
       + theme_minimal()
       + theme(text = element_text(family = my.font, size = 12),
               plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"),
               axis.title.x = element_text(vjust = 0.3)))
#p.depth

t.end <- Sys.time() 
paste0("Completed model run at ", t.end, ". Model run took ", round(difftime(t.end,t.start, units = "mins"),1)," minutes to complete.") # paste total script time


## SAVES
# save plots
#ggsave(paste0("1D-modeled-pave-temp-at-depth_new.png"), p.depth, 
#       device = "png", path = here("figures"), scale = 1, width = 6.5, height = 5, dpi = 300, units = "in")

# save workspace
save.image(here("data/outputs/1D-pave-heat-model-out.RData"))


##################
## check CFL boundary condition

# first create empty vectors to store of min allowed change in time (RHS of eqn 13 from [1])
t.delta.min.L1 <- vector(mode = "numeric", length = length(h.rad))
t.delta.min.L2 <- vector(mode = "numeric", length = length(h.rad))
t.delta.min.L3 <- vector(mode = "numeric", length = length(h.rad))

# calc values for CFL check for each timestep
for(a in 1:length(h.rad)){
  
  # check the time steps obey the CFL condition for stability
  # (each timestep must staisfy the explicit finite difference condition for stability)
  t.delta.min.L1[a] <- (rho.c["surface"] * (delta.x^2)) / (2 * (h.rad[a] * delta.x + h.inf[a] * delta.x + k["surface"])) # in seconds
  t.delta.min.L2[a] <-  (rho.c["base"] * (delta.x^2)) / (2 * (h.rad[a] * delta.x + h.inf[a] * delta.x + k["base"])) # in seconds
  t.delta.min.L3[a] <- (rho.c["subgrade"] * (delta.x^2)) / (2 * (h.rad[a] * delta.x + h.inf[a] * delta.x + k["subgrade"])) # in seconds
}

# check if CFL condition met for the chosen delta.t, if so delete the cols for checking
if(delta.t <= min(pmin(t.delta.min.L1, t.delta.min.L2, t.delta.min.L3))){
  print("CFL condition met for all timesteps")
} else {print(paste0("CFL condition *NOT* met for all timesteps. ", 100*round(sum(ifelse(delta.t <= t.delta.min.L1, 1, 0), 
                                                                             ifelse(delta.t <= t.delta.min.L2, 1, 0), 
                                                                             ifelse(delta.t <= t.delta.min.L3, 1, 0)) 
                                                                         / sum(length(t.delta.min.L1),
                                                                               length(t.delta.min.L1),
                                                                               length(t.delta.min.L3)), 2),
                     "% of timesteps do not obey CFL boundary condition"))}

paste0("The mean of all timesteps to satifiy the CFL stability condition is ", 
       round(mean(rowMeans(cbind(t.delta.max.L1, t.delta.max.L2, t.delta.max.L3))), 0), 
       " seconds compared to a (max) delta t of ", delta.t, " seconds.") 


                                                                                                                                            

##################
### References ###
##################

# [1] https://doi.org/10.1061/(ASCE)0899-1561(2007)19:8(683)

# [2] http://bouteloup.pierre.free.fr/lica/phythe/don/air/air_k_plot.pdf

# [3] https://www.maricopa.gov/3769/Weather-Sensor-Data

# [4] https://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html

# [5] http://www-mdp.eng.cam.ac.uk/web/library/enginfo/aerothermal_dvd_only/aero/fprops/propsoffluids/node5.html

# [6] https://doi.org/10.1016/1352-2310(94)00140-5


##########################

# for loop iterations (slower than current data.table assignment with mini-loop)
for(iteration in 1:5){
  
  for(p in 1:(p.n-1)){ # state at time p is used to model time p+1, so stop and p-1 to get final model output at time p
    
    # current pavement surface temp (in Kelvin)
    T.s[p] <- pave.time[time.s == t.step[p] & depth.m == 0, T.K]
    
    w <- which(t.step[p] == weather$time.s) # store location of timestep match (will be empty if no match)
    if(length(w) == 1){ # if timestep match is valid (there is a weather obs at this timestep in the model)
      
      # use observation values for calculations
      T.sky[p] <- weather$T.sky[w] 
      T.inf[p] <- weather$T.inf[w]
      solar[p] <- weather$solar[w]
      winspd[p] <- weather$winspd[w]
      v.inf[p] <- weather$v.inf[w]
      k.inf[p] <- weather$k.inf[w]
      h.inf[p] <- weather$h.inf[w]
      
    } else { # if there isn't a match, interpolate parameters linearlly (solar, temp)
      
      # find the closest two obsevations indices and store thier obs time
      w1 <- which.min(abs(weather$time.s - t.step[p]))
      w2 <- if(w1 == 1){2} else if(which.min(c(abs(weather$time.s[w1-1] - t.step[p]),abs(weather$time.s[w1+1] - t.step[p]))) == 1){w1-1} else {w1+1}
      
      t1 <- weather$time.s[w1]
      t2 <- weather$time.s[w2]
      
      # iterpolate btwn necessary variables
      T.sky[p] <- (weather$T.sky[w2] * (t.step[p] - t1) / (t2 - t1)) +
        (weather$T.sky[w1] * (t2 - t.step[p]) / (t2 - t1))
      
      T.inf[p] <- (weather$T.inf[w2] * (t.step[p] - t1) / (t2 - t1)) +
        (weather$T.inf[w1] * (t2 - t.step[p]) / (t2 - t1)) 
      
      solar[p] <- (weather$solar[w2] * (t.step[p] - t1) / (t2 - t1)) +
        (weather$solar[w1] * (t2 - t.step[p]) / (t2 - t1))
      
      winspd[p] <- (weather$winspd[w2] * (t.step[p] - t1) / (t2 - t1)) +
        (weather$winspd[w1] * (t2 - t.step[p]) / (t2 - t1))
    }
    
    # estimate parameters
    h.rad[p] <- SVF * epsilon * sigma * ((T.s[p]^2) + (T.sky[p]^2)) * (T.s[p] + T.sky[p]) # radiative coefficient
    q.rad[p] <- h.rad[p] * (T.s[p] - T.sky[p]) # outgoing infared radiation
    v.inf[p] <- (16.92E-6) * (((T.inf[p] - 273.15) / 40)^0.7) #  kinematic viscosity of air [4], [5]
    k.inf[p] <- ((1.5207E-11) * (T.inf[p]^3)) - ((4.8574E-08) * (T.inf[p]^2)) + ((1.0184E-04) * (T.inf[p])) - 3.9333E-04 # [2]
    h.inf[p] <- 0.664 * (k.inf[p] * (Pr.inf ^ 0.3) * (v.inf[p] ^ -0.5) * (L ^ -0.5) * (winspd[p] ^ 0.5))
    
    # SURFACE NODE at time p+1
    # solve for surface node, s (pavement surface temp at time p+1) (EQN 10)
    
    pave.time[time.s == t.step[p+1] & node == 0, T.K := T.s[p] +
                ((((1 - albedo) * solar[p])
                  + (h.inf[p] * (T.inf[p] - T.s[p]))
                  + (h.rad[p] * (T.sky[p] - T.s[p]))
                  + ((k["surface"] * (pave.time[time.s == t.step[p] & node == 1, T.K] - T.s[p]))
                     / delta.x))
                 * (2 * delta.t / (rho.c["surface"] * delta.x)))]
    
    # INTERIOR NODES m at time p+1
    # solve for interior node m at time p+1 using state at time p (EQN 9)
    for(m in 1:(max(pave.time$node)-1)){ # for nodes 1 to max(nodes)-1
      # last node isn't calc'd b/c you need +1 and -1 node for estiamte. after model, remove last 2 nodes to clean up at then bottom boundary
      
      # calculate the p+1 temps at node m based on temps at time p at nodes m, m-1, and m+1
      
      # store the change in x between m and m-1 and m and m+1 (this is different at the boundary)
      d.x.up <- pave.time[time.s == t.step[p] & node == (m), depth.m] - pave.time[time.s == t.step[p] & node == (m-1), depth.m]
      d.x.dn <- pave.time[time.s == t.step[p] & node == (m+1), depth.m] - pave.time[time.s == t.step[p] & node == (m), depth.m]
      
      # if the node is a boundary node, use boundary condition to solve for the current boundary layer temp
      if(pave.time[time.s == t.step[p] & node == m, layer] == "boundary"){
        
        # store k values at above and below the boundary (they are different)
        k.up <- k[pave.time[time.s == t.step[p] & node == (m-1), layer]]
        k.dn <- k[pave.time[time.s == t.step[p] & node == (m+1), layer]]
        
        pave.time[time.s == t.step[p+1] & node == m, T.K := #273.15 +
                    (((k.up / k.dn) * (d.x.dn / d.x.up) * pave.time[time.s == t.step[p] & node == (m-1), T.K]) 
                     + pave.time[time.s == t.step[p] & node == (m+1), T.K])
                  / (1 + ((k.up / k.dn) * (d.x.dn / d.x.up)))]
        
      } else { # otherwise calc for non-boundary
        
        # store k value at layer
        k.m <- k[pave.time[time.s == t.step[p+1] & node == (m), layer]]
        rho.c.m <- rho.c[pave.time[time.s == t.step[p+1] & node == (m), layer]]
        
        # non-interface calc for  at p+1
        pave.time[time.s == t.step[p+1] & node == m, T.K := 
                    ((k.m * delta.t / rho.c.m) * (pave.time[time.s == t.step[p] & node == (m-1), T.K]
                                                  + pave.time[time.s == t.step[p] & node == (m+1), T.K]
                                                  - 2 * pave.time[time.s == t.step[p] & node == m, T.K])) 
                  + pave.time[time.s == t.step[p] & node == m, T.K]]
      }
    }
    
    # if timestep match is valid (there is a weather obs at this timestep in the model)
    # add current timestep pavement parameters to weather data
    if(length(w) == 1){ 
      weather$q.rad[w] <- q.rad[p] # outgoing infared radiation
      weather$h.rad[w] <- h.rad[p] # radiative coefficient
      weather$T.s[w] <- T.s[p] - 273.15 # pavement surface temp degC
      weather$T.n1[w] <- pave.time[time.s == t.step[p] & node == 1, T.K] - 273.15 # first node under pavement temp degC
      weather$T.avg.layer1[w] <- mean(pave.time[time.s == t.step[p] & layer == "surface", T.K]) - 273.15 # temp avg for layer 1 degC
      weather$T.avg.layer2[w] <- mean(pave.time[time.s == t.step[p] & layer == "base", T.K]) - 273.15 # temp avg for layer 2 degC
      weather$T.avg.layer3[w] <- mean(pave.time[time.s == t.step[p] & layer == "subgrade", T.K]) - 273.15 # temp avg for layer 3 degC
      weather$T.n238[w] <- pave.time[time.s == t.step[p] & node == 238, T.K] - 273.15 # 10ft (3m) temp degC
    }
  }
  
}



### sapply for nodal caluclations (currently slower than for loop)

# last node isn't calc'd b/c you need +1 and -1 node for estiamte. after model, remove last 2 nodes to clean up at then bottom boundary
# calculate the p+1 temps at node m based on temps at time p at nodes m, m-1, and m+1

# interface/boundary condition to solve for the current boundary layer temp
#pave.time[time.s == t.step[p] & layer == "boundary" & node != 0]$T.K <- sapply(1:nrow(pave.time[time.s == t.step[p] & layer == "boundary" & node != 0]),
#                                                                               function (x) ((((k[pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]-1), layer]] 
#                                                                                                / k[pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]+1), layer]]) 
#                                                                                               * ((pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]+1), depth.m] 
#                                                                                                   - pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]), depth.m]) 
#                                                                                                  / (pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]), depth.m] 
#                                                                                                     - pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]-1), depth.m])) 
#                                                                                               * pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]-1), T.K]) 
#                                                                                              + pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]+1), T.K])
#                                                                                             / (1 + ((k[pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]-1), layer]] 
#                                                                                                      / k[pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]+1), layer]]) 
#                                                                                                     * ((pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]+1), depth.m] 
#                                                                                                         - pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]), depth.m]) 
#                                                                                                        / (pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]), depth.m] 
#                                                                                                           - pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p] & layer == "boundary" & node != 0, node][x]-1), depth.m])))))
#)

# non-interface calc for  at p+1
#pave.time[time.s == t.step[p+1] & layer != "boundary" & node != 0]$T.K <- as.numeric(sapply(1:nrow(pave.time[time.s == t.step[p+1] & layer != "boundary" & node != 0]),
#                                                                                            function (x) ((k[pave.time[time.s == t.step[p+1] & node == (pave.time[time.s == t.step[p+1] & layer != "boundary" & node != 0, node][x]), layer]] * delta.t 
#                                                                                                           / rho.c[pave.time[time.s == t.step[p+1] & node == (pave.time[time.s == t.step[p+1] & layer != "boundary" & node != 0, node][x]), layer]]) 
#                                                                                                          * (pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p+1] & layer != "boundary" & node != 0, node][x]-1), T.K]
#                                                                                                             + pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p+1] & layer != "boundary" & node != 0, node][x]+1), T.K]
#                                                                                                             - 2 * pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p+1] & layer != "boundary" & node != 0, node][x]), T.K]))
#                                                                                            + pave.time[time.s == t.step[p] & node == (pave.time[time.s == t.step[p+1] & layer != "boundary" & node != 0, node][x]), T.K])
#)
