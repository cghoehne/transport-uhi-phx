# 1D model based on fundamental energy balance to calculate the pavement near-surface temperatures
# based on model outlined in Gui et al. (2007) [1]

# clear space and allocate memory
gc()
memory.limit(size = 56000) 
t.start <- Sys.time() # start script timestamp

# list of all dependant packages
list.of.packages <- c("tidyverse",
                      "lubridate",
                      "data.table",
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
invisible(lapply(list.of.packages, library, character.only = T)) # invisible() just hides printing stuff in console


## WEATHER DATA (using data from Ref [3])

# import weather data
w.data <- readRDS(here("data/outputs/temp/2017-weather-data.rds")) # all houlry weather data
w.stations <- readRDS(here("data/outputs/temp/2017-station-data.rds")) # all station data

# choose sample weather for hot day in 2017
weather <- w.data[id == "City of Glendale" & date(date.time) == "2017-06-23" & !is.na(solar)]
weather[, c("source","id","rh","qcflag","heat.f","heat.c","month","week","day","wday","date.time.round"):=NULL]
plot(weather$date.time, weather$temp.f)

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
delta.x <- 12.7/1000 # chosen nodal spacing within pavement (m). 12.7mm (0.5in)

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
by.seq <- 2*60 # store time step sequence every 2 mins (in units of seconds)
t.step <- seq(from = 0, # from time zero
              to = as.numeric(difftime(max(weather$date.time), min(weather$date.time), units = "secs")) + by.seq, 
              by = by.seq) 
p.n <- length(t.step) # store final time step n
delta.t <- t.step[2] - t.step[1] # change in time step (constant)

# intitalize pavement time series data for modeling
pave.time <- as.data.table(data.frame("time.s" = rep(t.step, each = nrow(p.data)),
                                      "node" = rep(0:(nrow(p.data)-1), p.n),
                                      "depth.m" = rep(p.data$x, p.n),
                                      "layer" = rep(p.data$layer, p.n),
                                      "T.K" = rep(seq(from = 38.6+273.15, to = 35+273.15, length.out = p.n)))) # surface temp in K (38.6 degC, down to 35 degC at 10m)

# static parameters for pavement heat transfer
alpha <- 4.0  # thermal diffusivity (m^2/s), typically range from 2 to 12; [1]
albedo <- 0.17 #  albedo (dimensionless); [1]
epsilon <- 0.8 #  emissivity (dimensionless); [1]
sigma <- 5.67*10^(-8) #  Stefan-Boltzmann constant (W/(m2*K4); [1]
SVF <- 1	# sky view factor (dimensionless [0,1]). assume 1.0: pavement is completely visible to sky
Pr.inf <- 0.7085 #  Prandtl number (dimensionless) [0.708, 0.719] (50 to -50 degC range)
L <- 1 #  characteristic length of pavement (m);

# parameters that vary by pavement layer
k	<- c("surface" = 1.21, "base" = 1.21, "subgrade" = 1.00) #  thermal conductivity (W/(m2*degK))
rho <- c("surface" = 2238, "base" = 2238, "subgrade" = 1500) #  pavement density (kg/m3);
c	<- c("surface" = 921, "base" = 921, "subgrade" = 1900) # specific heat (J/(kg*degK);
rho.c <- rho * c #  volumetric heat capacity (J/(m3 degK)); [1]

# calculate parameters that vary by time/weather
weather$time.s <- as.numeric(difftime(weather$date.time, weather$date.time[1], units = "secs")) # time.s to match with iterations in weather data (assuming first obs is time zero)
weather$winspd <- weather$winspd * 0.44704 # convert to m/s from mi/h
weather$T.inf <- weather$temp.c + 273.15 #  atmospheric dry-bulb temperature (K);
weather$T.sky <- weather$T.inf * ((0.004 * weather$dewpt.c + 0.8)^0.25) # sky temperature (K), caluclated using equation (2)
weather$v.inf <- (16.92E-6) * ((weather$temp.c / 40)^0.7) # emperical fit for kinematic viscosity of air using refrence of 30 deg C where v.inf is 15.98E-6 m2/s  [4],[5]
weather$k.inf <- ((1.5207E-11) * (weather$T.inf^3)) - ((4.8574E-08) * (weather$T.inf^2)) + ((1.0184E-04) * (weather$T.inf)) - 3.9333E-04 # Ref [2] # thermal conductivity of air in W/(m2*degK)
weather$h.inf <- 0.664 * (weather$k.inf * (Pr.inf ^ 0.3) * (weather$v.inf ^ -0.5) * (L ^ -0.5) * (weather$winspd ^ 0.5)) # convective heat transfer coefficient in W/(m2*degK)

# predefine variables as vectors that will need to be interpolated
T.s <- vector(mode = "numeric", length = p.n) # pavement suface temp in K
T.sky <- vector(mode = "numeric", length = p.n) # sky temperature in K
T.inf <- vector(mode = "numeric", length = p.n) # atmospheric dry-bulb temperature in K
solar <- vector(mode = "numeric", length = p.n) # solar radiation in W/m2
winspd <- vector(mode = "numeric", length = p.n) # wind speed in m/s
v.inf <- vector(mode = "numeric", length = p.n) # kinematic viscosity of air in m2/s [10E-6,14E-6]
k.inf <- vector(mode = "numeric", length = p.n) # thermal conductivity of air in W/(m*degK) [0.02, 0.03] (for normal air temps)
h.inf <- vector(mode = "numeric", length = p.n) # convective heat transfer coefficient in W/(m2*degK) [0.5, 1000]
q.rad <- vector(mode = "numeric", length = p.n) # infrared radiation heat transfer W/m2
h.rad <- vector(mode = "numeric", length = p.n) # radiative heat transfer coefficient in W/(m2*degK)

## MODEL HEAT TRANSFER OF PAVEMENT
# iterate through time steps and model pavement heat transfer
for(p in 1:20){ #(p.n-1) state at time p is used to model time p+1, so stop and p-1 to get final model output at time p
  
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
    T.sky[p] <- (weather$T.sky[w1] * (t.step[p] - t1) / (t2 - t1)) +
                (weather$T.sky[w2] * (t2 - t.step[p]) / (t2 - t1))
    
    T.inf[p] <- (weather$T.inf[w1] * (t.step[p] - t1) / (t2 - t1)) +
                (weather$T.inf[w2] * (t2 - t.step[p]) / (t2 - t1)) 
    
    solar[p] <- (weather$solar[w1] * (t.step[p] - t1) / (t2 - t1)) +
                (weather$solar[w2] * (t2 - t.step[p]) / (t2 - t1))
    
    winspd[p] <- (weather$winspd[w1] * (t.step[p] - t1) / (t2 - t1)) +
                 (weather$winspd[w2] * (t2 - t.step[p]) / (t2 - t1))
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

pave.time[, T.degC := T.K - 273.15] # create temp in deg C from Kelvin
pave.time <- pave.time[node != max(pave.time$node) | node != max(pave.time$node)-1] # remove last 2 nodes to eliminate fuzziness at end nodes

# remove the last time step (not calculated) in relevant data
pave.time <- pave.time[time.s != t.step[p.n]]
T.s <- T.s[1:(p.n-1)]
T.sky <- T.sky[1:(p.n-1)]
T.inf <- T.inf[1:(p.n-1)]
solar <- solar[1:(p.n-1)]
winspd <- winspd[1:(p.n-1)]
v.inf <- v.inf[1:(p.n-1)]
k.inf <- k.inf[1:(p.n-1)]
h.inf <- h.inf[1:(p.n-1)]
q.rad <- q.rad[1:(p.n-1)]
h.rad <- h.rad[1:(p.n-1)]

## replicate Gui et al. Fig. 2 

# load windows fonts and store as string
windowsFonts(Century=windowsFont("TT Century Gothic"))
windowsFonts(Times=windowsFont("TT Times New Roman"))
my.font <- "Century"

#min.x <- weather$date.time[1]
#max.x <- weather$date.time[which(t.step[p] == weather$time.s)]
min.x <- t.step[1]
max.x <- t.step[p]
min.y <- min(pave.time[node == 0, T.degC]-1)
max.y <- max(pave.time[node == 0, T.degC]+1)

p1 <- (ggplot(data = pave.time[node == 0]) # weather
       + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
       + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
       + geom_point(aes(y = T.degC, x = time.s)) #date.time
       + geom_line(aes(y = T.degC, x = time.s), color = "grey50", linetype = 2, size = 0.75) #date.time
       + scale_x_continuous(expand = c(0,0), limits = c(min.x,max.x))
       + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y))
       + labs(x = "Time (s)", y = "Temperature (deg C)") # "Time of Day"
       + theme_minimal()
       + theme(text = element_text(family = my.font, size = 12),
               plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt")))

p1

saveRDS(pave.time, here("data/outputs/1D-pave-heat-model-out.rds"))

# print script endtime
t.end <- Sys.time() 
paste0("Completed task at ", t.end, ". Task took ", round(difftime(t.end,t.start, units = "mins"),1)," minutes to complete.") # paste total script time
# end


##################

# plot interpolated sky temperatures
temp.data <- cbind(pave.time[node == 0], T.sky)
temp.data[,T.sky := V2 - 273.15]

min.x <- t.step[1]
max.x <- t.step[p]
min.y <- min(temp.data[,T.sky] - 1)
max.y <- max(temp.data[,T.sky] + 1)

p2 <- (ggplot(data = temp.data) # weather
       + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
       + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
       + geom_point(aes(y = T.sky, x = time.s)) #date.time
       + geom_line(aes(y = T.sky, x = time.s), color = "grey50", linetype = 2, size = 0.75) #date.time
       + scale_x_continuous(expand = c(0,0), limits = c(min.x,max.x))
       + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y))
       + labs(x = "Time (s)", y = "Temperature (deg C)") # "Time of Day"
       + theme_minimal()
       + theme(text = element_text(family = my.font, size = 12),
               plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt")))

p2


# check CFL boundary condition
for(a in 1:nrow(weather)){
  
  # check the time steps obey the CFL condition for stability
  # (each timestep must staisfy the explicit finite difference condition for stability)
  weather$t.delta.max.L1[a] <- (rho.c * (delta.x^2)) / (2 * (weather$h.rad[a] * delta.x + weather$h.inf[a] * delta.x + k["surface"])) # in seconds
  weather$t.delta.max.L2[a] <-  (rho.c * (delta.x^2)) / (2 * (weather$h.rad[a] * delta.x + weather$h.inf[a] * delta.x + k["base"])) # in seconds
  weather$t.delta.max.L3[a] <- (rho.c * (delta.x^2)) / (2 * (weather$h.rad[a] * delta.x + weather$h.inf[a] * delta.x + k["subgrade"])) # in seconds
  
}

# check if CFL condition met for the chosen delta.t, if so delete the cols for checking
if(abs(t.step[1] - t.step[2]) <= min(c(weather$t.delta.max.L1, weather$t.delta.max.L2, weather$t.delta.max.L3))){
  weather$t.delta.max.L1 <- NULL
  weather$t.delta.max.L2 <- NULL
  weather$t.delta.max.L3 <- NULL
  print("CFL condition met for all timesteps")
} else {print("CFL condition *NOT* met for all timesteps")}


##################
### References ###
##################

# [1] https://doi.org/10.1061/(ASCE)0899-1561(2007)19:8(683)

# [2] http://bouteloup.pierre.free.fr/lica/phythe/don/air/air_k_plot.pdf

# [3] https://www.maricopa.gov/3769/Weather-Sensor-Data

# [4] https://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html

# [5] http://www-mdp.eng.cam.ac.uk/web/library/enginfo/aerothermal_dvd_only/aero/fprops/propsoffluids/node5.html


##########################
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


