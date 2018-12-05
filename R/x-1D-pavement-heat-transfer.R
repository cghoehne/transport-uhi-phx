# 1D model based on fundamental energy balance to calculate the pavement near-surface temperatures
# based on model outlined in Gui et al. (2007) [1]

# start by maxing out the memory allocation (we use high number to force max allocation)
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
                                   #"t0" = rep(33.5, x/delta.x+1)))
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

# create timestep columns (in this case, column of pavement temps by depth calculated every 30mins for 24 hours)
#t.step <- sapply(1:length(weather$date.time), function(x) paste0("t",x))
#p.data[, (t.step) := t0] # assign new columns with t0 temp
t.step <- seq(from = 0, # from time zero
              to = as.numeric(difftime(max(weather$date.time), min(weather$date.time), units = "secs")), # to max time difference in weather data
              by = 2*60) # time step sequence every 2 mins (in units of seconds)
p.n <- length(t.step) # store final time step n
delta.t <- t.step[2] - t.step[1] # change in time step (constant)

pave.time <- as.data.table(data.frame("time.s" = rep(t.step, each = nrow(p.data)),
                                      "node" = rep(0:(nrow(p.data)-1), p.n),
                                      "depth.m" = rep(p.data$x, p.n),
                                      "layer" = rep(p.data$layer, p.n),
                                      "T.degC" = 33.5)) # intialize temps as 33.5 deg C 

# static parameters for pavement heat transfer

#weather$solar # solar radiation heat transfer (W/m^2) 
alpha <- 4.0  # thermal diffusivity (m^2/s), typically range from 2 to 12; [1]
albedo <- 0.17 #  albedo (dimensionless); [1]
epsilon <- 0.8 #  emissivity (dimensionless); [1]
rho.c <- 2.2*10^6 #  volumetric heat capacity (J/(m^3 degC)); [1]
sigma <- 5.67*10^(-8) #  Stefan-Boltzmann constant (W/(m^2 K^-4); [1]
SVF <- 1	# sky view factor (dimensionless [0,1]). assume 1.0: pavement is completely visible to sky


# parameters that vary by pavement layer
k	<- c("surface" = 1.21, "base" = 1.21, "subgrade" = 1.00) #  thermal conductivity (W/(m^2 deg C))
rho <- c("surface" = 2238, "base" = 2238, "subgrade" = 1500) #  pavement density (kg/m^3);
c	<- c("surface" = 921, "base" = 921, "subgrade" = 1900) # specific heat (J/(kg deg C);

# static parameters needed to check CFL condition for stability
Pr.inf <- 0.71 #  Prandtl number (dimensionless);
v.inf <- 0.02 #  kinematic viscosity of air (m^2/s); ~0.02
L <- 10 #  characteristic length of pavement (m);

## calculate time/weather-dependant parameters
weather$time.s <- as.numeric(difftime(weather$date.time, weather$date.time[1], units = "secs")) # time.s to match with iterations in weather data (assuming first obs is time zero)
weather$T.inf <- weather$temp.c + 273.15 #  atmospheric dry-bulb temperature (K);
weather$T.sky <- weather$T.inf * ((0.004 * weather$dewpt.c + 0.8)^0.25) # sky temperature (K), caluclated using equation (2)
weather$k.inf <- ((1.5207E-11) * (weather$temp.c^3)) - ((4.8574E-08) * (weather$temp.c^2)) + ((1.0184E-04) * (weather$temp.c)) - 3.9333E-04 # Ref [2]
weather$h.inf <- 0.664 * (weather$k.inf * (Pr.inf ^ 0.3) * (v.inf ^ -0.5) * (L ^ -0.5) * ((weather$winspd * 0.44704) ^ 0.5)) # convective heat coefficient of air
# U.inf (m/s) is weather$winspd * 0.44704 (winspd is in mph, so multiply by 0.44704 to get m/s)

## MODEL HEAT TRANSFER OF PAVEMENT

# iterate through time steps and model pavement heat transfer

# predefine variables that will need to be interpolated
q.rad <- vector(mode = "numeric", length = p.n)
h.rad <- vector(mode = "numeric", length = p.n)
T.sky <- vector(mode = "numeric", length = p.n)
T.inf <- vector(mode = "numeric", length = p.n)
T.s <- vector(mode = "numeric", length = p.n)
solar <- vector(mode = "numeric", length = p.n)
k.inf <- vector(mode = "numeric", length = p.n)
h.inf <- vector(mode = "numeric", length = p.n)
temp.c <- vector(mode = "numeric", length = p.n)
winspd <- vector(mode = "numeric", length = p.n)

for(p in 1:(p.n-1)){ # state at time p is used to model time p+1, so stop and p-1 to get final model output at time p
  
  # current pavement surface temp
  T.s[p] <- pave.time[time.s == t.step[p] & depth.m == 0, T.degC]
  
  w <- which(t.step[p] == weather$time.s) # store location of timestep match (will be empty if no match)
  if(length(w) == 1){ # if timestep match is valid (there is a weather obs at this timestep in the model)
    
    # use observation values for calculations
    T.sky[p] <- weather$T.sky[w]
    T.inf[p] <- weather$T.inf[w]
    solar[p] <- weather$solar[w]
    k.inf[p] <- weather$k.inf[w]
    h.inf[p] <- weather$h.inf[w]
    temp.c[p] <- weather$temp.c[w]
    winspd[p] <- weather$winspd[w] * 0.44704 # convert to m/s from mph
    
  } else { # if there isn't a match, interpolate parameters linearlly (solar, temp)
    
    # find the closest two obsevations indices and store thier obs time
    w1 <- which.min(abs(weather$time.s - t.step[p]))
    w2 <- if(w1 == 1){2} else if(which.min(c(abs(weather$time.s[w1-1] - t.step[p]),abs(weather$time.s[w1+1] - t.step[p]))) == 1){w1-1} else {w1+1}
  
    t1 <- weather$time.s[w1]
    t2 <- weather$time.s[w2]
    
    # iterpolate btwn necessary variables
    T.sky[p] <- (weather$T.sky[w1] * (abs(t.step[p] - t1) / abs(t2 - t1))) +
                (weather$T.sky[w2] * (abs(t.step[p] - t2) / abs(t2 - t1)))
    
    solar[p] <- (weather$solar[w1] * (abs(t.step[p] - t1) / abs(t2 - t1))) +
                (weather$solar[w2] * (abs(t.step[p] - t2) / abs(t2 - t1)))
    
    temp.c[p] <- (weather$temp.c[w1] * (abs(t.step[p] - t1) / abs(t2 - t1))) +
                 (weather$temp.c[w2] * (abs(t.step[p] - t2) / abs(t2 - t1))) 
    
    winspd[p] <- (weather$winspd[w1] * (abs(t.step[p] - t1) / abs(t2 - t1))) +
                 (weather$winspd[w2] * (abs(t.step[p] - t2) / abs(t2 - t1)))
    
    T.inf[p] <- temp.c[p] + 273.15 #  temp.c alread interpolated, convert to Kelvin
  }
  
  # estimate parameters
  q.rad[p] <- SVF * epsilon * sigma * ((T.s[p] ^ 4) - (T.sky[p] ^ 4)) # outgoing infared radiation
  h.rad[p] <- (T.s[p] - T.sky[p]) / q.rad[p] # radiative coefficient
  h.inf[p] <- 0.664 * (k.inf[p] * (Pr.inf ^ 0.3) * (v.inf ^ -0.5) * (L ^ -0.5) * (winspd[p] ^ 0.5)) # windspeed is correctly in m/s here
  k.inf[p] <- ((1.5207E-11) * (temp.c[p]^3)) - ((4.8574E-08) * (temp.c[p]^2)) + ((1.0184E-04) * (temp.c[p])) - 3.9333E-04 # Ref [2]
  
  # SURFACE NODE at time p+1
  # solve for surface node, s (pavement surface temp at time p+1) (EQN 10)

  pave.time[time.s == t.step[p+1] & node == 0, T.degC := 
              ((((1 - albedo) * solar[p])
                + (h.inf[p] * (T.inf[p] - T.s[p]))
                + (h.rad[p] * (T.sky[p] - T.s[p]))
                + ((k["surface"] * (pave.time[time.s == t.step[p] & node == 1, T.degC] - T.s[p]))
                   / delta.x))
               * (2 * delta.t / (rho.c * delta.x)))]

  # INTERIOR NODES m at time p+1
  # solve for interior node m at time p+1 using state at time p (EQN 9)
  for(m in 1:(max(pave.time$node)-1)){ # for nodes 1 to max(nodes)-1
    # calculate the p+1 temps at node m based on temps at time p at nodes m, m-1, and m+1
    
    # store the change in x between m and m-1 and m and m+1 (this is different at the boundary)
    d.x.up <- pave.time[time.s == t.step[p+1] & node == (m), depth.m] - pave.time[time.s == t.step[p+1] & node == (m-1), depth.m]
    d.x.dn <- pave.time[time.s == t.step[p+1] & node == (m+1), depth.m] - pave.time[time.s == t.step[p+1] & node == (m), depth.m]
    
    # if the node is a boundary node, use boundary condition to solve for the next time step, otherwise not
    if(pave.time[time.s == t.step[p] & node == m, layer] == "boundary"){
  
      # store k values at above and below the boundary (they are different)
      k.up <- k[pave.time[time.s == t.step[p+1] & node == (m-1), layer]]
      k.dn <- k[pave.time[time.s == t.step[p+1] & node == (m+1), layer]]
      
      pave.time[time.s == t.step[p+1] & node == m, T.degC := 
                  ((k.up / k.dn) * (d.x.dn / d.x.up) * pave.time[time.s == t.step[p] & node == (m-1), T.degC])
                / (1 + ((k.up / k.dn) * (d.x.dn / d.x.up)))]
      
    } else { # otherwise calc for non-boundary
      
      # store k value at layer
      k.m <- k[pave.time[time.s == t.step[p+1] & node == (m-1), layer]]
      
      # non-interface calc for  at p+1
      pave.time[time.s == t.step[p+1] & node == m, T.degC := 
                  ((k.m * delta.t / rho.c) * (pave.time[time.s == t.step[p] & node == (m-1), T.degC]
                                             + pave.time[time.s == t.step[p] & node == (m+1), T.degC]
                                             - 2 * pave.time[time.s == t.step[p] & node == m, T.degC])) 
                + pave.time[time.s == t.step[p] & node == m, T.degC]] 
    }
  }
}


# print script endtime
t.end <- Sys.time() 
paste0("Completed task at ", t.end, ". Task took ", round(difftime(t.end,t.start, units = "mins"),1)," minutes to complete.") # paste total script time
# end






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
### PARAMETERS ###
##################

# calcluate kinematic visocsity of air using Sutherland's formula (Crane, 1988):
#a <- 0.555*524.07+120
#b <- 0.555*(weather$T.inf*1.8)+120 # T.inf*1.8 to get air temp in Rankine
#v.inf <- 0.01827 * (a/b)*((weather$T.inf/524.07)^1.5) # in centipoise 
#v.inf <- v.inf * 1000 # convert to N-s/m2
#v.inf <- 1/v.inf

CFL <- 0
#  Courant-Friedrichs-Lewy (dimensionless);

q.cond <- 0
#  conduction heat transfer (W/m^2);

q.conv <- 0
#  convection heat transfer (W/m^2);

q.int <- 0
#  heat flux through interface (W/m^2);

q.rad <- 0	
#  infrared radiation heat transfer (W/m^2);

R.c <- c("layer 1 and 2" = 0, "layer 2 and 3" = 0)
#  contact resistance ((m2 deg C)/W);

T.b1 <- 0
#  first temperature node in lower layer (deg C);

T.layeri.int <- 0
#  interfacial temperature at layer i (deg C);

T.n <- 0
#  last temperature node in upper layer (deg C);

T.ref <- 0 #(max,min)
#  reference temperature (deg C);

#k.inf <- ((1.5207E-11) * (weather$temp.c^3)) - ((4.8574E-08) * (weather$temp.c^2)) + ((1.0184E-04) * (weather$temp.c)) - 3.9333E-04
#sapply(1:nrow(weather), function(p) ((1.5207E-11) * (weather$temp.c[p]^3)) - ((4.8574E-08) * (weather$temp.c[p]^2)) + ((1.0184E-04) * (weather$temp.c[p])) - 3.9333E-04)


##################
### References ###
##################

# [1] https://doi.org/10.1061/(ASCE)0899-1561(2007)19:8(683)

# [2] http://bouteloup.pierre.free.fr/lica/phythe/don/air/air_k_plot.pdf

# [3] https://www.maricopa.gov/3769/Weather-Sensor-Data



