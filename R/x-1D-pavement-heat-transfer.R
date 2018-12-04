# 1D model based on fundamental energy balance to calculate the pavement near-surface temperatures
# based on model outlined in Gui et al. (2007); https://doi.org/10.1061/(ASCE)0899-1561(2007)19:8(683)

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


## WEATHER DATA 

# import weather data
w.data <- readRDS(here("data/outputs/temp/2017-weather-data.rds")) # all houlry weather data
w.stations <- readRDS(here("data/outputs/temp/2017-station-data.rds")) # all station data

# choose sample weather for hot day in 2017
weather <- w.data[id == "City of Glendale" & date(date.time) == "2017-06-21" & !is.na(solar)]


## PAVEMENT SPECIFICATIONS

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
                                   "layer" = "surface",
                                   "t0" = rep(33.5, x/delta.x+1)))
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
t.step <- sapply(1:length(weather$date.time), function(x) paste0("t",x))
p.data[, (t.step) := t0] # assign new columns with t0 temp

# convert weather data into approariate units
weather$T.inf <- weather$temp.c + 273.15 #  atmospheric dry-bulb temperature (K);
weather$T.sky <- weather$T.inf * ((0.004 * weather$dewpt.c + 0.8)^0.25) # sky temperature (K), caluclated using equation (2)

# store other relavant parameters
p.n <- length(weather$date.time) # store final time step n
q.rad <- vector(mode = "double", length = p.n) # create empty vector to store outgoing infared radiation at every time step
h.rad <- vector(mode = "double", length = p.n) # create empty vector to store the radiative coefficient at every time step
q.s <- weather$solar #  solar radiation heat transfer (W/m^2) 
k	<- c("surface" = 1.21, "base" = 1.21, "subgrade" = 1.00) #  thermal conductivity (W/(m^2 deg C))
rho <- c("surface" = 2238, "base" = 2238, "subgrade" = 1500) #  pavement density (kg/m^3);
c	<- c("surface" = 921, "base" = 921, "subgrade" = 1900) # specific heat (J/(kg deg C);
alpha <- 4.0  # thermal diffusivity (m^2/s), typically range from 2 to 12
albedo <- 0.17 #  albedo (dimensionless);
epsilon <- 0.8 #  emissivity (dimensionless);
rho.c <- 2.2*10^6 #  volumetric heat capacity (J/(m^3 degC));
sigma <- 5.67*10^(-8) #  Stefan-Boltzmann constant (W/(m^2 K^-4); 
SVF <- 1	#  sky view factor (dimensionless [0,1]). assume 1.0: pavement is completely visible to sky


## MODEL

# iterate through time steps and model pavement heat transfer
for(p in 1:p.n){
  
  # store index of time step column in p.data
  i <- t.step[p] # intitial state at time 0 (t0) is 3rd column, t1 is 4th col (1+3)
  
  # store current surface temp in deg C
  T.s <- p.data[[1,i]] 
  
  ## Radiation Heat Transfer
  # calc outgoing infared radiation and radiative coefficient w/ equation 1 for time p
  q.rad[p] <- SVF * epsilon * sigma * ((T.s ^ 4) - (weather$T.sky[p] ^ 4)) # outgoing infared radiation
  h.rad[p] <-  (T.s - weather$T.sky[p]) / q.rad[p] # radiative coefficient
  
}




# parameters needed to check CFL condition for stability
Pr.inf <- 0.71 #  Prandtl number (dimensionless);
#  thermal conductivity of air (W/(m^2 deg C)); calc using: http://bouteloup.pierre.free.fr/lica/phythe/don/air/air_k_plot.pdf
k.inf <- ((1.5207E-11) * (weather$temp.c^3)) - ((4.8574E-08) * (weather$temp.c^2)) + ((1.0184E-04) * (weather$temp.c)) - 3.9333E-04
U.inf <- 0	#  wind velocity (m/s); ASSUME ZERO WIND
v.inf <- 0.02 #  kinematic viscosity of air (m^2/s); ~0.02
L <- 10 #  characteristic length of pavement (m);

# calcluate kinematic visocsity of air using Sutherland's formula (Crane, 1988):
#a <- 0.555*524.07+120
#b <- 0.555*(weather$T.inf*1.8)+120 # T.inf*1.8 to get air temp in Rankine
#v.inf <- 0.01827 * (a/b)*((weather$T.inf/524.07)^1.5) # in centipoise 
#v.inf <- v.inf * 1000 # convert to N-s/m2
#v.inf <- 1/v.inf


# calc the convective heat coefficient of air (h.inf)
h.inf <- 0.664 * (k.inf * (Pr.inf ^ 0.3) * (v.inf ^ -0.5) * (L ^ -0.5) * (U.inf ^ 0.5))

# check the time steps obey the CFL condition for stability
(rho.c * (delta.x^2)) / (2 * (h.rad * delta.x + h.inf * delta.x + k)) # in seconds

##################
### PARAMETERS ###
##################

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

T.dew <- 0
#  dew-point temperature (deg C);

T.layeri.int <- 0
#  interfacial temperature at layer i (deg C);

T.n <- 0
#  last temperature node in upper layer (deg C);

T.ref <- 0 #(max,min)
#  reference temperature (deg C);




#################
### EQUATIONS ###
#################

## Convection Heat Transfer

# EQN 3: convective heat transfer occuring at the surface
#q.conv <- h.inf * (T.s - T.inf)

# EQN 4: convective heat coefficient of air (assuming laminar flow over a flat plate)
#h.inf <- 0.664 * (k.inf * (Pr.inf ^ 0.3) * (v.inf ^ -0.5) * (L ^ -0.5) * (U.inf ^ 0.5))


