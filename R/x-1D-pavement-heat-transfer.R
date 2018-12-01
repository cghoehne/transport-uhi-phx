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

# weather data
w.data <- readRDS(here("data/outputs/temp/2017-weather-data.rds")) # all houlry weather data
w.stations <- readRDS(here("data/outputs/temp/2017-station-data.rds")) # all station data

# choose sample weather for hot day in 2017
#weather <- w.data[id == "KPHX" & date(date.time) == "2017-06-21"]
weather <- w.data[id == "City of Glendale" & date(date.time) == "2017-06-21" & !is.na(solar)]

##################
### PARAMETERS ###
##################

CFL <- 0
#  Courant-Friedrichs-Lewy (dimensionless);

c	<- c("surface" = 921, "base" = 921, "subgrade" = 1900)	
# specific heat (J/(kg deg C);

h.rad	<- 0
#  radiative heat transfer coefficient (W/(m^2 deg C));

h.inf <- 0
#  convective heat transfer coefficient (W/(m^2 deg C))

k	<- c("surface" = 1.21, "base" = 1.21, "subgrade" = 1.00)	
#  thermal conductivity (W/(m^2 deg C))

k.inf <- 0
#  thermal conductivity of air (W/(m^2 deg C));

L <- 0
#  characteristic length of pavement (m);

Pr.inf <- 0
#  Prandtl number (dimensionless);

p <- 0 # or p+1
#  time step (dimensionless);

q.cond <- 0
#  conduction heat transfer (W/m^2);

q.conv <- 0
#  convection heat transfer (W/m^2);

q.int <- 0
#  heat flux through interface (W/m^2);

q.rad <- 0	
#  infrared radiation heat transfer (W/m^2);

q.s <- 0
#  solar radiation heat transfer (W/m^2);

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

T.s <- 0	
#  pavement surface temperature (deg C);

T.sky <- 0	
#  sky temperature (K);

T.inf <- 0	
#  atmospheric dry-bulb temperature (K);

U.inf <- 0	
#  wind velocity (m/s);

x <- 3.048
#  ground depth (m);

alpha <- 4.0  # range from 2 to 12
#  thermal diffusivity (m^2/s);

albedo <- 0.17
#  albedo (dimensionless);

delta.t <- 0	
#  time-step spacing (s);

delta.x <- 12.7/1000 # 12.7mm (0.5in)  10.3992/jgb.2.2.121
#  nodal spacing within pavement (m);

epsilon <- 0.8
#  emissivity (dimensionless);

v.inf <- 0
#  kinematic viscosity of air (m^2/s);

rho <- c("surface" = 2238, "base" = 2238, "subgrade" = 1500)
#  density (kg/m^3);

rho.c <- 2.2*10^6
#  volumetric heat capacity (J/(m^3 degC));

sigma <- 5.67*10^8 # via Tu, Li, and Guan (2010); http://doi.org/10.1109/iCECE.2010.926
#  Stefan-Boltzmann constant (W/(m^2 degC^4); and

psi.sky <- 0	
#  sky view factor (dimensionless).

# thickness
x.layer <- c("surface" = 0.1, "base" = 0.1, "subgrade" = 0)
x.layer[3] <- x - x.layer[1] - x.layer[2]  
x.layer.df <- as.data.table(data.frame("layer" = names(x.layer), 
                                    "u.b" = c(0, x.layer[1],  x.layer[1] + x.layer[2]),
                                    "l.b" = c(0 + x.layer[1], x.layer[1] + x.layer[2], x.layer[1] + x.layer[2] + x.layer[3])))
View(x.layer.df)

# HMA:
# surface: 0.0625 - 0.123 m 
# base: 0.05 - 0.150 m
# subgrade: soil

# PCC:
# surface: 0.0625 - 0.123 m 
# base: 0.075 - 0.350 m
# subgrade: soil

#################
### EQUATIONS ###
#################

## Radiation Heat Transfer

# EQN 1: outgoing radiation
q.rad <- psi.sky * epsilon * sigma * ((T.s ^ 4) - (T.sky ^ 4))
q.rad <- h.rad * (T.s - T.sky)

# EQN 2: sky temperature (ASHRAE 2005)
T.sky <- T.inf * (0.004 * T.dew + 0.8)^0.25


## Convection Heat Transfer

# EQN 3: convective heat transfer occuring at the surface
q.conv <- h.inf * (T.s - T.inf)

# EQN 4: convective heat coefficient of air (assuming laminar flow over a flat plate)
h.inf <- 0.664 * (k.inf * (Pr.inf ^ 0.3) * (v.inf ^ -0.5) * (L ^ -0.5) * (U.inf ^ 0.5))


## Governing Equation

# EQN 5
# [ change in T  w.r.t  time (1st order partial) == (k/rho.c) times rate of change of T  w.r.t  x  (2nd order partial) ]

# EQN 6

# EQN 7

# EQN 8

# create data from pavement temperature at depth
p.data <- as.data.table(data.frame("x" = seq(0, x, delta.x), 
                                   "T0" = rep(33.5, x/delta.x+1)))
# name layers appropriately
for(l in names(x.layer)){
  p.data[x <= x.layer.df[layer == l, l.b] & x >= x.layer.df[layer == l, u.b], layer := l]
}

# create layer transition nodes 
p.data[,layer := factor(layer, levels = c("surface","base","subgrade"))]
s <- split(p.data, f = p.data$layer)
p.data.2 <- rbind(s[[1]],
                  s[[1]][.N],
                  s[[2]],
                  s[[2]][.N],
                  s[[3]])

p.data.2[s[[1]][,.N]+1, `:=`(x = x.layer.df$l.b[1], layer = "boundary")]
p.data.2[s[[1]][,.N]+1+s[[2]][,.N]+1, `:=`(x = x.layer.df$l.b[2], layer = "boundary")]

View(p.data.2)
