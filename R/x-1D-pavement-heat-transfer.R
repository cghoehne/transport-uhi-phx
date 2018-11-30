# 1D model based on fundamental energy balance to calculate the pavement near-surface temperatures
# based on model outlined in Gui et al. (2007); https://doi.org/10.1061/(ASCE)0899-1561(2007)19:8(683)

##################
### PARAMETERS ###
##################

CFL <- 0
#  Courant-Friedrichs-Lewy (dimensionless);

c	<- 0	
# specific heat (Jkg???1 deg C???1);

h.rad	<- 0
#  radiative heat transfer coefficient (Wm???2 deg C???1);

h.inf <- 0
#  convective heat transfer coefficient (Wm???2 deg C???1);

k	<- 0  # 
#  thermal conductivity (Wm???1 deg C???1);

k.inf <- 0
#  thermal conductivity of air (Wm???2 deg C???1);

L <- 0
#  characteristic length of pavement (m);

Pr.inf <- 0
#  Prandtl number (dimensionless);

p <- 0 # or p+1
#  time step (dimensionless);

q.cond <- 0
#  conduction heat transfer (Wm???2);

q.conv <- 0
#  convection heat transfer (Wm???2);

q.int <- 0
#  heat flux through interface (Wm???2);

q.rad <- 0	
#  infrared radiation heat transfer (Wm???2);

q.s <- 0
#  solar radiation heat transfer (Wm???2);

R.c <- 0
#  contact resistance (W???1m2 deg C);

T.b1 <- 0
#  first temperature node in lower layer ( deg C);

T.dew <- 0
#  dew-point temperature ( deg C);

T.layeri.int <- 0
#  interfacial temperature at layer i ( deg C);

Tn <- 0
#  last temperature node in upper layer ( deg C);

T.ref <- 0 #(max,min)
#  reference temperature ( deg C);

T.s <- 0	
#  pavement surface temperature ( deg C);

T.sky <- 0	
#  sky temperature (K);

T.inf <- 0	
#  atmospheric dry-bulb temperature (K);

U.inf <- 0	
#  wind velocity (ms???1);

x <- 0
#  ground depth (m);

alpha <- 4.0  # range from 2 to 12
#  thermal diffusivity (m2s???1);

albedo <- 0.17
#  albedo (dimensionless);

delta.t <- 0	
#  time-step spacing (s);

delta.x
#  nodal spacing within pavement (m);

epsilon <- 0.8
#  emissivity (dimensionless);

v.inf <- 0
#  kinematic viscosity of air (m2s???1);

rho <- 0
#  density (kgm???3);

rho.c <- 0	
#  volumetric heat capacity (Jm???3 degC???1);

sigma <- 0	
#  Stefan-Boltzmann constant (Wm???2 degC???4); and

psi.sky <- 0	
#  sky view factor (dimensionless).

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
# [ change in T  w.r.t  time (1st order partial) == (k/rho.c) times rate of change of T  w.r.t  x  (2nd order partial) ]


# EQN 5


