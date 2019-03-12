#################################################################
# asphalt parking lot or road low volume traffic layer profiles #
#################################################################

layer.profiles <- list(
  data.table( # Low Volume HMA #1 (50mm DFG + 100mm DFG)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.05, 0.1, 1.35), # layer thickness (m)
    k = c(1.2, 1.6, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(850, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.20, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Low Volume HMA #2 (100mm DFG rebonded 100mm DFG)
    layer = c("surface", "intermediate", "base", "subgrade"),
    thickness = c(0.1, 0.1, 1.3), # layer thickness (m)
    k = c(1.8, 1.8, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2550, 2500, 2450, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(900, 925, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.20, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Low Volume Porous Asphalt #1 (high air voids, crushed agg base/subgrade)
    layer = c("surface", "intermediate", "base", "subgrade"),
    thickness = c(0.075, 0.03, 1.395), # layer thickness (m)
    k = c(0.82, 1.5, 1.46), # layer thermal conductivity (W/(m*degK)) 
    rho = c(1906, 1430, 1600), # layer density (kg/m3) 2382 (base from infravation)
    c = c(946, 840, 880), # layer specific heat (J/(kg*degK)
    albedo = c(0.2, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.9, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Low Volume HMA #1 (50mm DFG + 100mm DFG, Low Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.05, 0.1, 1.35), # layer thickness (m)
    k = c(1.2, 1.6, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(850, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.85, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Low Volume HMA #1 (50mm DFG + 100mm DFG, High Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.05, 0.1, 1.35), # layer thickness (m)
    k = c(1.2, 1.6, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(850, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.10, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names corresponding to the validation site location IDs
# so we can pull weather data from the nearest weather station to match the desired validaiton site
layer.sites <- c("A8", "C4", "C1", "A9", "A6") 
names(layer.profiles) <- c("Low Volume HMA #1 (50mm DFG + 100mm DFG)", 
                           "Low Volume HMA #2 (100mm DFG rebonded 100mm DFG)", 
                           "Low Volume Porous Asphalt #1 (high air voids, crushed agg base/subgrade)", 
                           "Low Volume HMA #1 (50mm DFG + 100mm DFG, Low Albedo)", 
                           "Low Volume HMA #1 (50mm DFG + 100mm DFG, High Albedo)") 

###################################
# high traffic volume asphalt HMA #
###################################

layer.profiles <- list(
  data.table( # High Volume HMA #1 (OGFC 25mm rebonded on 275mm DGHMA)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.025, 0.275, 1.2), # layer thickness (m)
    k = c(0.96, 0.92, 1.10), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2187, 2093, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(964, 953, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.175, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.025, 0.275, 1.2), # layer thickness (m)
    k = c(1.16, 1.08, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2350, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(964, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.175, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume HMA #3 (OGFC 25mm rebonded on 275mm DGHMA)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.025, 0.275, 1.2), # layer thickness (m)
    k = c(2.0, 1.6, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2550, 2350, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(850, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.175, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA, Low Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.025, 0.275, 1.2), # layer thickness (m)
    k = c(1.16, 1.08, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2350, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(964, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.10, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA, HIgh Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.025, 0.275, 1.2), # layer thickness (m)
    k = c(1.16, 1.08, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2350, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(964, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.85, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names corresponding to the validation site location IDs
# so we can pull weather data from the nearest weather station to match the desired validaiton site
layer.sites <- c("A8", "C4", "C1", "A9", "A6") 
names(layer.profiles) <- c("High Volume HMA #1 (OGFC 25mm rebonded on 275mm DGHMA)", 
                           "High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA)", 
                           "High Volume HMA #3 (OGFC 25mm rebonded on 275mm DGHMA)", 
                           "High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA, Low Albedo)", 
                           "High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA, HIgh Albedo)") 


#####################################
# concrete road or parking profiles #
#####################################

layer.profiles <- list(
  data.table( # Low Volume thin whitetopping bonded on HMA
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.1, 0.1, 1.3), # layer thickness (m)
    k = c(1.4, 1.6, 1.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(2240, 2370, 1500), # layer density (kg/m3)
    c = c(1050, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.325, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Low Volume ultra-thin whitetopping bonded on HMA
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.06, 0.1, 1.34), # layer thickness (m)
    k = c(1.4, 1.6, 1.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(2240, 2370, 1500), # layer density (kg/m3)
    c = c(1050, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.30, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume thin whitetopping bonded on HMA
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.1, 0.275, 1.225), # layer thickness (m)
    k = c(1.4, 1.6, 1.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(2240, 2350, 1500), # layer density (kg/m3)
    c = c(1050, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.325, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume ultra-thin whitetopping bonded on HMA
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.06, 0.3, 1.14), # layer thickness (m)
    k = c(1.4, 1.6, 1.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(2240, 2350, 1500), # layer density (kg/m3)
    c = c(1050, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.30, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Low Volume PCC w/ additives
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.4, 0.1, 1.0), # layer thickness (m)
    k = c(2.0, 1.6, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2250, 1500), # layer density (kg/m3)
    c = c(1050, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.275, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names corresponding to the validation site location IDs
# this will pull weather data from the nearest weather site with data to the validaiton site specfied
layer.sites <- c("A8", "C4", "C1", "A9", "A6") 
names(layer.profiles) <- c("Low Volume thin whitetopping bonded on HMA", 
                           "Low Volume ultra-thin whitetopping bonded on HMA", 
                           "High Volume thin whitetopping bonded on HMA", 
                           "High Volume ultra-thin whitetopping bonded on HMA", 
                           "Low Volume PCC w/ additives") 


#########################
# bare dirt/soil/ground #
#########################

layer.profiles <- list(
  data.table( # Bare Dry Soil #1
    layer = c("surface", "base", "subgrade"),
    thickness = c(1.5), # layer thickness (m)
    k = c(1.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500), # layer density (kg/m3)
    c = c(1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.40), # surface albedo (dimensionless)
    emissivity = c(0.92, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Bare Dry Soil #2
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.5, 1.0), # layer thickness (m)
    k = c(1.0, 1.1), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500, 1600), # layer density (kg/m3)
    c = c(1500, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.45, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )

)

# define layer profile names corresponding to the validation site location IDs
# this will pull weather data from the nearest weather site with data to the validaiton site specfied
layer.sites <- c("B1", "B2") 
names(layer.profiles) <- c("Bare Dry Soil #1", 
                           "Bare Dry Soil #2") 
