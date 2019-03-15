# LAYER PROFILES

if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(data.table, lib.loc = lib.path, quietly = T)
library(here, lib.loc = lib.path, quietly = T)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # archive date for all used packages (besides checkpoint itself!)
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

# specify layer profiles as list of data.tables 

# BARE GROUND
layer.profiles.bg <- list(
  data.table( # Bare Dry Soil #1
    layer = c("surface", "subgrade"),
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(0.9, 1.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500, 1500), # layer density (kg/m3)
    c = c(1900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.45, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Bare Dry Soil #2
    layer = c("surface","subgrade"),
    thickness = c(0.5, 1.0), # layer thickness (m)
    k = c(1.0, 1.1), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500, 1600), # layer density (kg/m3)
    c = c(1500, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.50, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,  data.table( # Bare Dry Soil #3
    layer = c("surface", "subgrade"),
    thickness = c(1.0, 0.5), # layer thickness (m)
    k = c(0.9, 1.1), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500, 1600), # layer density (kg/m3)
    c = c(1900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.55, NA), # surface albedo (dimensionless)
    emissivity = c(0.92, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Bare Dry Soil #4
    layer = c("surface","subgrade"),
    thickness = c(1.25, 0.25), # layer thickness (m)
    k = c(1.0, 1.05), # layer thermal conductivity (W/(m*degK))  
    rho = c(1400, 1600), # layer density (kg/m3)
    c = c(1600, 1800), # layer specific heat (J/(kg*degK)
    albedo = c(0.60, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.bg) <- c("Bare Dry Soil #1", 
                           "Bare Dry Soil #2",
                           "Bare Dry Soil #3",
                           "Bare Dry Soil #4") 

# CONCRETE / COMPOSITE

layer.profiles.c <- list(
  data.table( # Low Volume PCC w/ additives #1
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.4, 0.1, 1.0), # layer thickness (m)
    k = c(2.0, 1.6, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2440, 2250, 1500), # layer density (kg/m3)
    c = c(1050, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.275, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Low Volume PCC w/ additives #2
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.4, 0.1, 1.0), # layer thickness (m)
    k = c(1.4, 1.8, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2300, 1500), # layer density (kg/m3)
    c = c(1050, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.275, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume PCC w/ additives 
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.6, 0.15, 0.75), # layer thickness (m)
    k = c(1.6, 3.0, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2420, 2350, 1500), # layer density (kg/m3)
    c = c(1050, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume thin whitetopping bonded on HMA
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.1, 0.275, 1.225), # layer thickness (m)
    k = c(1.4, 1.6, 1.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(2240, 2350, 1500), # layer density (kg/m3)
    c = c(1050, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.375, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume ultra-thin whitetopping bonded on HMA
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.075, 0.3, 1.125), # layer thickness (m)
    k = c(1.5, 1.6, 1.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(2250, 2400, 1500), # layer density (kg/m3)
    c = c(1050, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.35, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.c) <- c("Low Volume PCC w/ additives #1", 
                           "Low Volume PCC w/ additives #2", 
                           "High Volume PCC w/ additives",
                           "High Volume thin whitetopping bonded on HMA", 
                           "High Volume ultra-thin whitetopping bonded on HMA") 

# LOW VOLUME ASPHALT

layer.profiles.lva <- list(
  data.table( # Low Volume HMA #1 (50mm DFG + 100mm DFG, Lower Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.05, 0.1, 1.35), # layer thickness (m)
    k = c(1.3, 1.5, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(850, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.10, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Low Volume HMA #2 (100mm DFG rebonded 100mm DFG, Higher Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.1, 0.1, 1.3), # layer thickness (m)
    k = c(1.8, 1.8, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2550, 2450, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(900, 925, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Low Volume Porous Asphalt #1 (high air voids, crushed agg base/subgrade)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.08, 0.05, 1.37), # layer thickness (m)
    k = c(0.9, 1.5, 1.35), # layer thermal conductivity (W/(m*degK)) 
    rho = c(1900, 1550, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(946, 840, 1000), # layer specific heat (J/(kg*degK)
    albedo = c(0.175, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.9, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Low Volume HMA #1 (75mm DFG + 100mm DFG, Lower Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.075, 0.1, 1.325), # layer thickness (m)
    k = c(1.4, 1.7, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(850, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.10, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.85, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Low Volume HMA #1 (50mm DFG + 100mm DFG, Higher Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.05, 0.1, 1.35), # layer thickness (m)
    k = c(1.2, 1.6, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(850, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.lva) <- c("Low Volume HMA #1 (50mm DFG + 100mm DFG, Lower Albedo)", 
                           "Low Volume HMA #2 (100mm DFG + 100mm DFG, Higher Albedo)", 
                           "Low Volume Porous Asphalt #1 (high air voids, crushed agg base/subgrade)", 
                           "Low Volume HMA #3 (75mm DFG + 100mm DFG, Lower Albedo)", 
                           "Low Volume HMA #4 (50mm DFG + 100mm DFG, Higher Albedo)")

# HIGH VOLUME ASPHALT
layer.profiles.hva <- list(
  data.table( # High Volume HMA #1 (OGFC 25mm rebonded on 275mm DGHMA)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.035, 0.265, 1.2), # layer thickness (m)
    k = c(0.96, 0.92, 1.10), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2300, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(945, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.025, 0.275, 1.2), # layer thickness (m)
    k = c(1.16, 1.08, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2350, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(975, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.20, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume HMA #3 (OGFC 25mm rebonded on 275mm DGHMA)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.04, 0.26, 1.2), # layer thickness (m)
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
    rho = c(2450, 2350, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(964, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.10, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA, HIgh Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.05, 0.25, 1.2), # layer thickness (m)
    k = c(1.2, 1.08, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2500, 2400, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(964, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.85, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.hva) <- c("Med Volume HMA #1 (OGFC 35mm rebonded on 265mm DGHMA)", 
                           "Med Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA)", 
                           "High Volume HMA #1 (OGFC 40mm rebonded on 260mm DGHMA, Low Albedo)", 
                           "High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA, Med Albedo)", 
                           "High Volume HMA #3 (OGFC 50mm rebonded on 250mm DGHMA, High Albedo)") 

# SAVE
saveRDS(layer.profiles.bg, here("data/outputs/layer-profiles-BG.rds"))
saveRDS(layer.profiles.c, here("data/outputs/layer-profiles-C.rds"))
saveRDS(layer.profiles.lva, here("data/outputs/layer-profiles-LVA.rds"))
saveRDS(layer.profiles.hva, here("data/outputs/layer-profiles-HVA.rds"))
