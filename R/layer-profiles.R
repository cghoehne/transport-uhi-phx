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
    k = c(0.9, 0.9), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500, 1600), # layer density (kg/m3)
    c = c(1900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.475, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Bare Dry Soil #2
    layer = c("surface","subgrade"),
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(1.1, 1.1), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500, 1600), # layer density (kg/m3)
    c = c(1900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.525, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,  data.table( # Bare Dry Soil #3
    layer = c("surface", "subgrade"),
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(0.9, 0.9), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500, 1550), # layer density (kg/m3)
    c = c(1800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.475, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,  data.table( # Bare Dry Soil #4
    layer = c("surface", "subgrade"),
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(1.1, 1.1), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500, 1550), # layer density (kg/m3)
    c = c(1800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.525, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Bare Dry Soil #5
    layer = c("surface","subgrade"),
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(0.9, 0.9), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500, 1600), # layer density (kg/m3)
    c = c(1700, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.50, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,  data.table( # Bare Dry Soil #6
    layer = c("surface", "subgrade"),
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(1.1, 1.1), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500, 1600), # layer density (kg/m3)
    c = c(1700, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.40, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.bg) <- c("Bare Dry Soil #1",
                              "Bare Dry Soil #2",
                              "Bare Dry Soil #3",
                              "Bare Dry Soil #4",
                              "Bare Dry Soil #5",
                              "Bare Dry Soil #6") 

# CONCRETE / COMPOSITE

layer.profiles.c <- list(
   data.table( # Low Volume PCC w/ additives #1 (0.25 albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.4, 0.1, 1.0), # layer thickness (m)
    k = c(1.4, 1.8, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2300, 1500), # layer density (kg/m3)
    c = c(1050, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Low Volume PCC w/ additives #1 (0.30 albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.4, 0.1, 1.0), # layer thickness (m)
    k = c(1.4, 1.8, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2300, 1500), # layer density (kg/m3)
    c = c(1050, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.3, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Low Volume PCC w/ additives #1 (0.35 albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.4, 0.1, 1.0), # layer thickness (m)
    k = c(1.4, 1.8, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2300, 1500), # layer density (kg/m3)
    c = c(1050, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.35, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Thin whitetopping on Low Volume HMA #1 (100mm DFG rebonded 100mm DFG)
    layer = c("surface","base", "subbase", "subgrade"),
    thickness = c(0.1, 0.1, 0.1, 1.2), # layer thickness (m)
    k = c(1.4, 1.8, 1.8, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2240, 2350, 2400, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(1050, 875, 890, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.3, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Thin whitetopping on Low Volume HMA #2 (75mm DFG + 100mm DFG)
    layer = c("surface", "base", "subbase", "subgrade"),
    thickness = c(0.1, 0.075, 0.1, 1.225), # layer thickness (m)
    k = c(1.4, 1.45, 1.55, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2240, 2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(1050, 900, 875, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.3, NA, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Thin whitetopping on Low Volume HMA #3 (50mm DFG + 100mm DFG)
    layer = c("surface", "base", "subbase", "subgrade"),
    thickness = c(0.1, 0.05, 0.1, 1.25), # layer thickness (m)
    k = c(1.4, 1.2, 1.6, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2240, 2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(1050, 850, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.3, NA, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Ultra-thin whitetopping on Low Volume HMA #1 (100mm DFG rebonded 100mm DFG)
    layer = c("surface","base", "subbase", "subgrade"),
    thickness = c(0.075, 0.1, 0.1, 1.225), # layer thickness (m)
    k = c(1.4, 1.8, 1.8, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2240, 2350, 2400, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(1050, 875, 890, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.3, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Ultra-thin whitetopping on Low Volume HMA #2 (75mm DFG + 100mm DFG)
    layer = c("surface", "base", "subbase", "subgrade"),
    thickness = c(0.075, 0.075, 0.1, 1.25), # layer thickness (m)
    k = c(1.4, 1.45, 1.55, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2240, 2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(1050, 900, 875, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.3, NA, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # Ultra-thin whitetopping on Low Volume HMA #3 (50mm DFG + 100mm DFG)
    layer = c("surface", "base", "subbase", "subgrade"),
    thickness = c(0.075, 0.05, 0.1, 1.275), # layer thickness (m)
    k = c(1.4, 1.2, 1.6, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2240, 2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(1050, 850, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.3, NA, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.c) <- c("Low Stress PCC #1 (0.25 albedo)",
                             "Low Stress PCC #1 (0.30 albedo)",
                             "Low Stress PCC #1 (0.35 albedo)",
                             "Thin whitetopping on Low Stress HMA #1 (100mm DFG rebonded 100mm DFG)",
                             "Thin whitetopping on Low Stress HMA #2 (75mm DFG + 100mm DFG)",
                             "Thin whitetopping on Low Stress HMA #3 (50mm DFG + 100mm DFG)",
                             "Ultra-thin whitetopping on Low Stress HMA #1 (100mm DFG rebonded 100mm DFG)",
                             "Ultra-thin whitetopping on Low Stress HMA #2 (75mm DFG + 100mm DFG)",
                             "Ultra-thin whitetopping on Low Stress HMA #3 (50mm DFG + 100mm DFG)") 

# LOW VOLUME ASPHALT

layer.profiles.lva <- list(
  data.table( # Low Volume HMA #2 (100mm DFG rebonded 100mm DFG)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.1, 0.1, 1.3), # layer thickness (m)
    k = c(1.8, 1.8, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2400, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(875, 890, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Low Volume HMA #1 (75mm DFG + 100mm DFG)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.075, 0.1, 1.325), # layer thickness (m)
    k = c(1.45, 1.55, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(900, 875, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Low Volume HMA #1 (50mm DFG + 100mm DFG)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.05, 0.1, 1.35), # layer thickness (m)
    k = c(1.2, 1.6, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(850, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Low Volume HMA #2 (100mm DFG rebonded 100mm DFG)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.1, 0.1, 1.3), # layer thickness (m)
    k = c(1.8, 1.8, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2400, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(875, 890, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Low Volume HMA #1 (75mm DFG + 100mm DFG)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.075, 0.1, 1.325), # layer thickness (m)
    k = c(1.45, 1.55, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2370, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(900, 875, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA ,NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Low Volume HMA #1 (50mm DFG + 100mm DFG)
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
names(layer.profiles.lva) <- c("Low Volume HMA (100mm DFG + 100mm DFG, 0.15 albedo)",
                               "Low Volume HMA (75mm DFG + 100mm DFG, 0.15 albedo)", 
                               "Low Volume HMA (50mm DFG + 100mm DFG, 0.15 albedo)",
                               "Low Volume HMA (100mm DFG + 100mm DFG, 0.25 albedo)",
                               "Low Volume HMA (75mm DFG + 100mm DFG, 0.25 albedo)", 
                               "Low Volume HMA (50mm DFG + 100mm DFG, 0.25 albedo)")

# HIGH VOLUME ASPHALT
layer.profiles.hva <- list(
  data.table( # High Volume HMA #1 (OGFC 25mm rebonded on 275mm DGHMA)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.035, 0.265, 1.2), # layer thickness (m)
    k = c(0.96, 0.92, 1.10), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2300, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(945, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.87, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA, Low Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.025, 0.275, 1.2), # layer thickness (m)
    k = c(1.16, 1.10, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2350, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(964, 940, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA, HIgh Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.05, 0.25, 1.2), # layer thickness (m)
    k = c(1.1, 1.2, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2450, 2400, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(964, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.85, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,  data.table( # High Volume HMA #1 (OGFC 25mm rebonded on 275mm DGHMA)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.035, 0.265, 1.2), # layer thickness (m)
    k = c(0.96, 0.92, 1.10), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2300, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(945, 900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.87, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA, Low Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.025, 0.275, 1.2), # layer thickness (m)
    k = c(1.16, 1.10, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2350, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(964, 940, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
  ,data.table( # High Volume HMA #2 (OGFC 25mm rebonded on 275mm DGHMA, HIgh Albedo)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.05, 0.25, 1.2), # layer thickness (m)
    k = c(1.1, 1.2, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2450, 2400, 1500), # layer density (kg/m3) 2382 (base from infravation)
    c = c(964, 960, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.85, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.hva) <- c("High Volume HMA (OGFC 35mm + 265mm DGHMA, 0.15 albedo)", 
                               "High Volume HMA (OGFC 25mm + 275mm DGHMA, 0.15 albedo)", 
                               "High Volume HMA (OGFC 50mm + 250mm DGHMA, 0.15 albedo)",
                               "High Volume HMA (OGFC 35mm + 265mm DGHMA, 0.25 albedo)", 
                               "High Volume HMA (OGFC 25mm + 275mm DGHMA, 0.25 albedo)", 
                               "High Volume HMA (OGFC 50mm + 250mm DGHMA, 0.25 albedo)") 

# SAVE
saveRDS(layer.profiles.bg, here("data/outputs/layer-profiles-BG.rds"))
saveRDS(layer.profiles.c, here("data/outputs/layer-profiles-C.rds"))
saveRDS(layer.profiles.lva, here("data/outputs/layer-profiles-LVA.rds"))
saveRDS(layer.profiles.hva, here("data/outputs/layer-profiles-HVA.rds"))
