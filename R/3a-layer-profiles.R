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
  #data.table( # Bare Dry Soil #1 (high heat)
  #  layer = c("surface", "subgrade"),
  #  thickness = c(0.75, 0.75), # layer thickness (m)
  #  k = c(2.0, 2.0), # layer thermal conductivity (W/(m*degK))  
  #  rho = c(1400, 1400), # layer density (kg/m3)
  #  c = c(1900, 1900), # layer specific heat (J/(kg*degK)
  #  albedo = c(0.30, NA), # surface albedo (dimensionless)
  #  emissivity = c(0.90, NA), # emissivity (dimensionless)
  #  R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  #),
  #data.table( # Bare Dry Soil #2 (med-high heat)
  #  layer = c("surface","subgrade"), 
  #  thickness = c(0.75, 0.75), # layer thickness (m)
  #  k = c(1.575, 1.575), # layer thermal conductivity (W/(m*degK))  
  #  rho = c(1550, 1550), # layer density (kg/m3)
  #  c = c(1625, 1625), # layer specific heat (J/(kg*degK)
  #  albedo = c(0.35, NA), # surface albedo (dimensionless)
  #  emissivity = c(0.9175, NA), # emissivity (dimensionless)
  #  R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  #),
  data.table( # Bare Dry Soil #2 (med heat)
    layer = c("surface","subgrade"), 
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(1.8, 1.8), # layer thermal conductivity (W/(m*degK))  
    rho = c(2000, 2000), # layer density (kg/m3)
    c = c(1900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.40, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Bare Dry Soil #2 (med-low heat)
    layer = c("surface","subgrade"), 
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(1.4, 1.4), # layer thermal conductivity (W/(m*degK))  
    rho = c(1750, 1750), # layer density (kg/m3)
    c = c(1500, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.45, NA), # surface albedo (dimensionless)
    emissivity = c(0.935, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Bare Dry Soil #3 (low heat)
    layer = c("surface", "subgrade"),
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(1.0, 1.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(1500, 1500), # layer density (kg/m3)
    c = c(1100, 1100), # layer specific heat (J/(kg*degK)
    albedo = c(0.50, NA), # surface albedo (dimensionless)
    emissivity = c(0.97, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.bg) <- c(#"Bare Dry Soil #1",
                              #"Bare Dry Soil #2",
                              "Bare Dry Soil #3",
                              "Bare Dry Soil #4",
                              "Bare Dry Soil #5") 

# CONCRETE

layer.profiles.c <- list(
  #data.table( # PCC (high heat)
  #  layer = c("surface", "base", "subgrade"),
  #  thickness = c(0.10, 0.08, 1.32), # layer thickness (m)
  #  k = c(4.0, 3.0, 2.0), # layer thermal conductivity (W/(m*degK)) 
  #  rho = c(2300, 2200, 1400), # layer density (kg/m3)
  #  c = c(1200, 1100, 1900), # layer specific heat (J/(kg*degK)
  #  albedo = c(0.17, NA, NA), # surface albedo (dimensionless)
  #  emissivity = c(0.90, NA, NA), # emissivity (dimensionless)
  #  R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  #),
  #data.table( # PCC (med-high heat)
  #  layer = c("surface", "base", "subgrade"),
  #  thickness = c(0.15, 0.135, 1.215), # layer thickness (m)
  #  k = c(3.125, 2.60, 1.575), # layer thermal conductivity (W/(m*degK)) 
  #  rho = c(2350, 2250, 1550), # layer density (kg/m3)
  #  c = c(1100, 1025, 1625), # layer specific heat (J/(kg*degK)
  #  albedo = c(0.235, NA, NA), # surface albedo (dimensionless)
  #  emissivity = c(0.915, NA, NA), # emissivity (dimensionless)
  #  R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  #),
  data.table( # PCC (med heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.10, 0.10, 1.3), # layer thickness (m)
    k = c(2.2, 3.0, 1.8), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2400, 2000), # layer density (kg/m3)
    c = c(1050, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # PCC (med-low heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.20, 0.20, 1.1), # layer thickness (m)
    k = c(1.7, 2.25, 1.4), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2250, 2400, 1850), # layer density (kg/m3)
    c = c(945, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.325, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # PCC (low heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.30, 0.20, 1.0), # layer thickness (m)
    k = c(1.2, 1.5, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2100, 2400, 1500), # layer density (kg/m3)
    c = c(840, 800, 1100), # layer specific heat (J/(kg*degK)
    albedo = c(0.40, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.96, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.c) <- c(#"Portland Cement Concrete #1",
                             #"Portland Cement Concrete #2",
                             "Portland Cement Concrete #3",
                             "Portland Cement Concrete #4",
                             "Portland Cement Concrete #5") 

# ASPHALT

layer.profiles.a <- list(
  #data.table( # asphalt (high heat)
  #  layer = c("surface", "base", "subgrade"),
  #  thickness = c(0.04, 0.08, 1.38), # layer thickness (m)
  #  k = c(3.0, 3.0, 2.0), # layer thermal conductivity (W/(m*degK)) 
  #  rho = c(1900, 2200, 1400), # layer density (kg/m3)
   # c = c(2000, 1100, 1900), # layer specific heat (J/(kg*degK)
  #  albedo = c(0.05, NA, NA), # surface albedo (dimensionless)
  #  emissivity = c(0.85, NA, NA), # emissivity (dimensionless)
  #  R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  #),
  #data.table( # asphalt (med-high heat)
  #  layer = c("surface", "base", "subgrade"),
  #  thickness = c(0.105, 0.135, 1.26), # layer thickness (m)
  #  k = c(2.375, 2.60, 1.575), # layer thermal conductivity (W/(m*degK)) 
  #  rho = c(2113, 2250, 1550), # layer density (kg/m3)
  #  c = c(1700, 1025, 1625), # layer specific heat (J/(kg*degK)
  #  albedo = c(0.11, NA, NA), # surface albedo (dimensionless)
  #  emissivity = c(0.87, NA, NA), # emissivity (dimensionless)
  #  R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  #),
  data.table( # asphalt (med heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.10, 0.15, 1.25), # layer thickness (m)
    k = c(2.2, 3.0, 1.8), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2500, 2300, 2000), # layer density (kg/m3)
    c = c(1400, 950, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.85, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt (low-med heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.15, 0.20, 1.15), # layer thickness (m)
    k = c(1.7, 2.25, 1.4), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2350, 1850), # layer density (kg/m3)
    c = c(950, 875, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.20, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt (low heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.2, 0.3, 1.0), # layer thickness (m)
    k = c(1.2, 1.5, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2200, 2400, 1500), # layer density (kg/m3)
    c = c(850, 800, 1100), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.a) <- c(#"Asphalt #1",
                             #"Asphalt #2", 
                             "Asphalt #3",
                             "Asphalt #4",
                             "Asphalt #5")


# COMPOSITE #1 (Whitetopped)
layer.profiles.wa <- list(
  #data.table( # whitetopped asphalt (high heat)
  #  layer = c("surface", "asphalt", "base", "subgrade"),
  #  thickness = c(0.06, 0.04, 0.08, 1.32), # layer thickness (m)
  #  k = c(4.0, 3.0, 3.0, 2.0), # layer thermal conductivity (W/(m*degK)) 
  #  rho = c(2300, 1900, 2200, 1400), # layer density (kg/m3)
  #  c = c(1200, 2000, 1100, 1900), # layer specific heat (J/(kg*degK)
  #  albedo = c(0.20, NA, NA, NA), # surface albedo (dimensionless)
  #  emissivity = c(0.92, NA, NA, NA), # emissivity (dimensionless)
  #  R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  #),
  #data.table( # whitetopped asphalt (med-high heat)
  #  layer = c("surface", "asphalt", "base", "subgrade"),
  #  thickness = c(0.07, 0.105, 0.135, 1.19), # layer thickness (m)
  #  k = c(3.125, 2.375, 2.60, 1.575), # layer thermal conductivity (W/(m*degK)) 
  #  rho = c(2350, 2113, 2250, 1550), # layer density (kg/m3)
  #  c = c(1100, 1700, 1025, 1625), # layer specific heat (J/(kg*degK)
  #  albedo = c(0.2625, NA, NA, NA), # surface albedo (dimensionless)
  #  emissivity = c(0.93, NA, NA, NA), # emissivity (dimensionless)
  #  R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  #),
  data.table( # whitetopped asphalt (med heat)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.08, 0.1, 0.15, 1.17), # layer thickness (m)
    k = c(2.2, 2.2, 3.0, 1.8), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2500, 2400, 2000), # layer density (kg/m3)
    c = c(1050, 1400, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # whitetopped asphalt (low-med heat)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.09, 0.15, 0.2, 1.06), # layer thickness (m)
    k = c(1.7, 1.7, 2.25, 1.4), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2250, 2350, 2400, 1850), # layer density (kg/m3)
    c = c(945, 950, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.325, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # whitetopped asphalt (low heat)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.1, 0.2, 0.3, 0.9), # layer thickness (m)
    k = c(1.2, 1.2, 1.5, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2100, 2200, 2400, 1500), # layer density (kg/m3)
    c = c(840, 850, 800, 1100), # layer specific heat (J/(kg*degK)
    albedo = c(0.4, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.96, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.wa) <- c(#"Whitetopped Asphalt #1", 
                              #"Whitetopped Asphalt #2",
                              "Whitetopped Asphalt #3",
                              "Whitetopped Asphalt #4",
                              "Whitetopped Asphalt #5") 


# COMPOSITE #2 (Asphalt Overlay on PCC)

layer.profiles.oc <- list(
  #data.table( # asphalt overlay on PCC (high heat)
  #  layer = c("surface", "PCC", "base", "subgrade"),
  #  thickness = c(0.05, 0.10, 0.08, 1.27), # layer thickness (m)
  #  k = c(3.0, 4.0, 3.0, 2.0), # layer thermal conductivity (W/(m*degK)) 
  #  rho = c(1900, 2300, 2200, 1400), # layer density (kg/m3)
  #  c = c(2000, 1200, 1100, 1900), # layer specific heat (J/(kg*degK)
  #  albedo = c(0.05, NA, NA, NA), # surface albedo (dimensionless)
  #  emissivity = c(0.85, NA, NA, NA), # emissivity (dimensionless)
  #  R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  #),
  #data.table( # asphalt overlay on PCC (med-high heat)
  #  layer = c("surface", "PCC", "base", "subgrade"),
  #  thickness = c(0.10, 0.15, 0.135, 1.115), # layer thickness (m)
  #  k = c(2.375, 3.125, 2.60, 1.575), # layer thermal conductivity (W/(m*degK)) 
  #  rho = c(2113, 2350, 2250, 1550), # layer density (kg/m3)
  #  c = c(1700, 1100, 1025, 1625), # layer specific heat (J/(kg*degK)
  #  albedo = c(0.11, NA, NA, NA), # surface albedo (dimensionless)
  #  emissivity = c(0.87, NA, NA, NA), # emissivity (dimensionless)
  #  R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  #),
  data.table( # asphalt overlay on PCC (med heat)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.09, 0.20, 0.15, 1.06), # layer thickness (m)
    k = c(2.2, 2.2, 2.74, 1.8), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2500, 2400, 2400, 2000), # layer density (kg/m3)
    c = c(1400, 1000, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.85, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt overlay on PCC (med-low heat)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.12, 0.25, 0.2, 0.93), # layer thickness (m)
    k = c(1.7, 1.7, 1.80, 1.4), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2250, 2400, 1850), # layer density (kg/m3)
    c = c(950, 900, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.20, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt overlay on PCC (low heat)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.150, 0.30, 0.3, 0.75), # layer thickness (m)
    k = c(1.2, 1.2, 1.5, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2200, 2100, 2400, 1500), # layer density (kg/m3)
    c = c(850, 800, 800, 1100), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.oc) <- c(#"Asphalt Overlay on PCC #1",
                              #"Asphalt Overlay on PCC #2",
                              "Asphalt Overlay on PCC #3",
                              "Asphalt Overlay on PCC #4",
                              "Asphalt Overlay on PCC #5") 


# SAVE
saveRDS(layer.profiles.bg, here("data/outputs/layer-profiles-BG.rds"))
saveRDS(layer.profiles.c, here("data/outputs/layer-profiles-C.rds"))
saveRDS(layer.profiles.a, here("data/outputs/layer-profiles-A.rds"))
saveRDS(layer.profiles.wa, here("data/outputs/layer-profiles-WA.rds"))
saveRDS(layer.profiles.oc, here("data/outputs/layer-profiles-OC.rds"))
