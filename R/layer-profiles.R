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
    k = c(1.15, 1.15), # layer thermal conductivity (W/(m*degK))  
    rho = c(1700, 1700), # layer density (kg/m3)
    c = c(1350, 1350), # layer specific heat (J/(kg*degK)
    albedo = c(0.40, NA), # surface albedo (dimensionless)
    emissivity = c(0.935, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Bare Dry Soil #2 (med-low heat)
    layer = c("surface","subgrade"), 
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(0.725, 0.725), # layer thermal conductivity (W/(m*degK))  
    rho = c(1850, 1850), # layer density (kg/m3)
    c = c(1075, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.45, NA), # surface albedo (dimensionless)
    emissivity = c(0.9525, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Bare Dry Soil #3 (low heat)
    layer = c("surface", "subgrade"),
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(0.3, 0.3), # layer thermal conductivity (W/(m*degK))  
    rho = c(2000, 2000), # layer density (kg/m3)
    c = c(800, 800), # layer specific heat (J/(kg*degK)
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
    thickness = c(0.20, 0.19, 1.11), # layer thickness (m)
    k = c(2.25, 2.20, 1.15), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2300, 1700), # layer density (kg/m3)
    c = c(1000, 950, 1350), # layer specific heat (J/(kg*degK)
    albedo = c(0.30, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # PCC (med-low heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.25, 0.245, 1.005), # layer thickness (m)
    k = c(1.375, 1.80, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2375, 2350, 1850), # layer density (kg/m3)
    c = c(900, 875, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.365, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.945, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # PCC (low heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.30, 0.30, 0.6), # layer thickness (m)
    k = c(0.50, 1.4, 0.30), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2400, 2000), # layer density (kg/m3)
    c = c(800, 800, 800), # layer specific heat (J/(kg*degK)
    albedo = c(0.43, NA, NA), # surface albedo (dimensionless)
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
    thickness = c(0.17, 0.19, 1.14), # layer thickness (m)
    k = c(1.75, 2.20, 1.15), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2225, 2300, 1700), # layer density (kg/m3)
    c = c(1400, 950, 1350), # layer specific heat (J/(kg*degK)
    albedo = c(0.17, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt (low-med heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.235, 0.245, 1.02), # layer thickness (m)
    k = c(1.125, 1.80, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2388, 2350, 1850), # layer density (kg/m3)
    c = c(1100, 875, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.23, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.91, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt (low heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.3, 0.3, 0.9), # layer thickness (m)
    k = c(0.50, 1.4, 0.30), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2550, 2400, 2000), # layer density (kg/m3)
    c = c(800, 800, 800), # layer specific heat (J/(kg*degK)
    albedo = c(0.29, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA), # emissivity (dimensionless)
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
    thickness = c(0.08, 0.17, 0.19, 1.06), # layer thickness (m)
    k = c(2.25, 1.75, 2.20, 1.15), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2225, 2300, 1700), # layer density (kg/m3)
    c = c(1000, 1400, 950, 1350), # layer specific heat (J/(kg*degK)
    albedo = c(0.325, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.94, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # whitetopped asphalt (low-med heat)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.09, 0.235, 0.245, 0.93), # layer thickness (m)
    k = c(1.375, 1.125, 1.80, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2375, 2388, 2350, 1850), # layer density (kg/m3)
    c = c(900, 1100, 875, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.3875, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # whitetopped asphalt (low heat)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.1, 0.3, 0.3, 0.8), # layer thickness (m)
    k = c(0.50, 0.50, 1.4, 0.30), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2550, 2400, 2000), # layer density (kg/m3)
    c = c(800, 800, 800, 800), # layer specific heat (J/(kg*degK)
    albedo = c(0.45, NA, NA, NA), # surface albedo (dimensionless)
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
    thickness = c(0.15, 0.20, 0.19, 0.96), # layer thickness (m)
    k = c(1.75, 2.25, 2.20, 1.15), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2225, 2350, 2300, 1700), # layer density (kg/m3)
    c = c(1400, 1000, 950, 1350), # layer specific heat (J/(kg*degK)
    albedo = c(0.17, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.89, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt overlay on PCC (med-low heat)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.20, 0.25, 0.245, 0.805), # layer thickness (m)
    k = c(1.125, 1.375, 1.80, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2388, 2375, 2350, 1850), # layer density (kg/m3)
    c = c(1100, 900, 875, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.23, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.91, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt overlay on PCC (low heat)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.20, 0.30, 0.30, 0.7), # layer thickness (m)
    k = c(0.50, 0.50, 1.4, 0.30), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2550, 2400, 2400, 2000), # layer density (kg/m3)
    c = c(800, 800, 800, 800), # layer specific heat (J/(kg*degK)
    albedo = c(0.29, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA, NA), # emissivity (dimensionless)
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
