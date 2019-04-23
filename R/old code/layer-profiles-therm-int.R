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
    k = c(0.3, 0.3), # layer thermal conductivity (W/(m*degK))  
    rho = c(2000, 2000), # layer density (kg/m3)
    c = c(900, 900), # layer specific heat (J/(kg*degK)
    albedo = c(0.50, NA), # surface albedo (dimensionless)
    emissivity = c(0.97, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.bg) <- c("Bare Dry Soil #1") 

# CONCRETE

layer.profiles.c <- list(
  data.table( # PCC (low TI)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.25, 0.245, 1.005), # layer thickness (m)
    k = c(0.5, 1.60, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2300, 1850), # layer density (kg/m3)
    c = c(800, 875, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.35, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.945, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # PCC (med TI)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.25, 0.245, 1.005), # layer thickness (m)
    k = c(1.5, 1.80, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2375, 2370, 1850), # layer density (kg/m3)
    c = c(1000, 900, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.35, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.945, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # PCC (high TI)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.25, 0.245, 1.005), # layer thickness (m)
    k = c(2.5, 2.0, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2350, 1850), # layer density (kg/m3)
    c = c(1200, 1000, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.35, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.945, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.c) <- c(#"Portland Cement Concrete #1",
                             #"Portland Cement Concrete #2",
                             "Portland Cement Concrete (low TI)",
                             "Portland Cement Concrete (med TI)",
                             "Portland Cement Concrete (high TI)") 

# ASPHALT

layer.profiles.a <- list(
  data.table( # asphalt (low TI)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.235, 0.245, 1.02), # layer thickness (m)
    k = c(0.5, 1.20, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2300, 2150, 1850), # layer density (kg/m3)
    c = c(800, 875, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.2, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.91, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt (med TI)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.235, 0.245, 1.02), # layer thickness (m)
    k = c(1.25, 1.50, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2300, 1850), # layer density (kg/m3)
    c = c(1150, 1000, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.2, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.91, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt (high TI)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.235, 0.245, 1.02), # layer thickness (m)
    k = c(2.0, 1.85, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2500, 2350, 1850), # layer density (kg/m3)
    c = c(1500, 1250, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.2, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.91, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.a) <- c("Asphalt (low TI)",
                             "Asphalt (med TI)",
                             "Asphalt (high TI)")


# COMPOSITE #1 (Whitetopped)
layer.profiles.wa <- list(
  data.table( # whitetopped asphalt (low TI)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.09, 0.235, 0.245, 0.93), # layer thickness (m)
    k = c(0.5, 0.75, 1.25, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2300, 2250, 2200, 1850), # layer density (kg/m3)
    c = c(800, 800, 875, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.35, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # whitetopped asphalt (med TI)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.09, 0.235, 0.245, 0.93), # layer thickness (m)
    k = c(1.5, 1.25, 1.0, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2300, 2275, 1850), # layer density (kg/m3)
    c = c(900, 1100, 975, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.35, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # whitetopped asphalt (high TI)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.09, 0.235, 0.245, 0.93), # layer thickness (m)
    k = c(2.5, 2.0, 1.80, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2350, 2300, 1850), # layer density (kg/m3)
    c = c(1100, 1200, 1050, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.35, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.wa) <- c("Whitetopped Asphalt (low TI)",
                              "Whitetopped Asphalt (med TI)",
                              "Whitetopped Asphalt (high TI)") 


# COMPOSITE #2 (Asphalt Overlay on PCC)

layer.profiles.oc <- list(
  data.table( # asphalt overlay on PCC (low TI)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.20, 0.25, 0.245, 0.805), # layer thickness (m)
    k = c(0.5, 0.75, 1.20, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2250, 2300, 2200, 1850), # layer density (kg/m3)
    c = c(800, 900, 1000, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.2, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.91, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt overlay on PCC (med TI)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.20, 0.25, 0.245, 0.805), # layer thickness (m)
    k = c(1.25, 1.375, 1.50, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2350, 2300, 1850), # layer density (kg/m3)
    c = c(1100, 1000, 950, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.2, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.91, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt overlay on PCC (high TI)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.20, 0.25, 0.245, 0.805), # layer thickness (m)
    k = c(2.0, 1.8, 1.50, 0.725), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2550, 2475, 2350, 1850), # layer density (kg/m3)
    c = c(1400, 1200, 1000, 1075), # layer specific heat (J/(kg*degK)
    albedo = c(0.2, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.91, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.oc) <- c("Asphalt Overlay on PCC (low TI)",
                              "Asphalt Overlay on PCC (med TI)",
                              "Asphalt Overlay on PCC (high TI)") 


# SAVE
saveRDS(layer.profiles.bg, here("data/outputs/layer-profiles-BG.rds"))
saveRDS(layer.profiles.c, here("data/outputs/layer-profiles-C.rds"))
saveRDS(layer.profiles.a, here("data/outputs/layer-profiles-A.rds"))
saveRDS(layer.profiles.wa, here("data/outputs/layer-profiles-WA.rds"))
saveRDS(layer.profiles.oc, here("data/outputs/layer-profiles-OC.rds"))
