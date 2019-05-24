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
  data.table( # Bare Dry Soil #2 (high thermal inertia)
    layer = c("surface","subgrade"), 
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(1.8, 1.8), # layer thermal conductivity (W/(m*degK))  
    rho = c(2000, 2000), # layer density (kg/m3)
    c = c(1900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.40, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Bare Dry Soil #3 (low thermal inertia)
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
                              "Bare Dry Soil High TI",
                              "Bare Dry Soil Low TI") 

# CONCRETE

layer.profiles.c <- list(
  data.table( # PCC (high thermal inertia)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.20, 0.20, 1.1), # layer thickness (m)
    k = c(2.2, 3.0, 1.8), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2400, 2000), # layer density (kg/m3)
    c = c(1050, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.3, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # PCC (low thermal inertia)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.20, 0.20, 1.1), # layer thickness (m)
    k = c(1.2, 1.5, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2100, 2400, 1500), # layer density (kg/m3)
    c = c(840, 800, 1100), # layer specific heat (J/(kg*degK)
    albedo = c(0.3, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.96, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.c) <- c("Portland Cement Concrete High TI",
                             "Portland Cement Concrete Low TI") 

# ASPHALT

layer.profiles.a <- list(
  data.table( # asphalt (high thermal inertia))
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.14, 0.2, 1.16), # layer thickness (m)
    k = c(2.2, 3.0, 1.8), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2500, 2300, 2000), # layer density (kg/m3)
    c = c(1400, 950, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.9, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt (low thermal inertia)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.14, 0.2, 1.16), # layer thickness (m)
    k = c(1.2, 1.5, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2200, 2400, 1500), # layer density (kg/m3)
    c = c(850, 800, 1100), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.9, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.a) <- c("Asphalt High TI",
                             "Asphalt Low TI")


# COMPOSITE #1 (Whitetopped)
layer.profiles.wa <- list(
  data.table( # whitetopped asphalt (med heat)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.1, 0.14, 0.2, 1.06), # layer thickness (m)
    k = c(2.2, 2.2, 3.0, 1.8), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2400, 2500, 2400, 2000), # layer density (kg/m3)
    c = c(1050, 1400, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.3, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # whitetopped asphalt (low heat)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.1, 0.14, 0.2, 1.06), # layer thickness (m)
    k = c(1.2, 1.2, 1.5, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2100, 2200, 2400, 1500), # layer density (kg/m3)
    c = c(840, 850, 800, 1100), # layer specific heat (J/(kg*degK)
    albedo = c(0.3, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.wa) <- c(#"Whitetopped Asphalt #1", 
                              #"Whitetopped Asphalt #2",
                              "Whitetopped Asphalt High TI",
                              "Whitetopped Asphalt Low TI") 


# COMPOSITE #2 (Asphalt Overlay on PCC)

layer.profiles.oc <- list(
  data.table( # asphalt overlay on PCC (high thermal inertia)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.13, 0.15, 0.2, 1.02), # layer thickness (m)
    k = c(2.2, 2.2, 3.0, 1.8), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2500, 2400, 2400, 2000), # layer density (kg/m3)
    c = c(1400, 1000, 800, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.9, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt overlay on PCC (low thermal inertia)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.13, 0.15, 0.2, 1.02), # layer thickness (m)
    k = c(1.2, 1.2, 1.5, 1.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2200, 2100, 2400, 1500), # layer density (kg/m3)
    c = c(850, 800, 800, 1100), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.9, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.oc) <- c("Asphalt Overlay on PCC High TI",
                              "Asphalt Overlay on PCC Low TI") 


# SAVE
saveRDS(layer.profiles.bg, here("data/outputs/layer-profiles-BG.rds"))
saveRDS(layer.profiles.c, here("data/outputs/layer-profiles-C.rds"))
saveRDS(layer.profiles.a, here("data/outputs/layer-profiles-A.rds"))
saveRDS(layer.profiles.wa, here("data/outputs/layer-profiles-WA.rds"))
saveRDS(layer.profiles.oc, here("data/outputs/layer-profiles-OC.rds"))
