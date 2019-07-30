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
  data.table( # Bare Dry Soil #2 (med heat)
    layer = c("surface","subgrade"), 
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(1.0, 1.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(1600, 1600), # layer density (kg/m3)
    c = c(1500, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.4, NA), # surface albedo (dimensionless)
    emissivity = c(0.97, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Bare Dry Soil #2 (med-low heat)
    layer = c("surface","subgrade"), 
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(2.0, 2.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(1600, 1600), # layer density (kg/m3)
    c = c(1700, 1700), # layer specific heat (J/(kg*degK)
    albedo = c(0.5, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # Bare Dry Soil #3 (low heat)
    layer = c("surface", "subgrade"),
    thickness = c(0.75, 0.75), # layer thickness (m)
    k = c(3.0, 3.0), # layer thermal conductivity (W/(m*degK))  
    rho = c(1700, 1700), # layer density (kg/m3)
    c = c(1900, 1900), # layer specific heat (J/(kg*degK)
    albedo = c(0.6, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.bg) <- c(#"Bare Dry Soil #1",
                              #"Bare Dry Soil #2",
                              "Bare Dry Soil Low A",
                              "Bare Dry Soil Med A",
                              "Bare Dry Soil High A") 

# CONCRETE

layer.profiles.c <- list(
  data.table( # PCC (med heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.20, 0.20, 1.1), # layer thickness (m)
    k = c(1.2, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2250, 2400, 1850), # layer density (kg/m3)
    c = c(850, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.96, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # PCC (med-low heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.20, 0.20, 1.1), # layer thickness (m)
    k = c(1.5, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2250, 2400, 1850), # layer density (kg/m3)
    c = c(900, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.325, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # PCC (low heat)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.20, 0.20, 1.1), # layer thickness (m)
    k = c(1.8, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2250, 2400, 1850), # layer density (kg/m3)
    c = c(950, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.4, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.c) <- c(#"Portland Cement Concrete #1",
                             #"Portland Cement Concrete #2",
                             "Portland Cement Concrete Low A",
                             "Portland Cement Concrete Med A",
                             "Portland Cement Concrete High A") 

# ASPHALT

layer.profiles.a <- list(
  data.table( # asphalt (thin)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.14, 0.2, 1.16), # layer thickness (m)
    k = c(1.2, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2350, 1850), # layer density (kg/m3)
    c = c(800, 875, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt (med)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.14, 0.2, 1.16), # layer thickness (m)
    k = c(1.7, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2350, 1850), # layer density (kg/m3)
    c = c(900, 875, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.2, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt (thick)
    layer = c("surface", "base", "subgrade"),
    thickness = c(0.14, 0.2, 1.16), # layer thickness (m)
    k = c(2.2, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2350, 1850), # layer density (kg/m3)
    c = c(1000, 875, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.85, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.a) <- c(#"Asphalt #1",
                             #"Asphalt #2", 
                             "Asphalt Low A",
                             "Asphalt Med A",
                             "Asphalt High A")


# COMPOSITE #1 (Whitetopped)
layer.profiles.wa <- list(
  data.table( # whitetopped asphalt (med heat)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.1, 0.14, 0.2, 1.06), # layer thickness (m)
    k = c(1.2, 1.2, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2250, 2350, 2400, 1850), # layer density (kg/m3)
    c = c(850, 800, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.96, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # whitetopped asphalt (low-med heat)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.1, 0.14, 0.2, 1.06), # layer thickness (m)
    k = c(1.5, 1.7, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2250, 2350, 2400, 1850), # layer density (kg/m3)
    c = c(900, 900, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.325, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.93, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # whitetopped asphalt (low heat)
    layer = c("surface", "asphalt", "base", "subgrade"),
    thickness = c(0.1, 0.14, 0.2, 1.06), # layer thickness (m)
    k = c(1.8, 2.2, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2250, 2350, 2400, 1850), # layer density (kg/m3)
    c = c(950, 1000, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.4, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.90, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.wa) <- c(#"Whitetopped Asphalt #1", 
                              #"Whitetopped Asphalt #2",
                              "Whitetopped Asphalt Low A",
                              "Whitetopped Asphalt Med A",
                              "Whitetopped Asphalt High A") 


# COMPOSITE #2 (Asphalt Overlay on PCC)

layer.profiles.oc <- list(
  data.table( # asphalt overlay on PCC (med heat)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.13, 0.15, 0.2, 1.02), # layer thickness (m)
    k = c(1.2, 1.2, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2250, 2400, 1850), # layer density (kg/m3)
    c = c(800, 850, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.15, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.95, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt overlay on PCC (med-low heat)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.13, 0.15, 0.2, 1.02), # layer thickness (m)
    k = c(1.7, 1.5, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2250, 2400, 1850), # layer density (kg/m3)
    c = c(900, 900, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.2, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.9, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  ),
  data.table( # asphalt overlay on PCC (low heat)
    layer = c("surface", "PCC", "base", "subgrade"),
    thickness = c(0.13, 0.15, 0.2, 1.02), # layer thickness (m)
    k = c(2.2, 1.8, 1.5, 2.0), # layer thermal conductivity (W/(m*degK)) 
    rho = c(2350, 2250, 2400, 1850), # layer density (kg/m3)
    c = c(1000, 950, 800, 1500), # layer specific heat (J/(kg*degK)
    albedo = c(0.25, NA, NA, NA), # surface albedo (dimensionless)
    emissivity = c(0.85, NA, NA, NA), # emissivity (dimensionless)
    R.c.top = c(0, 0, 0, 0) # thermal contact resistance at top boundary of layer (dimensionless)
  )
)

# define layer profile names 
names(layer.profiles.oc) <- c(#"Asphalt Overlay on PCC #1",
                              #"Asphalt Overlay on PCC #2",
                              "Asphalt Overlay on PCC Low A",
                              "Asphalt Overlay on PCC Med A",
                              "Asphalt Overlay on PCC High A") 


# SAVE
saveRDS(layer.profiles.bg, here("data/outputs/layer-profiles-BG.rds"))
saveRDS(layer.profiles.c, here("data/outputs/layer-profiles-C.rds"))
saveRDS(layer.profiles.a, here("data/outputs/layer-profiles-A.rds"))
saveRDS(layer.profiles.wa, here("data/outputs/layer-profiles-WA.rds"))
saveRDS(layer.profiles.oc, here("data/outputs/layer-profiles-OC.rds"))
