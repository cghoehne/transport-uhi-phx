# clear space and allocate memory
gc()
memory.limit(size = 50000) 
options(scipen = 999) # prevent scientific notation when printing

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues+
if (!require("checkpoint")){f
  install.packages("checkpoint")
  library(checkpoint)
}

# load all other dependant packages from the local repo
.libPaths(paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1"))
library(zoo)
library(lubridate)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(stringr)
library(sf)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(raster)
library(data.table)
library(here)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # archive date for all used packages (besides checkpoint itself!)
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

library(ggplot2) # load again

# load windows fonts and store as string
windowsFonts(Century=windowsFont("TT Century Gothic"))
windowsFonts(Times=windowsFont("TT Times New Roman"))
my.font <- "Century"

# create RMSE function
RMSE = function(m, o){sqrt(mean((m - o)^2, na.rm = T))}

#######################################
# MODELED VS OBSERVED VALIDATION PLOT #
#######################################

# IMPORT VALIDATION MODEL DATA
folder <- here("data/outputs/run_metadata_20190611_105020") # validation runs 
# run_metadata_20190610_113813
all.model.runs <- readRDS(paste0(folder, "/stats_all_model_runs.rds"))

all.model.runs[, RMSE(Modeled, Observed), by = pave.name][order(V1)]
all.model.runs[, mean(p.err), by = pave.name][order(V1)]

all.model.runs[, RMSE(Modeled, Observed), by = pave.name][order(V1)]
all.model.runs[, mean(p.err), by = c("pave.name", "day.sea")][order(V1)]

all.model.runs[, RMSE(Modeled, Observed), by = day.sea][order(V1)]


# for validation only, drop unrealistic/bad predictors and high volume pavements as they are not representative
model.names <- sort(unique(all.model.runs$pave.name))
model.runs <- all.model.runs#[p.err <= 0.30 & is.finite(p.err) & RMSE(Modeled, Observed) <= 10,] # remove poor performers or NAs if there are any 
#model.runs <- model.runs[!(pave.name %in% model.names[7:12])]


min.x <- 0  # round(min(valids[,.(T.degC, T.degC.sat)] - 5), - 1)  
max.x <- round(max(model.runs[,.(Modeled,Observed)], na.rm = T) + 5, - 1)  #valids.long[, temp]
min.y <- min.x
max.y <- max.x

# create different legend charateristics for plotting
# create different legend charateristics for plotting
m.o.names <- levels(model.runs[, day.sea])
m.o.shp <- rep(c(16, 18), 4) # circle = day, diamond = night
m.o.siz <- rep(c(2.5, 2.8), 4) # make diamond a little bigger
m.o.col <- c("#1E964E", "#1E964E", "#CC0E2A", "#CC0E2A", "#602E00", "#602E00", "#0062DB", "#0062DB") 
names(m.o.shp) <- m.o.names
names(m.o.siz) <- m.o.names
names(m.o.col) <- m.o.names

# create new names 
model.runs[, new.name := pave.name]

#model.runs[pave.name %in% model.names[7:9], new.name := "Bare Ground (Desert Soil)"]
#model.runs[pave.name %in% model.names[1:3], new.name := "Asphalt Pavements"] 
#model.runs[pave.name %in% model.names[10:12], new.name := "Concrete Pavements"]
#model.runs[pave.name %in% model.names[c(4:6,13:15)], new.name := "Composite Concrete/Asphalt Pavements"]

model.runs[grepl("bare", pave.name, ignore.case = T) == T, new.name := "Bare Ground (Desert Soil)"]
model.runs[grepl("asphalt", pave.name, ignore.case = T) == T, new.name := "Asphalt Pavements"]
model.runs[grepl("concrete", pave.name, ignore.case = T) == T, new.name := "Concrete Pavements"]
model.runs[grepl("whitetop", pave.name, ignore.case = T) == T |
           grepl("overlay", pave.name, ignore.case = T) == T, new.name := "Composite Concrete/Asphalt Pavements"]


# create new ordered titles with a:d for facet titles
p.names <- unique(model.runs[, new.name])
model.runs[new.name == p.names[1], new.name.t := paste("(a)", p.names[1])]
model.runs[new.name == p.names[2], new.name.t := paste("(b)", p.names[2])]
model.runs[new.name == p.names[3], new.name.t := paste("(c)", p.names[3])]
model.runs[new.name == p.names[4], new.name.t := paste("(d)", p.names[4])]
model.runs[, new.name.t := as.factor(new.name.t)]

# RMSE & MAPE
model.runs[new.name == p.names[1], RMSE.batch := RMSE(Modeled, Observed)]
model.runs[new.name == p.names[2], RMSE.batch := RMSE(Modeled, Observed)]
model.runs[new.name == p.names[3], RMSE.batch := RMSE(Modeled, Observed)]
model.runs[new.name == p.names[4], RMSE.batch := RMSE(Modeled, Observed)]
model.runs[, RMSE.batch := paste("RMSE =", format(signif(RMSE.batch, 3), nsmall = 1))]

model.runs[new.name == p.names[1], MAPE.batch := mean(p.err, na.rm = T)]
model.runs[new.name == p.names[2], MAPE.batch := mean(p.err, na.rm = T)]
model.runs[new.name == p.names[3], MAPE.batch := mean(p.err, na.rm = T)]
model.runs[new.name == p.names[4], MAPE.batch := mean(p.err, na.rm = T)]
model.runs[, MAPE.batch := paste0("MAPE = ", format(signif(MAPE.batch, 3) * 100, nsmall = 1), "%")]

# create plot
my.plot.f <- (ggplot(data = model.runs) 
            
            # custom border
            + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
            + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
            
            # points for modeled vs observed + ref line
            + geom_point(aes(y = Modeled, x = Observed, color = day.sea, shape = day.sea, size = day.sea))
            + geom_text(aes(y = max.y /  6, x = max.x * 0.85, label = MAPE.batch), family = my.font, size = 3.5)
            + geom_text(aes(y = max.y / 12, x = max.x * 0.85, label = RMSE.batch), family = my.font, size = 3.5)
            + geom_abline(intercept = 0, slope = 1) # line of equality
            
            # plot/axis titles & second axis for solar rad units
            + labs(x = "Observed Surface Temperature (deg C)", y = "Modeled Surface Temperature (deg C)")
            + scale_color_manual(name = "", values = m.o.col)
            + scale_size_manual(name = "", values = m.o.siz)
            + scale_shape_manual(name = "", values = m.o.shp)
            + facet_wrap(~new.name.t)
            
            # scales
            + scale_x_continuous(expand = c(0,0), limits = c(min.x,max.x), breaks = seq(min.x,max.x,10))
            + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,10))
            
            # theme and formatting
            + theme_minimal()
            + theme(text = element_text(family = my.font, size = 12, colour = "black"), 
                    axis.text = element_text(colour = "black"),
                    plot.margin = margin(t = 5, r = 10, b = 55, l = 20, unit = "pt"),
                    panel.spacing.x = unit(10, "mm"),
                    panel.spacing.y = unit(7, "mm"),
                    axis.ticks = element_line(color = "grey80", size = 0.25),
                    axis.text.x  = element_text(size = 11, vjust = -0.5),
                    axis.title.x = element_text(size = 12, margin = margin(t = 13, r = 0, b = 0, l = 0)),
                    axis.text.y  = element_text(size = 11),
                    axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 10, b = 0, l = 0)),
                    strip.text.x = element_text(size = 12, hjust = 0.45, vjust = 1), #, face = "bold"
                    legend.position = c(0.45, -0.175),
                    legend.text = element_text(size = 12),
                    legend.direction ="horizontal",
                    legend.background = element_blank())
            )

# save plot
ggsave("figures/modeled-observed.png", my.plot.f, 
       device = "png", scale = 1.5, width = 7, height = 5, dpi = 300, units = "in")

#######################################
# HEAT FLUX PLOT (NON-VALIDATION RUNS #
#######################################

# import data
#bg.surface.data <- readRDS(here("data/outputs/run_metadata_20190520_171637/all_pave_surface_data.rds"))[batch.name == "Bare Ground / Desert Soil",]
#pave.surface.data <- readRDS(here("data/outputs/run_metadata_20190526_185113_varied_thick/all_pave_surface_data.rds")) # surface temporal data by run for pavements
#all.surface.data <- rbind(bg.surface.data, pave.surface.data) # merge bare ground and pavement simulations seperatley (bare ground is rarely rerun)

all.surface.data <- readRDS(here("data/outputs/run_metadata_20190625_110129/all_pave_surface_data.rds"))
#all.surface.data.7d <- readRDS(here("data/outputs/run_metadata_20190520_171637/all_pave_surface_data.rds")) # old 7 day runs (will replace)
#all.surface.data.th <- readRDS(here("data/outputs/run_metadata_20190526_185113_varied_thick/all_pave_surface_data.rds"))
#all.surface.data.ti <- readRDS(here("data/outputs/run_metadata_20190524_112541_varied_TI/all_pave_surface_data.rds"))
#all.surface.data.al <- readRDS(here("data/outputs/run_metadata_20190526_182759_varied_albedo/all_pave_surface_data.rds"))
#all.surface.data <- rbind(all.surface.data.th, all.surface.data.ti, all.surface.data.al, all.surface.data.7d)

all.surface.data <- all.surface.data[!(pave.name %in% c("Bare Dry Soil #1"))]

# force to static date for date.time for easier manipulation in ggplot, will ignore date
# NOTE: all summarized surface data is filtered previously to only last day of data
all.surface.data[, date.time := as.POSIXct("2019-01-01 00:00:00 MST") + hours(hrs) + minutes(mins) + seconds(secs)] 

# flux comparisons by type
all.surface.data[, mean(q.rad + q.cnv), by = c("pave.name", "batch.name", "albedo")][order(V1)]

# new names
all.surface.data[, new.name := batch.name]
all.surface.data[batch.name == "Asphalt Pavements", new.name := "Asphalt Surface Course"]
all.surface.data[batch.name == "Asphalt Overlays on PCC Pavements", new.name := "Asphalt Surface Course"]
all.surface.data[batch.name == "Concrete Pavements", new.name := "Concrete Surface Course"]
all.surface.data[batch.name == "Whitetopped Asphalt Pavements", new.name := "Concrete Surface Course"]
all.surface.data[batch.name == "Bare Ground / Desert Soil", new.name := "Bare Ground (Desert Soil)"]

# calc flux vars
all.surface.data[, inc.sol := ((1 - albedo) * SVF * solar)]
all.surface.data[, ref.sol := albedo * SVF * solar]
all.surface.data[, net.flux := -inc.sol + q.rad + q.cnv]

# aggregate for plotting
surface.data.a <- all.surface.data[, .(out.flux = mean(q.rad + q.cnv),
                                       inc.sol = mean(inc.sol),
                                       net.flux = mean(net.flux),
                                       ref.sol = mean(ref.sol)),
                                   by = c("date.time", "new.name", "season", "SVF")]  
surface.data.a <- surface.data.a[SVF == 1.0,]


# plot min/maxes
min.x.flux <- min(surface.data.a$date.time, na.rm = T) 
max.x.flux <- ceiling_date(max(surface.data.a[, date.time]), unit = "hours")
min.y.flux <- 0 #round(min(surface.data.a[, out.flux] - 5), - 1) # round down to nearest multiple of 10
max.y.flux <- round(max(surface.data.a[, out.flux] + 5), - 1) # round up to nearest multiple of 10

# adjust y to by multiple of mult
min.y.flux <- ifelse(min.y.flux %% 20 == 0, min.y.flux, min.y.flux - 10)
max.y.flux <- ifelse(max.y.flux %% 20 == 0, max.y.flux, max.y.flux + 10)

# create different legend charateristics for plotting
p.names <- unique(surface.data.a[, new.name])
#surface.data.a[, new.name.f := factor(new.name, levels = p.names)]
surface.data.a[, new.name.f := factor(new.name, levels = c(p.names[1], p.names[2], p.names[3]))]
p.col <- c("#0C120C", "#918D77", "#C1912A")  
#p.col <- c("#0C120C", "#0C120C", "#918D77", "#918D77", "#C1912A") 
p.lty <- c("solid",  "longdash", "twodash") 
#p.lty <- c("solid", "longdash", "solid", "longdash", "twodash") 
names(p.col) <- levels(surface.data.a[,new.name.f])
names(p.lty) <- levels(surface.data.a[,new.name.f])


# create better labels for season factor
#setattr(surface.data.a$season,"levels", c("(a) Spring", "(b) Summer", "(c) Fall", "(d) Winter"))
setattr(surface.data.a$season,"levels", c("(a) Spring", "(a) Summer", "(c) Fall", "(b) Winter"))

# create plot
p.flux.a <- (ggplot(data = surface.data.a[season %in% c("(a) Summer", "(b) Winter")])   #
             
             # custom border
             + geom_segment(aes(x = min.x.flux, y = min.y.flux, xend = max.x.flux, yend = min.y.flux))   # x border (x,y) (xend,yend)
             + geom_segment(aes(x = min.x.flux, y = min.y.flux, xend = min.x.flux, yend = max.y.flux))  # y border (x,y) (xend,yend)
             
             # line + point based on named factor of flux
             + geom_line(aes(y = out.flux, x = date.time, color = new.name.f, linetype = new.name.f), size = 1)
             #+ geom_point(aes(y = out.flux, x = date.time, color = new.name.f), size = 2, data = surface.data.a[mins %in% c(0) & secs == 0,])

             # plot/axis titles & second axis for solar rad units
             + labs(x = "Time of Day", y = bquote('Mean Outgoing Sensible Heat Flux ('*W/m^2*')'))
             + scale_color_manual(name = "", values = p.col, guide = guide_legend(reverse = F))
             + scale_linetype_manual(name = "", values = p.lty, guide = guide_legend(reverse = F))
             #+ scale_shape_manual(name = "", values = p.shp)
             + facet_wrap(~season)
             
             # scales
             + scale_x_datetime(expand = c(0,0), limits = c(min.x.flux, max.x.flux), date_breaks = "3 hours", date_labels = "%H") #
             + scale_y_continuous(expand = c(0,0), limits = c(min.y.flux, max.y.flux), breaks = seq(min.y.flux, max.y.flux, 40))
             #+ guides(col = guide_legend(ncol = 2)) # two rows in legend
             
             # theme and formatting
             + theme_minimal()
             + theme(text = element_text(family = my.font, size = 12, colour = "black"), 
                     axis.text = element_text(colour = "black"),
                     plot.margin = margin(t = 2, r = 8, b = 70, l = 2, unit = "pt"),
                     panel.spacing.x = unit(7, "mm"),
                     panel.spacing.y = unit(4, "mm"),
                     axis.text.x = element_text(vjust = -1),
                     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)),
                     axis.ticks = element_line(color = "grey80", size = 0.28),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 10.5),
                     strip.text.x = element_text(size = 13, hjust = 0.5, vjust = 1), #face = "bold"
                     legend.position = c(0.525, -0.35),
                     legend.key.width = unit(30, "mm"),
                     legend.text = element_text(size = 10),
                     legend.spacing.y = unit(1, "mm"),
                     legend.direction ="vertical")
)

# save plot
ggsave("figures/heat-flux-diff.png", p.flux.a, 
       device = "png", scale = 1, width = 6, height = 4.5, dpi = 300, units = "in") 

#########################
# THERMAL INERTIA PLOTS #
#########################

# import runs for thermal variying thermal inertia
all.surface.data <- readRDS(here("data/outputs/run_metadata_20190524_112541_varied_TI/all_pave_surface_data.rds"))
all.model.runs <- readRDS(here("data/outputs/run_metadata_20190524_112541_varied_TI/stats_all_model_runs.rds"))

# calc the thermal inertia values for layers
all.model.runs[, L1.TI := sqrt(L1.k * L1.rho * L1.c)]   # UNITS: (W/(m*degK)) * (kg/m3) * (J/(kg*degK) = J/(m2*degK*(s^0.5))
all.model.runs[, L2.TI := sqrt(L2.k * L2.rho * L2.c)]   # UNITS: (W/(m*degK)) * (kg/m3) * (J/(kg*degK) = J/(m2*degK*(s^0.5))
#all.model.runs[, L3.TI := sqrt(L3.k * L3.rho * L3.c)]   # UNITS: (W/(m*degK)) * (kg/m3) * (J/(kg*degK) = J/(m2*degK*(s^0.5))
#all.model.runs[, L4.TI := sqrt(L4.k * L4.rho * L4.c)]   # UNITS: (W/(m*degK)) * (kg/m3) * (J/(kg*degK) = J/(m2*degK*(s^0.5))

# add thermal inertia values for 1st and 2nd layer to surface data
all.surface.data <- merge(all.surface.data, unique(all.model.runs[, .(L1.TI, L2.TI), by = pave.name]), by = "pave.name")

# force to static date for date.time for easier manipulation in ggplot, will ignore date
# NOTE: all summarized surface data is filtered previously to only last day of data
all.surface.data[, date.time := as.POSIXct("2019-01-01 00:00:00 MST") + hours(hrs) + minutes(mins) + seconds(secs)] 

# flux comparisons by type
all.surface.data[, mean(q.rad + q.cnv), by = c("pave.name", "batch.name", "albedo")][order(V1)]

# calc flux vars
all.surface.data[, inc.sol := ((1 - albedo) * SVF * solar)]
all.surface.data[, ref.sol := albedo * SVF * solar]
all.surface.data[, net.flux := -inc.sol + q.rad + q.cnv]

surface.data.b <- all.surface.data[, .(batch.name = batch.name,
                                       L1.TI = signif(mean(L1.TI, na.rm = T), 3), 
                                       L2.TI = signif(mean(L2.TI, na.rm = T), 3),
                                       out.flux = mean(q.rad + q.cnv),
                                       inc.sol = mean(inc.sol),
                                       net.flux = mean(net.flux),
                                       ref.sol = mean(ref.sol)),
                                   by = c("date.time", "pave.name", "SVF")]  
surface.data.b <- unique(surface.data.b[SVF == 1.0,])

# new batch names as factor
surface.data.b[batch.name == "Asphalt Pavements", batch.name := "(a) Asphalt Only"]
surface.data.b[batch.name == "Asphalt Overlays on PCC Pavements", batch.name := "(b) Asphalt Overlays on PCC "]
surface.data.b[batch.name == "Concrete Pavements", batch.name := "(c) Concrete Only"]
surface.data.b[batch.name == "Whitetopped Asphalt Pavements", batch.name := "(d) Whitetopped Asphalt"]
b.names <- unique(surface.data.b[,batch.name])
surface.data.b[, batch.name.f := factor(batch.name, levels = c(b.names[1], b.names[4], b.names[2], b.names[3]))]

# thermal inertia factor
surface.data.b[, group := substr(pave.name, nchar(pave.name) - 6, nchar(pave.name))]
surface.data.b[group == "High TI", group := "High Thermal Inertia"]
surface.data.b[group == " Low TI", group := "Low Thermal Inertia"]

# thermal inertia label as character formatted
surface.data.b[, L1.TI.l := as.character(L1.TI)]
surface.data.b[, L2.TI.l := as.character(L2.TI)]
surface.data.b[group == "High Thermal Inertia", L1.TI.l := paste("L1 TI (High) =", format(L1.TI, nsmall = 0))]
surface.data.b[group == "High Thermal Inertia", L2.TI.l := paste("L2 TI (High) =", format(L2.TI, nsmall = 0))]
surface.data.b[group == "Low Thermal Inertia", L1.TI.l := paste("L1 TI (Low) =", format(L1.TI, nsmall = 0))]
surface.data.b[group == "Low Thermal Inertia", L2.TI.l := paste("L2 TI (Low) =", format(L2.TI, nsmall = 0))]

# create different legend charateristics for plotting
p.group <- unique(surface.data.b[, group])
surface.data.b[, group.f := factor(group, levels = p.group)]
p.col <- c("#10316B", "#BF1C3D")  
p.lty <- c("twodash", "solid") 
names(p.col) <- levels(surface.data.b[,group.f])
names(p.lty) <- levels(surface.data.b[,group.f])

# plot min/maxes
min.x.flux <- min(surface.data.b$date.time, na.rm = T) 
max.x.flux <- ceiling_date(max(surface.data.b[, date.time]), unit = "hours")
min.y.flux <- 0 #round(min(surface.data.b[, out.flux] - 5), - 1) # round down to nearest multiple of 10
max.y.flux <- round(max(surface.data.b[, out.flux] + 5), - 1) # round up to nearest multiple of 10

# adjust y to by multiple of mult
min.y.flux <- ifelse(min.y.flux %% 20 == 0, min.y.flux, min.y.flux - 10)
max.y.flux <- ifelse(max.y.flux %% 20 == 0, max.y.flux, max.y.flux + 10)

# create text labels of TI
TI.lab <- data.table(yH1 = max.y.flux /  6,
                     yH2 = max.y.flux /  12,
                     xH1 = as.POSIXct("2019-01-01 05:30:00 MST"), #  "2019-01-01 04:45:00 MST"
                     xH2 = as.POSIXct("2019-01-01 05:30:00 MST"),
                     yL1 = max.y.flux /  6,
                     yL2 = max.y.flux /  12,
                     xL1 = as.POSIXct("2019-01-01 18:30:00 MST"),
                     xL2 = as.POSIXct("2019-01-01 18:30:00 MST"),
                     HL1 = unique(surface.data.b[group == "High Thermal Inertia", L1.TI.l, by = batch.name.f])[,L1.TI.l],
                     HL2 = unique(surface.data.b[group == "High Thermal Inertia", L2.TI.l, by = batch.name.f])[,L2.TI.l],
                     LL1 = unique(surface.data.b[group == "Low Thermal Inertia", L1.TI.l, by = batch.name.f])[,L1.TI.l],
                     LL2 = unique(surface.data.b[group == "Low Thermal Inertia", L2.TI.l, by = batch.name.f])[,L2.TI.l],
                     batch.name.f = b.names)

# create plot
p.flux.b <- (ggplot(data = surface.data.b)
             
             # custom border
             + geom_segment(aes(x = min.x.flux, y = min.y.flux, xend = max.x.flux, yend = min.y.flux))   # x border (x,y) (xend,yend)
             + geom_segment(aes(x = min.x.flux, y = min.y.flux, xend = min.x.flux, yend = max.y.flux))  # y border (x,y) (xend,yend)
             
             # line + point based on named factor of flux
             + geom_line(aes(y = out.flux, x = date.time, color = group.f, linetype = group.f), size = 1)
             #+ geom_point(aes(y = out.flux, x = date.time, color = new.name.f), size = 2, data = surface.data.a[mins %in% c(0) & secs == 0,])
             
             # thermal inertia labels
             + geom_text(data = TI.lab, aes(y = yH1, x = xH1, label = HL1), family = my.font, size = 2.5, color = "#10316B")
             + geom_text(data = TI.lab, aes(y = yH2, x = xH2, label = HL2), family = my.font, size = 2.5, color = "#10316B")
             + geom_text(data = TI.lab, aes(y = yL1, x = xL1, label = LL1), family = my.font, size = 2.5, color = "#BF1C3D")
             + geom_text(data = TI.lab, aes(y = yL2, x = xL2, label = LL2), family = my.font, size = 2.5, color = "#BF1C3D")
             
             # units label for inertia values
             #+ geom_text(aes(y = 80, x = as.POSIXct("2019-01-01 12:00:00")), label = expression(paste(J, m^{-2}, K^{-1}, s^{0.5})), family = my.font, size = 2.5, color = "#10316B")
             
             # plot/axis titles & second axis for solar rad units
             + labs(x = "Time of Day", y = bquote('Mean Outgoing Sensible Heat Flux ('*W/m^2*')'))
             + scale_color_manual(name = "", values = p.col, guide = guide_legend(reverse = F))
             + scale_linetype_manual(name = "", values = p.lty, guide = guide_legend(reverse = F))
             #+ scale_shape_manual(name = "", values = p.shp)
             + facet_wrap(~batch.name.f)
             
             # scales
             + scale_x_datetime(expand = c(0,0), limits = c(min.x.flux, max.x.flux), date_breaks = "3 hours", date_labels = "%H") #
             + scale_y_continuous(expand = c(0,0), limits = c(min.y.flux, max.y.flux), breaks = seq(min.y.flux, max.y.flux, 40))
             
             # theme and formatting
             + theme_minimal()
             + theme(text = element_text(family = my.font, size = 12, colour = "black"), 
                     axis.text = element_text(colour = "black"),
                     plot.margin = margin(t = 5, r = 10, b = 50, l = 10, unit = "pt"),
                     panel.spacing.x = unit(7, "mm"),
                     panel.spacing.y = unit(4, "mm"),
                     axis.text.x = element_text(vjust = -1),
                     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)),
                     axis.ticks = element_line(color = "grey80", size = 0.28),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                     strip.text.x = element_text(size = 11, hjust = 0.5, vjust = 1), #face = "bold"
                     legend.position = c(0.525, -0.17),
                     legend.key.width = unit(30, "mm"),
                     legend.text = element_text(size = 10),
                     legend.spacing.y = unit(1, "mm"),
                     legend.direction ="vertical")
)
p.flux.b

ggsave("figures/heat-flux-diff-thermal-inertia.png", p.flux.b, 
       device = "png", scale = 1, width = 6, height = 6, dpi = 300, units = "in") 

### two facet version

# new batch names as factor
surface.data.b[, batch.name2 := batch.name]
surface.data.b[batch.name == "(a) Asphalt Only", batch.name2 := "(a) Asphalt Surfaced Pavements"]
surface.data.b[batch.name == "(b) Asphalt Overlays on PCC ", batch.name2 := "(a) Asphalt Surfaced Pavements"]
surface.data.b[batch.name == "(c) Concrete Only", batch.name2 := "(b) Concrete Surfaced Pavements"]
surface.data.b[batch.name == "(d) Whitetopped Asphalt", batch.name2 := "(b) Concrete Surfaced Pavements"]

# aggregate
surface.data.b2 <- surface.data.b[, .(L1.TI = signif(mean(L1.TI, na.rm = T), 3), 
                                      L2.TI = signif(mean(L2.TI, na.rm = T), 3),
                                      out.flux = mean(out.flux)),
                                   by = c("date.time", "batch.name2", "group.f")]


# label as character formatted
b.names2 <- unique(surface.data.b2[,batch.name2])
surface.data.b2[, batch.name2.f := factor(batch.name2, levels = b.names2)]
surface.data.b2[, group.f := factor(group.f, levels = p.group)]
surface.data.b2[, L1.TI.l := as.character(L1.TI)]
surface.data.b2[, L2.TI.l := as.character(L2.TI)]
surface.data.b2[group.f == "High Thermal Inertia", L1.TI.l := paste("L1 TI (High) =", format(L1.TI, nsmall = 0))]
surface.data.b2[group.f == "High Thermal Inertia", L2.TI.l := paste("L2 TI (High) =", format(L2.TI, nsmall = 0))]
surface.data.b2[group.f == "Low Thermal Inertia", L1.TI.l := paste("L1 TI (Low) =", format(L1.TI, nsmall = 0))]
surface.data.b2[group.f == "Low Thermal Inertia", L2.TI.l := paste("L2 TI (Low) =", format(L2.TI, nsmall = 0))]

# create text labels of TI
TI.lab2 <- data.table(yH1 = max.y.flux /  6,
                     yH2 = max.y.flux /  12,
                     xH1 = as.POSIXct("2019-01-01 05:30:00 MST"), #  "2019-01-01 04:45:00 MST"
                     xH2 = as.POSIXct("2019-01-01 05:30:00 MST"),
                     yL1 = max.y.flux /  6,
                     yL2 = max.y.flux /  12,
                     xL1 = as.POSIXct("2019-01-01 18:30:00 MST"),
                     xL2 = as.POSIXct("2019-01-01 18:30:00 MST"),
                     HL1 = unique(surface.data.b2[group.f == "High Thermal Inertia", L1.TI.l, by = batch.name2.f])[,L1.TI.l],
                     HL2 = unique(surface.data.b2[group.f == "High Thermal Inertia", L2.TI.l, by = batch.name2.f])[,L2.TI.l],
                     LL1 = unique(surface.data.b2[group.f == "Low Thermal Inertia", L1.TI.l, by = batch.name2.f])[,L1.TI.l],
                     LL2 = unique(surface.data.b2[group.f == "Low Thermal Inertia", L2.TI.l, by = batch.name2.f])[,L2.TI.l],
                     batch.name2.f = b.names2)

# create plot
p.flux.b2 <- (ggplot(data = surface.data.b2)
             
             # custom border
             + geom_segment(aes(x = min.x.flux, y = min.y.flux, xend = max.x.flux, yend = min.y.flux))   # x border (x,y) (xend,yend)
             + geom_segment(aes(x = min.x.flux, y = min.y.flux, xend = min.x.flux, yend = max.y.flux))  # y border (x,y) (xend,yend)
             
             # line + point based on named factor of flux
             + geom_line(aes(y = out.flux, x = date.time, color = group.f, linetype = group.f), size = 1)
             #+ geom_point(aes(y = out.flux, x = date.time, color = new.name.f), size = 2, data = surface.data.a[mins %in% c(0) & secs == 0,])
             
             # thermal inertia labels
             + geom_text(data = TI.lab2, aes(y = yH1, x = xH1, label = HL1), family = my.font, size = 2.5, color = "#10316B")
             + geom_text(data = TI.lab2, aes(y = yH2, x = xH2, label = HL2), family = my.font, size = 2.5, color = "#10316B")
             + geom_text(data = TI.lab2, aes(y = yL1, x = xL1, label = LL1), family = my.font, size = 2.5, color = "#BF1C3D")
             + geom_text(data = TI.lab2, aes(y = yL2, x = xL2, label = LL2), family = my.font, size = 2.5, color = "#BF1C3D")
             
             # units label for inertia values
             #+ geom_text(aes(y = 80, x = as.POSIXct("2019-01-01 12:00:00")), label = expression(paste(J, m^{-2}, K^{-1}, s^{0.5})), family = my.font, size = 2.5, color = "#10316B")
             
             # plot/axis titles & second axis for solar rad units
             + labs(x = "Time of Day", y = bquote('Mean Outgoing Sensible Heat Flux ('*W/m^2*')'))
             + scale_color_manual(name = "", values = p.col, guide = guide_legend(reverse = F))
             + scale_linetype_manual(name = "", values = p.lty, guide = guide_legend(reverse = F))
             #+ scale_shape_manual(name = "", values = p.shp)
             + facet_wrap(~batch.name2.f)
             
             # scales
             + scale_x_datetime(expand = c(0,0), limits = c(min.x.flux, max.x.flux), date_breaks = "3 hours", date_labels = "%H") #
             + scale_y_continuous(expand = c(0,0), limits = c(min.y.flux, max.y.flux), breaks = seq(min.y.flux, max.y.flux, 40))
             
             # theme and formatting
             + theme_minimal()
             + theme(text = element_text(family = my.font, size = 12, colour = "black"), 
                     axis.text = element_text(colour = "black"),
                     plot.margin = margin(t = 2, r = 8, b = 40, l = 2, unit = "pt"),
                     panel.spacing.x = unit(7, "mm"),
                     panel.spacing.y = unit(4, "mm"),
                     axis.text.x = element_text(vjust = -1),
                     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)),
                     axis.ticks = element_line(color = "grey80", size = 0.28),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 10.5),
                     strip.text.x = element_text(size = 10.5, hjust = 0.5, vjust = 1), #face = "bold"
                     legend.position = c(0.525, -0.3),
                     legend.key.width = unit(30, "mm"),
                     legend.text = element_text(size = 10),
                     legend.spacing.y = unit(1, "mm"),
                     legend.direction ="vertical")
)
p.flux.b2

ggsave("figures/heat-flux-diff-thermal-inertia2.png", p.flux.b2, 
       device = "png", scale = 1, width = 6, height = 4, dpi = 300, units = "in") 

###############################
# VEH WASTE HEAT BY TIME PLOT #
###############################
#veh.heat <- fread(here("data/veh-waste-heat-sample.csv"))

# define name of run
run.name <- "metro-phx"
#run.name <- "phx-dwntwn"
#run.name <- "north-tempe"

# define resolution 
#res <- 164.042  #  ~50m x 50m
#res <- 328.084  # ~100m x 100m
res <- 820.21  # ~250m x 250m
#res <- 1640.42 # ~500m x 500m
#res <- 3280.84 # ~1000 x 1000 


# import veh hourly raster data
#veh.heat <- readRDS(here(paste0("data/outputs/rasters/master-veh-time-heat-", run.name, "-", res / 3.28084, "m.rds")))
veh.heat <- stack(here(paste0("data/outputs/rasters/master-veh-time-heat-", run.name, "-", res / 3.28084, "m.tif")))
#names(veh.heat) <- readRDS(here(paste0("data/outputs/rasters/master-veh-time-heat-", run.name, "-", res / 3.28084, "m-names.rds")))

# pavement summary data
pheat.u <- readRDS(here("data/outputs/pavement-heat-time-metadata.rds"))

# import daily avg raster data
r.all <- stack(here(paste0("data/outputs/rasters/master-pave-veh-heat-", run.name, "-", res / 3.28084, "m.tif")))
names(r.all) <- readRDS(here(paste0("data/outputs/rasters/master-veh-time-heat-", run.name, "-", res / 3.28084, "m-names.rds")))

# calculate mean veh heat flux over road area by class and time step
# to prevent issues with extrememly high estaimtes of waste heat, force very small fractional areas to 0 (force less 5% to 0)
#values(r.all$avg.local.road) * ((res / 3.28084)^2) # area (m2) of local roads by cell local
# create summary data.table for roadway class added heat flux by time of day
vheat <- list(date.time = unique(pheat.u[, date.time]),
              pave.class = c("highway", "major", "minor", "local"))
vheat <- as.data.table(expand.grid(vheat))

min.road.frac <- 0.01

# min/max flux by hour and road class is min veh flux in cell div by max fractional area road
# median flux by hour and road class is as avg of min median and max median flux by cell
for(i in 1:(length(unique(vheat[,date.time])))){
  d <- unique(vheat[,date.time])[i]
  temp.min <- ifelse(values(r.all$max.local.road) < min.road.frac, NA, values(veh.heat[[i]]) / values(r.all$max.local.road))
  temp.max <- ifelse(values(r.all$min.local.road) < min.road.frac, NA, values(veh.heat[[i+500]]) / values(r.all$min.local.road))
  temp.avg <- ifelse(values(r.all$avg.local.road) < min.road.frac, NA, values(veh.heat[[i+1000]]) / values(r.all$avg.local.road))
  temp.min <- ifelse(is.infinite(temp.min) == T, 0, temp.min)
  temp.max <- ifelse(is.infinite(temp.max) == T, 0, temp.max)
  temp.avg <- ifelse(is.infinite(temp.avg) == T, 0, temp.avg)
  vheat[date.time == d & pave.class == "local", min.add.flux := ifelse(sum(temp.min, na.rm = T) == 0, 0, mean(temp.min, na.rm = T))]
  vheat[date.time == d & pave.class == "local", max.add.flux := ifelse(sum(temp.max, na.rm = T) == 0, 0, mean(temp.max, na.rm = T))]
  vheat[date.time == d & pave.class == "local", med.add.flux := (quantile(temp.min, 0.5, na.rm = T) + quantile(temp.max, 0.5, na.rm = T)) / 2]
  
  temp.min <- ifelse(values(r.all$max.minor.road) < min.road.frac, NA, values(veh.heat[[i+100]]) / values(r.all$max.minor.road))
  temp.max <- ifelse(values(r.all$min.minor.road) < min.road.frac, NA, values(veh.heat[[i+600]]) / values(r.all$min.minor.road))
  temp.min <- ifelse(is.infinite(temp.min) == T, 0, temp.min)
  temp.max <- ifelse(is.infinite(temp.max) == T, 0, temp.max)
  vheat[date.time == d & pave.class == "minor", min.add.flux := ifelse(sum(temp.min, na.rm = T) == 0, 0, mean(temp.min, na.rm = T))]
  vheat[date.time == d & pave.class == "minor", max.add.flux := ifelse(sum(temp.max, na.rm = T) == 0, 0, mean(temp.max, na.rm = T))]
  vheat[date.time == d & pave.class == "minor", med.add.flux := (quantile(temp.min, 0.5, na.rm = T) + quantile(temp.max, 0.5, na.rm = T)) / 2]
  
  temp.min <- ifelse(values(r.all$max.major.road) < min.road.frac, NA, values(veh.heat[[i+200]]) / values(r.all$max.major.road))
  temp.max <- ifelse(values(r.all$min.major.road) < min.road.frac, NA, values(veh.heat[[i+700]]) / values(r.all$min.major.road))
  temp.min <- ifelse(is.infinite(temp.min) == T, 0, temp.min)
  temp.max <- ifelse(is.infinite(temp.max) == T, 0, temp.max)
  vheat[date.time == d & pave.class == "major", min.add.flux := ifelse(sum(temp.min, na.rm = T) == 0, 0, mean(temp.min, na.rm = T))]
  vheat[date.time == d & pave.class == "major", max.add.flux := ifelse(sum(temp.max, na.rm = T) == 0, 0, mean(temp.max, na.rm = T))]
  vheat[date.time == d & pave.class == "major", med.add.flux := (quantile(temp.min, 0.5, na.rm = T) + quantile(temp.max, 0.5, na.rm = T)) / 2]
  
  temp.min <- ifelse(values(r.all$max.hiway.road) < min.road.frac, NA, values(veh.heat[[i+300]]) / values(r.all$max.hiway.road))
  temp.max <- ifelse(values(r.all$min.hiway.road) < min.road.frac, NA, values(veh.heat[[i+800]]) / values(r.all$min.hiway.road))
  temp.min <- ifelse(is.infinite(temp.min) == T, 0, temp.min)
  temp.max <- ifelse(is.infinite(temp.max) == T, 0, temp.max)
  vheat[date.time == d & pave.class == "highway", min.add.flux := ifelse(sum(temp.min, na.rm = T) == 0, 0, mean(temp.min, na.rm = T))]
  vheat[date.time == d & pave.class == "highway", max.add.flux := ifelse(sum(temp.max, na.rm = T) == 0, 0, mean(temp.max, na.rm = T))]
  vheat[date.time == d & pave.class == "highway", med.add.flux := (quantile(temp.min, 0.5, na.rm = T) + quantile(temp.max, 0.5, na.rm = T)) / 2]

  temp.avg <- ifelse(values(r.all$avg.local.road) < min.road.frac, NA, values(veh.heat[[i+1000]]) / values(r.all$avg.local.road))
  temp.avg <- ifelse(is.infinite(temp.avg) == T, 0, temp.avg)
  vheat[date.time == d & pave.class == "local", mean.add.flux := ifelse(sum(temp.avg, na.rm = T) == 0, 0, mean(temp.avg, na.rm = T))]
  
  temp.avg <- ifelse(values(r.all$avg.minor.road) < min.road.frac, NA, values(veh.heat[[i+1100]]) / values(r.all$avg.minor.road))
  temp.avg <- ifelse(is.infinite(temp.avg) == T, 0, temp.avg)
  vheat[date.time == d & pave.class == "minor", mean.add.flux := ifelse(sum(temp.avg, na.rm = T) == 0, 0, mean(temp.avg, na.rm = T))]
  
  temp.avg <- ifelse(values(r.all$avg.major.road) < min.road.frac, NA, values(veh.heat[[i+1200]]) / values(r.all$avg.major.road))
  temp.avg <- ifelse(is.infinite(temp.avg) == T, 0, temp.avg)
  vheat[date.time == d & pave.class == "major", mean.add.flux := ifelse(sum(temp.avg, na.rm = T) == 0, 0, mean(temp.avg, na.rm = T))]
  
  temp.avg <- ifelse(values(r.all$avg.hiway.road) < min.road.frac, NA, values(veh.heat[[i+1300]]) / values(r.all$avg.hiway.road))
  temp.avg <- ifelse(is.infinite(temp.avg) == T, 0, temp.avg)
  vheat[date.time == d & pave.class == "highway", mean.add.flux := ifelse(sum(temp.avg, na.rm = T) == 0, 0, mean(temp.avg, na.rm = T))]
}

# mean flux
#vheat[, mean.add.flux := (min.add.flux + max.add.flux) / 2]

# create time raster of pave heat
pave.heat <- stack(r.all$min.day.flux.local, r.all$max.day.flux.local,
                   r.all$min.day.flux.minor, r.all$max.day.flux.minor,
                   r.all$min.day.flux.major, r.all$max.day.flux.major,
                   r.all$min.day.flux.hiway, r.all$max.day.flux.hiway)
pave.heat <- stack(pave.heat, stackApply(pave.heat, indices = rep(1:4, each = 2), fun = mean))
names(pave.heat)[9:12] <- c("avg.day.flux.local", "avg.day.flux.minor", "avg.day.flux.major", "avg.day.flux.hiway")

pave.heat.time <- veh.heat
values(pave.heat.time) <- NA
#pave.heat.time <- raster()

#plot(pheat.u[pave.class == "local" & SVF == 1.0, date.time], pheat.u[pave.class == "local" & SVF == 1, min.add.flux], type = "l")
save.image(here("data/outputs/temp/figures-1.RData"))

for(i in 1:(length(unique(pheat.u[,date.time])))){
  d <- unique(pheat.u[,date.time])[i]
  
  # min/max local roadway flux for by times 
  pave.heat.time[[i]] <- ((r.all$adj.SVF * pheat.u[date.time == d & pave.class == "local" & SVF == 1.0, min.add.flux]) +
                                    ((1 - r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "local" & SVF == 0.1, min.add.flux])) * r.all$min.local.road
  values(pave.heat.time[[i+500]]) <- ((values(r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "local" & SVF == 1.0, max.add.flux]) +
                                        ((1 - values(r.all$adj.SVF)) * pheat.u[date.time == d & pave.class == "local" & SVF == 0.1, max.add.flux])) * values(r.all$max.local.road)
  pave.heat.time[[i+1000]] <- ((r.all$adj.SVF * pheat.u[date.time == d & pave.class == "local" & SVF == 1.0, mean.add.flux]) +
                                 ((1 - r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "local" & SVF == 0.1, mean.add.flux])) * r.all$avg.local.road

  # min/max minor roadway flux for by times 
  values(pave.heat.time[[i+100]]) <- ((values(r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "minor" & SVF == 1.0, min.add.flux]) +
                                    ((1 - values(r.all$adj.SVF)) * pheat.u[date.time == d & pave.class == "minor" & SVF == 0.1, min.add.flux])) * values(r.all$min.minor.road)
  values(pave.heat.time[[i+600]]) <- ((values(r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "minor" & SVF == 1.0, max.add.flux]) +
                                        ((1 - values(r.all$adj.SVF)) * pheat.u[date.time == d & pave.class == "minor" & SVF == 0.1, max.add.flux])) * values(r.all$max.minor.road) 
  pave.heat.time[[i+1100]] <- ((r.all$adj.SVF * pheat.u[date.time == d & pave.class == "minor" & SVF == 1.0, mean.add.flux]) +
                                 ((1 - r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "minor" & SVF == 0.1, mean.add.flux])) * r.all$avg.minor.road
  
  # min/max minor roadway flux for by times 
  values(pave.heat.time[[i+200]]) <- ((values(r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "major" & SVF == 1.0, min.add.flux]) +
                                        ((1 - values(r.all$adj.SVF)) * pheat.u[date.time == d & pave.class == "major" & SVF == 0.1, min.add.flux])) * values(r.all$min.major.road)
  values(pave.heat.time[[i+700]]) <- ((values(r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "major" & SVF == 1.0, max.add.flux]) +
                                        ((1 - values(r.all$adj.SVF)) * pheat.u[date.time == d & pave.class == "major" & SVF == 0.1, max.add.flux])) * values(r.all$max.major.road) 
  pave.heat.time[[i+1200]] <- ((r.all$adj.SVF * pheat.u[date.time == d & pave.class == "major" & SVF == 1.0, mean.add.flux]) +
                                 ((1 - r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "major" & SVF == 0.1, mean.add.flux])) * r.all$avg.major.road
  
  # min/max minor roadway flux for by times 
  values(pave.heat.time[[i+300]]) <- ((values(r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "highway" & SVF == 1.0, min.add.flux]) +
                                        ((1 - values(r.all$adj.SVF)) * pheat.u[date.time == d & pave.class == "highway" & SVF == 0.1, min.add.flux])) * values(r.all$min.hiway.road)
  values(pave.heat.time[[i+800]]) <- ((values(r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "highway" & SVF == 1.0, max.add.flux]) +
                                        ((1 - values(r.all$adj.SVF)) * pheat.u[date.time == d & pave.class == "highway" & SVF == 0.1, max.add.flux])) * values(r.all$max.hiway.road)
  pave.heat.time[[i+1300]] <- ((r.all$adj.SVF * pheat.u[date.time == d & pave.class == "highway" & SVF == 1.0, mean.add.flux]) +
                                 ((1 - r.all$adj.SVF) * pheat.u[date.time == d & pave.class == "highway" & SVF == 0.1, mean.add.flux])) * r.all$avg.hiway.road
}

# add min/max all roads by time
pave.heat.time <- stack(pave.heat.time[[1:400]], stackApply(pave.heat.time[[1:400]], indices = rep(1:100, 4), fun = sum),
                        pave.heat.time[[501:900]], stackApply(pave.heat.time[[501:900]], indices = rep(1:100, 4), fun = sum),
                        pave.heat.time[[1001:1400]], stackApply(pave.heat.time[[1001:1400]], indices = rep(1:100, 4), fun = sum))
names(pave.heat.time)[1:1000] <- c(paste0("min.flux.pave.local.", 1:100), paste0("min.flux.pave.minor.", 1:100), 
                                   paste0("min.flux.pave.major.", 1:100), paste0("min.flux.pave.hiway.", 1:100),
                                   paste0("min.flux.pave.all.", 1:100), 
                                   paste0("max.flux.pave.local.", 1:100), paste0("max.flux.pave.minor.", 1:100),
                                   paste0("max.flux.pave.major.", 1:100), paste0("max.flux.pave.hiway.", 1:100),
                                   paste0("max.flux.pave.all.", 1:100))
names(pave.heat.time)[1001:1500] <- c(paste0("avg.flux.pave.local.",1:100), paste0("avg.flux.pave.minor.",1:100), 
                                      paste0("avg.flux.pave.major.",1:100), paste0("avg.flux.pave.hiway.",1:100), 
                                      paste0("avg.flux.pave.all.",1:100))

pheat <- list(date.time = unique(pheat.u[, date.time]),
              pave.class = c("highway", "major", "minor", "local"))
pheat <- as.data.table(expand.grid(pheat))

# calculate mean pave heat flux over road area by class and time step
# follow same approach as vehicles for consistency

# min/max flux by hour and road class is min veh flux in cell div by max fractional area road
# median flux by hour and road class is as avg of min median and max median flux by cell
for(i in 1:(length(unique(pheat[,date.time])))){
  d <- unique(pheat[,date.time])[i]
  temp.min.lr <- ifelse(values(r.all$max.local.road) < min.road.frac, NA, values(pave.heat.time[[i]]) / values(r.all$max.local.road))
  temp.max.lr <- ifelse(values(r.all$min.local.road) < min.road.frac, NA, values(pave.heat.time[[i+500]]) / values(r.all$min.local.road))
  temp.avg.lr <- ifelse(values(r.all$avg.local.road) < min.road.frac, NA, values(pave.heat.time[[i+1000]]) / values(r.all$avg.local.road))
  temp.min.lr <- ifelse(is.infinite(temp.min.lr) == T, 0, temp.min.lr)
  temp.max.lr <- ifelse(is.infinite(temp.max.lr) == T, 0, temp.max.lr)
  temp.avg.lr <- ifelse(is.infinite(temp.avg.lr) == T, 0, temp.avg.lr)
  pheat[date.time == d & pave.class == "local", min.add.flux := ifelse(sum(temp.min.lr, na.rm = T) == 0, 0, mean(temp.min.lr, na.rm = T))]
  pheat[date.time == d & pave.class == "local", max.add.flux := ifelse(sum(temp.max.lr, na.rm = T) == 0, 0, mean(temp.max.lr, na.rm = T))]
  pheat[date.time == d & pave.class == "local", med.add.flux := (quantile(temp.min.lr, 0.5, na.rm = T) + quantile(temp.max.lr, 0.5, na.rm = T)) / 2]
  pheat[date.time == d & pave.class == "local", mean.add.flux := ifelse(sum(temp.avg.lr, na.rm = T) == 0, 0, mean(temp.avg.lr, na.rm = T))]
  
  temp.min.mnr <- ifelse(values(r.all$max.minor.road) < min.road.frac, NA, values(pave.heat.time[[i+100]]) / values(r.all$max.minor.road))
  temp.max.mnr <- ifelse(values(r.all$min.minor.road) < min.road.frac, NA, values(pave.heat.time[[i+600]]) / values(r.all$min.minor.road))
  temp.avg.mnr <- ifelse(values(r.all$avg.minor.road) < min.road.frac, NA, values(pave.heat.time[[i+1100]]) / values(r.all$avg.minor.road))
  temp.min.mnr <- ifelse(is.infinite(temp.min.mnr) == T, 0, temp.min.mnr)
  temp.max.mnr <- ifelse(is.infinite(temp.max.mnr) == T, 0, temp.max.mnr)
  temp.avg.mnr <- ifelse(is.infinite(temp.avg.mnr) == T, 0, temp.avg.mnr)
  pheat[date.time == d & pave.class == "minor", min.add.flux := ifelse(sum(temp.min.mnr, na.rm = T) == 0, 0, mean(temp.min.mnr, na.rm = T))]
  pheat[date.time == d & pave.class == "minor", max.add.flux := ifelse(sum(temp.max.mnr, na.rm = T) == 0, 0, mean(temp.max.mnr, na.rm = T))]
  pheat[date.time == d & pave.class == "minor", med.add.flux := (quantile(temp.min.mnr, 0.5, na.rm = T) + quantile(temp.max.mnr, 0.5, na.rm = T)) / 2]
  pheat[date.time == d & pave.class == "minor", mean.add.flux := ifelse(sum(temp.avg.mnr, na.rm = T) == 0, 0, mean(temp.avg.mnr, na.rm = T))]
  
  temp.min.mjr <- ifelse(values(r.all$max.major.road) < min.road.frac, NA, values(pave.heat.time[[i+200]]) / values(r.all$max.major.road))
  temp.max.mjr <- ifelse(values(r.all$min.major.road) < min.road.frac, NA, values(pave.heat.time[[i+700]]) / values(r.all$min.major.road))
  temp.avg.mjr <- ifelse(values(r.all$avg.major.road) < min.road.frac, NA, values(pave.heat.time[[i+1200]]) / values(r.all$avg.major.road))
  temp.min.mjr <- ifelse(is.infinite(temp.min.mjr) == T, 0, temp.min.mjr)
  temp.max.mjr <- ifelse(is.infinite(temp.max.mjr) == T, 0, temp.max.mjr)
  temp.avg.mjr <- ifelse(is.infinite(temp.avg.mjr) == T, 0, temp.avg.mjr)
  pheat[date.time == d & pave.class == "major", min.add.flux := ifelse(sum(temp.min.mjr, na.rm = T) == 0, 0, mean(temp.min.mjr, na.rm = T))]
  pheat[date.time == d & pave.class == "major", max.add.flux := ifelse(sum(temp.max.mjr, na.rm = T) == 0, 0, mean(temp.max.mjr, na.rm = T))]
  pheat[date.time == d & pave.class == "major", med.add.flux := (quantile(temp.min, 0.5, na.rm = T) + quantile(temp.max, 0.5, na.rm = T)) / 2]
  pheat[date.time == d & pave.class == "major", mean.add.flux := ifelse(sum(temp.avg.mjr, na.rm = T) == 0, 0, mean(temp.avg.mjr, na.rm = T))]
  
  temp.min.hr <- ifelse(values(r.all$max.hiway.road) < min.road.frac, NA, values(pave.heat.time[[i+300]]) / values(r.all$max.hiway.road))
  temp.max.hr <- ifelse(values(r.all$min.hiway.road) < min.road.frac, NA, values(pave.heat.time[[i+800]]) / values(r.all$min.hiway.road))
  temp.avg.hr <- ifelse(values(r.all$avg.hiway.road) < min.road.frac, NA, values(pave.heat.time[[i+1300]]) / values(r.all$avg.hiway.road))
  temp.min.hr <- ifelse(is.infinite(temp.min.hr) == T, 0, temp.min.hr)
  temp.max.hr <- ifelse(is.infinite(temp.max.hr) == T, 0, temp.max.hr)
  temp.avg.hr <- ifelse(is.infinite(temp.avg.hr) == T, 0, temp.avg.hr)
  pheat[date.time == d & pave.class == "highway", min.add.flux := ifelse(sum(temp.min.hr, na.rm = T) == 0, 0, mean(temp.min.hr, na.rm = T))]
  pheat[date.time == d & pave.class == "highway", max.add.flux := ifelse(sum(temp.max.hr, na.rm = T) == 0, 0, mean(temp.max.hr, na.rm = T))]
  pheat[date.time == d & pave.class == "highway", med.add.flux := (quantile(temp.min.hr, 0.5, na.rm = T) + quantile(temp.max.hr, 0.5, na.rm = T)) / 2]
  pheat[date.time == d & pave.class == "highway", mean.add.flux := ifelse(sum(temp.avg.hr, na.rm = T) == 0, 0, mean(temp.avg.hr, na.rm = T))]
}

save.image(here("data/outputs/temp/figures-2.RData"))
load(here("data/outputs/temp/figures-2.RData"))

######################################

max(pheat$max.add.flux) # = 473.9721
max(pheat.u$max.add.flux) # 162.0044
max(values(pave.heat.time)) # = 221.3138

# test plot
plot(vheat[pave.class == "highway", date.time], vheat[pave.class == "highway", mean.add.flux], type="l", col="black", ylab = "Mean Heat Flux (W/m2)", xlab = "Time of Day", ylim = c(0, 200))
lines(vheat[pave.class == "major", date.time], vheat[pave.class == "major", mean.add.flux], col="red")
lines(vheat[pave.class == "minor", date.time], vheat[pave.class == "minor", mean.add.flux], col="blue")
lines(vheat[pave.class == "local", date.time], vheat[pave.class == "local", mean.add.flux], col="green")
lines(pheat[pave.class == "highway", date.time], pheat[pave.class == "highway", mean.add.flux], col="black")
lines(pheat[pave.class == "major", date.time], pheat[pave.class == "major", mean.add.flux], col="red")
lines(pheat[pave.class == "minor", date.time], pheat[pave.class == "minor", mean.add.flux], col="blue")
lines(pheat[pave.class == "local", date.time], pheat[pave.class == "local", mean.add.flux], col="green")
lines(pheat.u[pave.class == "highway" & SVF == 1, date.time], pheat.u[pave.class == "highway" & SVF == 1, mean.add.flux], col="black")
lines(pheat.u[pave.class == "major" & SVF == 1, date.time], pheat.u[pave.class == "major" & SVF == 1, mean.add.flux], col="red")
lines(pheat.u[pave.class == "minor" & SVF == 1, date.time], pheat.u[pave.class == "minor" & SVF == 1, mean.add.flux], col="blue")
lines(pheat.u[pave.class == "local" & SVF == 1, date.time], pheat.u[pave.class == "local" & SVF == 1, mean.add.flux], col="green")
plot(r.all[[c("avg.all.roads", "avg.all.park", "daily.vkt", "total.avg.day.flux")]])

######################################
# VEH + HEAT FLUX BY TIME OF DAY FIG #
######################################

# add plot label type to aggregated vehicle and pavement
vheat[, type := "Vehicles"]
pheat.a <- pheat[, .(mean.add.flux = mean(mean.add.flux)),
                 by = c("date.time")]
pheat.a[, class.type := "Average Phoenix Roadway"]

# import data
#bg.surface.data <- readRDS(here("data/outputs/run_metadata_20190520_171637/all_pave_surface_data.rds"))[batch.name == "Bare Ground / Desert Soil",]
#pave.surface.data <- readRDS(here("data/outputs/run_metadata_20190625_110129/all_pave_surface_data.rds")) # surface temporal data by run for pavements
#pave.surface.data <- readRDS(here("data/outputs/run_metadata_20190526_185113_varied_thick/all_pave_surface_data.rds"))
#all.surface.data <- rbind(bg.surface.data, pave.surface.data) # merge bare ground and pavement simulations seperatley (bare ground is rarely rerun)

#all.surface.data.7d <- readRDS(here("data/outputs/run_metadata_20190520_171637/all_pave_surface_data.rds")) # old 7 day runs (will replace)
#all.surface.data.th <- readRDS(here("data/outputs/run_metadata_20190526_185113_varied_thick/all_pave_surface_data.rds"))
#all.surface.data.ti <- readRDS(here("data/outputs/run_metadata_20190524_112541_varied_TI/all_pave_surface_data.rds"))
#all.surface.data.al <- readRDS(here("data/outputs/run_metadata_20190526_182759_varied_albedo/all_pave_surface_data.rds"))
#all.surface.data <- rbind(all.surface.data.th, all.surface.data.ti, all.surface.data.al, all.surface.data.7d)

#all.surface.data <- all.surface.data[!(pave.name %in% c("Bare Dry Soil #1"))]
all.surface.data <- readRDS(here("data/outputs/run_metadata_20190520_171637/all_pave_surface_data.rds")) # original fig

# force to static date for date.time for easier manipulation in ggplot, will ignore date
# NOTE: all summarized surface data is filtered previously to only last day of data
all.surface.data[, date.time := as.POSIXct("2019-01-01 00:00:00 MST") + hours(hrs) + minutes(mins) + seconds(secs)] 

# flux comparisons by type
all.surface.data[, mean(q.rad + q.cnv), by = c("pave.name", "batch.name", "albedo")][order(V1)]

# calc flux vars
all.surface.data[, inc.sol := ((1 - albedo) * SVF * solar)]
all.surface.data[, ref.sol := albedo * SVF * solar]
all.surface.data[, net.flux := -inc.sol + q.rad + q.cnv]

# aggregate for plotting
surface.data.c <- all.surface.data[, .(out.flux = mean(q.rad + q.cnv),
                                       inc.sol = mean(inc.sol),
                                       net.flux = mean(net.flux),
                                       ref.sol = mean(ref.sol)),
                                   by = c("date.time", "batch.name", "SVF")]  
surface.data.c <- surface.data.c[SVF == 1.0]

# new names
surface.data.c[, new.name := batch.name]
surface.data.c[batch.name == "Asphalt Pavements", new.name := "Asphalt Pavement"]
surface.data.c[batch.name == "Asphalt Overlays on PCC Pavements", new.name := "Asphalt Pavement"]
surface.data.c[batch.name == "Concrete Pavements", new.name := "Concrete Pavement"]
surface.data.c[batch.name == "Whitetopped Asphalt Pavements", new.name := "Concrete Pavement"]
surface.data.c[batch.name == "Bare Ground / Desert Soil", new.name := "Bare Ground (Desert Soil)"]

surface.data.c <- surface.data.c[, .(out.flux = mean(out.flux)),
                                   by = c("date.time", "new.name")]  

# merge bare ground data as columns for easy subraction to get added heat relative to ground
surface.data.add <- merge(surface.data.c[new.name != "Bare Ground (Desert Soil)",],
                          surface.data.c[new.name == "Bare Ground (Desert Soil)",],
                          suffixes = c("", ".u"), by = c("date.time"))
surface.data.add[, mean.add.flux := out.flux - out.flux.u]

# combine vehicle and pavement heat diurnal summaries
#vheat[, label := paste0(pave.class)]

# plot min/maxes
min.x.veh <- floor_date(min(surface.data.add[, date.time]), unit = "hours")
max.x.veh <- ceiling_date(max(surface.data.add[, date.time]), unit = "hours")
min.y.veh <- 0 #round(min(unlist(vheat[, 2:13]) - 5), - 1) # round down to nearest multiple of 10
#max.y.veh <- round(max(unlist(aheat[, 3:6]) + 5), - 1) # round up to nearest multiple of 10
max.y.veh <- round(max(surface.data.add[, mean.add.flux] + 5), - 1) # round up to nearest multiple of 10

# rename major/minor roadway class to arterial/collector
vheat[, class := str_to_title(pave.class)] # capitilze first
vheat[class == "Major", class := "Arterial"]
vheat[class == "Minor", class := "Collector"]
vheat[, class.type := paste(type, "-", class, "Road")]

surface.data.add[, class.type := paste("Unshaded", new.name)]

p.v.add <- rbind(surface.data.add[, .(date.time, mean.add.flux, class.type)],
                 vheat[, .(date.time, mean.add.flux, class.type)])
                 #pheat.a[, .(date.time, mean.add.flux, class.type)])

# create different legend charateristics for plotting
#aheat[, label := factor(label, levels = v.names)]
pv.names <- unique(p.v.add[, class.type])
pv.names <- c(pv.names[1:2], " ", "  ", pv.names[3:6]) # add blank factor level to customize legend
#pv.names <- c(pv.names[2], pv.names[7], pv.names[1], "  ", pv.names[3:6]) # add blank factor level to customize legend

pv.col <- c("#0C120C", "#918D77", "white", "white", "#C60013", "#00599E", "#DD3E00", "#52006D")  
pv.lty <- c("solid", "twodash", "blank", "blank", "solid", "solid", "longdash", "longdash")
names(pv.col) <- pv.names
names(pv.lty) <- pv.names

# factorize for ggplot
p.v.add[, class.type.f := factor(class.type, levels = pv.names)]

#p.v.add <- p.v.add[class.type != "Pavements - Asphalt Surface" & date.time != min(date.time),]

# create plot
p.flux.c <- (ggplot(data = p.v.add) 
             
             # custom border
             + geom_segment(aes(x = min.x.veh, y = min.y.veh, xend = max.x.veh, yend = min.y.veh))   # x border (x,y) (xend,yend)
             + geom_segment(aes(x = min.x.veh, y = min.y.veh, xend = min.x.veh, yend = max.y.veh))  # y border (x,y) (xend,yend)
             
             # line + point based on named factor of flux
             #+ geom_ribbon(aes(ymin = min.add.flux, ymax = max.add.flux, x = date.time), fill = "grey10")
             + geom_line(aes(y = mean.add.flux, x = date.time, color = class.type.f, linetype = class.type.f), size = 1) #

             # plot/axis titles & second axis for solar rad units
             + labs(x = "Time of Day", y = bquote('Mean Anthropogenic Sensible Heat Flux ('*W/m^2*')'))
             + scale_color_manual(name = "", values = pv.col, drop = F)  # Type - Roadway Class
             + scale_linetype_manual(name = "", values = pv.lty, drop = F) # Type - Roadway Class
             #+ facet_wrap(~class.f)
             
             # scales
             + scale_x_datetime(expand = c(0,0), limits = c(min.x.veh, max.x.veh), date_breaks = "3 hours", date_labels = "%H") #
             + scale_y_continuous(expand = c(0,0), limits = c(min.y.veh, max.y.veh), breaks = seq(min.y.veh, max.y.veh, 20))
             + guides(color = guide_legend(nrow = 4, ncol = 2)) # two rows in legend
             
             # theme and formatting
             + theme_minimal()
             + theme(text = element_text(family = my.font, size = 12, colour = "black"), 
                     axis.text = element_text(colour = "black"),
                     plot.margin = margin(t = 5, r = 8, b = 90, l = 2, unit = "pt"), #b = 85
                     #panel.spacing.x = unit(7, "mm"),
                     #panel.spacing.y = unit(4, "mm"),
                     axis.text.x = element_text(vjust = -1),
                     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)),
                     axis.ticks = element_line(color = "grey50", size = 0.28),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                     #strip.text.x = element_text(size = 12, hjust = 0, vjust = 1), #face = "bold"
                     legend.position = c(0.5, -0.25),
                     legend.key.width = unit(30, "mm"),
                     legend.text = element_text(size = 10),
                     legend.spacing.y = unit(1, "mm"),
                     legend.direction ="vertical",
                     legend.background = element_blank())
)

# smooth out curves with roll mean
p.v.add.s1 <- p.v.add[date.time == min(date.time) | date.time == max(date.time),]
p.v.add.s1[, smean.add.flux := mean.add.flux]

p.v.add.s2 <- p.v.add[class.type %in% c("Unshaded Asphalt Pavement", "Unshaded Concrete Pavement"),] %>% 
  group_by(class.type) %>%
  mutate(smean.add.flux = rollmean(mean.add.flux, 100, fill = NA, align = "right"))

p.v.add.s3 <- p.v.add[!(class.type %in% c("Unshaded Asphalt Pavement", "Unshaded Concrete Pavement")),] %>% 
  group_by(class.type) %>%
  mutate(smean.add.flux = rollmean(mean.add.flux, 5, fill = NA, align = "right"))

p.v.add.s <- rbind(p.v.add.s1, as.data.table(p.v.add.s2), as.data.table(p.v.add.s3))
#p.v.add.s <- rbind(p.v.add.s2, p.v.add.s3)

# create smoothed plot
p.flux.c.s <- (ggplot(data = p.v.add.s[!is.na(smean.add.flux)]) 
             
             # custom border
             + geom_segment(aes(x = min.x.veh, y = min.y.veh, xend = max.x.veh, yend = min.y.veh))   # x border (x,y) (xend,yend)
             + geom_segment(aes(x = min.x.veh, y = min.y.veh, xend = min.x.veh, yend = max.y.veh))  # y border (x,y) (xend,yend)
             
             # line + point based on named factor of flux
             #+ geom_ribbon(aes(ymin = min.add.flux, ymax = max.add.flux, x = date.time), fill = "grey10")
             + geom_line(aes(y = smean.add.flux, x = date.time, color = class.type.f, linetype = class.type.f), size = 1) #
             
             # plot/axis titles & second axis for solar rad units
             + labs(x = "Time of Day", y = bquote('Mean Sensible Heat Flux ('*W/m^2*')'))
             + scale_color_manual(name = "", values = pv.col, drop = F)  # Type - Roadway Class
             + scale_linetype_manual(name = "", values = pv.lty, drop = F) # Type - Roadway Class
             #+ facet_wrap(~class.f)
             
             # scales
             + scale_x_datetime(expand = c(0,0), limits = c(min.x.veh, max.x.veh), date_breaks = "3 hours", date_labels = "%H") #
             + scale_y_continuous(expand = c(0,0), limits = c(min.y.veh, max.y.veh), breaks = seq(min.y.veh, max.y.veh, 20))
             + guides(color = guide_legend(nrow = 4, ncol = 2)) # two rows in legend
             
             # theme and formatting
             + theme_minimal()
             + theme(text = element_text(family = my.font, size = 12, colour = "black"), 
                     axis.text = element_text(colour = "black"),
                     plot.margin = margin(t = 5, r = 8, b = 90, l = 2, unit = "pt"), #b = 85
                     #panel.spacing.x = unit(7, "mm"),
                     #panel.spacing.y = unit(4, "mm"),
                     axis.text.x = element_text(vjust = -1),
                     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)),
                     axis.ticks = element_line(color = "grey50", size = 0.28),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                     #strip.text.x = element_text(size = 12, hjust = 0, vjust = 1), #face = "bold"
                     legend.position = c(0.5, -0.25),
                     legend.key.width = unit(30, "mm"),
                     legend.text = element_text(size = 10),
                     legend.spacing.y = unit(1, "mm"),
                     legend.direction ="vertical",
                     legend.background = element_blank())
)

# save plot
ggsave("figures/add-veh-pave-heat-flux-smooth.png", p.flux.c.s, 
       device = "png", scale = 1, width = 7, height = 6, dpi = 300, units = "in") 
saveRDS(vheat, here("data/outputs/veh-heat-time-summarized.rds"))


####################
# RASTER HEAT MAPS #
####################
plot(r.all[[c("avg.day.flux.roads","avg.day.flux.park", "avg.day.flux.veh", "total.avg.day.flux")]], 
     main = c("(a) Road Pavement","(b) Parking Pavement", 
              "(c) Vehicles","(d) Pavements & Vehicles"))

# import labels & borders 
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # Maricopa UZA (non-buffered) in EPSG:2223
phx.labels <- shapefile(here("data/shapefiles/other/phx_metro_labels-raster.shp"))
maricopa.cnty <- shapefile(here("data/shapefiles/boundaries/maricopa_county.shp"))

# set <1 to NA to ignore in diveraging color palette
values(r.all$avg.day.flux.veh) <- ifelse(values(r.all$avg.day.flux.veh) < 1, NA, values(r.all$avg.day.flux.veh))
values(r.all$avg.day.flux.park) <- ifelse(values(r.all$avg.day.flux.park) < 1, NA, values(r.all$avg.day.flux.park))
values(r.all$avg.day.flux.roads) <- ifelse(values(r.all$avg.day.flux.roads) < 1, NA, values(r.all$avg.day.flux.roads))
values(r.all$total.avg.day.flux) <- ifelse(values(r.all$total.avg.day.flux) < 1, NA, values(r.all$total.avg.day.flux))

#mc.hill <- brick("C:/Users/cghoehne/Dropbox (ASU)/Data and Tools/GIS/AZ Files/maricopa_hillshade/maricopa_hillshade.tif")
mc.hill <- brick(here("data/maricopa_hillshade.tif"))
#saveRDS(mc.hill, here("data/maricopa-hillshade.rds"))
#mc.hill <- readRDS(here("data/maricopa-hillshade.rds")) # Maricopa County Hillashade EPSG 2223, cropped to extent of rasters

# crop rasters to uza 
r.veh.flux <- crop(r.all$avg.day.flux.veh, uza.border, snap = "out")
r.park.flux <- crop(r.all$avg.day.flux.park, uza.border, snap = "out")
r.road.flux <- crop(r.all$avg.day.flux.roads, uza.border, snap = "out")
r.all.flux <- crop(r.all$total.avg.day.flux, uza.border, snap = "out")

# crop maricopa hillshade rasters extent
mc.hill <- crop(mc.hill, maricopa.cnty, snap="out")
mc.hill <- crop(mc.hill, extent(r.all.flux), snap="out")

my.palette <- rev(rainbow(255, end = 0.6)) # start = 0.5,  

# custom heat
my.palette <- colorRampPalette(c("#ffdbcb", "#fed0bd", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#67000d"), # "#ffe5d9", 
                               bias = 1, 
                               interpolate = "linear")(255)

# custom spectral 
my.palette <- colorRampPalette(c("#0076c1","#fff410","#d7191c"), #   
                               bias = 1, 
                               interpolate = "linear")(255)

# import data for inset maps
az.near.borders <- shapefile(here("data/shapefiles/boundaries/AZ-near-state-borders.shp")) # Maricopa UZA (non-buffered) in EPSG:2223
my.extent <- shapefile(here("data/shapefiles/boundaries/map-extent.shp"))
phx.label <- shapefile(here("data/shapefiles/other/phx_city_label_new.shp"))

# define extent of inset
my.coords <- data.table(lon = c(-115.436727,-108.508773), lat = c(37.512245,31.065606))
inset.extent <- as(extent(spTransform(SpatialPoints(coords = my.coords,
                                                    proj4string = crs("+proj=longlat +datum=WGS84")), crs(phx.label))), "SpatialPolygons")
proj4string(inset.extent) <- crs(phx.label)
az.near.borders.c <- crop(az.near.borders, inset.extent)

#p.all.flux.new <- tm_shape(r.all[[c("avg.day.flux.roads","avg.day.flux.park", "avg.day.flux.veh", "total.avg.day.flux")]]) +
p.all.flux.new <-   
  #tm_shape(mc.hill$maricopa_hillshade) +
  #tm_raster(palette = "-Greys", 
  #          legend.show = F, 
  #          alpha = 0.1) +
  tm_shape(stack(r.road.flux, r.park.flux, r.veh.flux, r.all.flux)) +
  tm_raster(palette = my.palette,
          style="cont",
          #breaks= list(c(1, seq(10, 40, 10)), c(1, seq(20, 60, 20)), c(1, seq(5, 15, 5)), c(1,seq(25, 75, 25))),
          breaks= list(c(1, seq(15, 45, 15)), c(1, seq(20, 60, 20)), c(1, seq(5, 15, 5)), c(1,seq(25, 75, 25))),
          legend.show = T,
          title = "Heat Flux\n(W/m\u00B2)",
          colorNA = NULL) + # white
  #tm_shape(phx.labels) +
  #tm_text("name", 
  #        size = 0.5, 
  #        fontfamily = my.font,
  #        fontface = "bold.italic",
  #        alpha = 0.9,
  #        just = "center") +
  tm_compass(north = 0, 
             type = "4star", 
             size = 1.25,
             show.labels = 1, 
             color.dark = "#2a2a2a",
             position = c(0,0.05)) +
  tm_scale_bar(position = c(0.13,0),
               breaks = c(0,10,20),
               size = 0.7) +
  tm_shape(maricopa.cnty) +
  tm_borders(lwd = 1, 
             lty = "solid",
             col = "black",
             alpha = 0.75) +
  tm_fill(alpha = 0) +
  tm_facets(free.scales.text.size = F) +
  #tm_shape(uza.border) +
  #tm_borders(lwd = 0.3, 
  #           lty = "solid",
  #           col = "grey40",
  #           alpha = 0.4) +
  tm_layout(fontfamily = my.font, 
            #bg.color = "grey95",
            #legend.position = c("LEFT", "top"),
            legend.position = c(0, 0.33),
            outer.margins = c(0, 0, 0, 0),
            inner.margins = c(0.01, 0.01, 0.01, 0.01), # b, l, t, r
            between.margin = 0,
            #panel.label.fontface = 2,
            frame = T,
            #frame.lwgd = 1,
            legend.title.size = 1.25,
            legend.text.size = 1,
            legend.height = 0.65,
            panel.show = T,
            panel.labels = c("(a) Road Pavement","(b) Parking Pavement", 
                             "(c) Vehicles","(d) Pavements & Vehicles"),
            panel.label.size = 1.1,
            panel.label.height = 1.7,
            panel.label.bg.color = "#E0E0E0"
            #legend.outside = T)
            ) +
  tmap_options(max.raster = c(plot = 367000200, view = 367000200))

#p.all.flux.new
#tmap_save(p.all.flux.new, width = 5.5, units = "in", filename = here(paste0("figures/mean-daily-heat-flux-4grid-", run.name, "-", res / 3.28084, "m.png")))


# create the inset map
p.inset <-  
  tm_shape(az.near.borders.c) +
  tm_borders(lwd = 1, 
             lty = "solid",
             col = "grey40",
             alpha = 1) +
  tm_shape(phx.label) +
  tm_text("name", 
          size = 0.3, 
          fontfamily = my.font,
          fontface = "bold.italic",
          ymod = 0.4,
          xmod = 0.3) +
  tm_dots(size = 0.01) +
  tm_shape(maricopa.cnty) +
  tm_borders(lwd = 0.7, 
             lty = "solid",
             col = "grey40",
             alpha = 1) +
  tm_shape(uza.border) +
  tm_polygons(col = "grey40",
              lty = 0,
             alpha = 0.4) +
  tm_shape(my.extent) +
  tm_borders(lwd = 0.75, 
             lty = "dotted",
             col = "black",
             alpha = 1) +
  tm_layout(fontfamily = my.font,
            frame = T,
            frame.lwd = 1,
            outer.margins = c(0, 0, 0, 0),
            inner.margins = c(0.05, 0, 0, 0)) # b, l, t, r)
#p.inset

tmap_save(p.all.flux.new, insets_tm = list(p.inset,p.inset,p.inset,p.inset), dpi = 1000, width = 5.875, units = "in", 
          insets_vp = list(viewport(0.44, 0.365, width = 0.14, height = 0.14), # bottom left
                           viewport(0.94, 0.365, width = 0.14, height = 0.14), # bottom right
                           viewport(0.44, 0.865, width = 0.14, height = 0.14), # top left
                           viewport(0.94, 0.865, width = 0.14, height = 0.14)), # top right
          filename = here(paste0("figures/mean-daily-heat-flux-4grid-", run.name, "-", res / 3.28084, "m.png")))


# ROAD FRACTION 4GRID PLOT

# crop rasters to uza
r.loc.min <- stackApply(stack(r.all$avg.local.road, r.all$avg.minor.road), fun = sum, indices = c(1))
r.loc.min <- crop(r.loc.min, uza.border, snap = "out")
r.maj.hig <- stackApply(stack(r.all$avg.major.road, r.all$avg.hiway.road), fun = sum, indices = c(1))
r.maj.hig <- crop(r.maj.hig, uza.border, snap = "out")
r.park <- crop(r.all$avg.all.park, uza.border, snap = "out")
r.pave <- crop(r.all$avg.pave, uza.border, snap = "out")

# set 0 to NA to ignore in diveraging color palette
values(r.loc.min) <- ifelse(values(r.loc.min) == 0, NA, values(r.loc.min))
values(r.maj.hig) <- ifelse(values(r.maj.hig) == 0, NA, values(r.maj.hig))
values(r.park) <- ifelse(values(r.park) == 0, NA, values(r.park))
values(r.pave) <- ifelse(values(r.pave) == 0, NA, values(r.pave))

p.pave.frac <-   
  #tm_shape(mc.hill$maricopa_hillshade) +
  #tm_raster(palette = "-Greys", 
  #          legend.show = F, 
  #          alpha = 0.1) +
  tm_shape(stack(r.loc.min, r.maj.hig, r.park, r.pave)) + #
  tm_raster(palette = my.palette,
            style="cont",
            #breaks= list(seq(0, 0.5, 0.10), seq(0, 0.75, 0.15), seq(0, 1, 0.20), seq(0, 1, 0.20)),
            legend.show = T,
            title = "Coverage Factor",
            colorNA = NULL) + # white
  #tm_shape(phx.labels) +
  #tm_text("name", 
  #        size = 0.5, 
  #        fontfamily = my.font,
  #        fontface = "bold.italic",
  #        alpha = 0.9,
  #        just = "center") +
  tm_compass(north = 0, 
             type = "4star", 
             size = 1.25,
             show.labels = 1, 
             color.dark = "#2a2a2a",
             position = c(0,0.05)) +
  tm_scale_bar(position = c(0.13,0),
               breaks = c(0,10,20),
               size = 0.7) +
  tm_shape(maricopa.cnty) +
  tm_borders(lwd = 1, 
             lty = "solid",
             col = "black",
             alpha = 0.75) +
  tm_fill(alpha = 0) +
  tm_facets(free.scales.text.size = F) +
  #tm_shape(uza.border) +
  #tm_borders(lwd = 0.3, 
  #           lty = "solid",
  #           col = "grey40",
  #           alpha = 0.4) +
  tm_layout(fontfamily = my.font, 
            #bg.color = "grey95",
            #legend.position = c("LEFT", "top"),
            legend.position = c(0, 0.33),
            outer.margins = c(0, 0, 0, 0),
            inner.margins = c(0.01, 0.01, 0.01, 0.01), # b, l, t, r
            between.margin = 0,
            #panel.label.fontface = 2,
            frame = T,
            #frame.lwgd = 1,
            legend.title.size = 1.25,
            legend.text.size = 1,
            legend.height = 0.65,
            panel.show = T,
            panel.labels = c("(a) Local + Collector Pavements","(b) Arterial + Highway Pavements", 
                             "(c) Parking Pavement","(d) All Pavements"),
            panel.label.size = 1.1,
            panel.label.height = 1.7,
            panel.label.bg.color = "#E0E0E0"
            #legend.outside = T)
  ) +
  tmap_options(max.raster = c(plot = 367000200, view = 367000200))

#p.all.flux.new
#tmap_save(p.all.flux.new, width = 5.5, units = "in", filename = here(paste0("figures/mean-daily-heat-flux-4grid-", run.name, "-", res / 3.28084, "m.png")))


tmap_save(p.pave.frac, insets_tm = list(p.inset,p.inset,p.inset,p.inset), dpi = 1000, width = 5.875, units = "in", 
          insets_vp = list(viewport(0.44, 0.365, width = 0.14, height = 0.14), # bottom left
                           viewport(0.94, 0.365, width = 0.14, height = 0.14), # bottom right
                           viewport(0.44, 0.865, width = 0.14, height = 0.14), # top left
                           viewport(0.94, 0.865, width = 0.14, height = 0.14)), # top right
          filename = here(paste0("figures/pave-coverage-4grid-", run.name, "-", res / 3.28084, "m.png")))




max(ifelse(values(r.all$avg.day.flux.roads) < 1 | values(r.all$avg.day.flux.park < 1), NA, 
           values(r.all$avg.day.flux.park) / values(r.all$avg.day.flux.roads)), na.rm = T)



