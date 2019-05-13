# clear space and allocate memory
gc()
memory.limit(size = 50000) 
options(scipen = 999) # prevent scientific notation when printing

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint)
}

# load all other dependant packages from the local repo
.libPaths(paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1"))
library(zoo)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
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

# define folder for data reterival (automaticlly take most recent folder with "run_metadata" in it)
folder <- as.data.table(file.info(list.dirs(here("data/outputs/"), recursive = F)), 
                        keep.rownames = T)[grep("run_metadata", rn),][order(ctime)][.N, rn]
#folder[max(grep("run_metadata", rn)), rn] # alt
#folder[grep("run_metadata", rn),][.N] # alt

# IMPORT MODEL DATA
all.model.runs <- readRDS(paste0(folder, "/stats_all_model_runs.rds"))

# filter out any unrealistic pavements
#all.model.runs <- all.model.runs[!(pave.name %in% c("Portland Cement Concrete #5","Whitetopped Asphalt #5")),]
#all.model.runs <- all.model.runs[!(run.n %in% c(40:42) & batch.id == "A")]

# filter if necessary / as desired
model.runs <- all.model.runs  #[p.err <= 0.35 & !is.na(p.err),] # remove poor performers or NAs if there are any 

# summary stats for paper
model.runs[, RMSE(Modeled, Observed)]
all.model.runs[, RMSE(Modeled, Observed)]
model.runs[, mean(p.err, na.rm = T)]
all.model.runs[, mean(p.err, na.rm = T)]

############################
# MODELED VS OBSERVED PLOT #
############################

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
model.runs[, new.name := batch.name]
model.runs[batch.name == "Concrete Pavements", new.name := "Concrete & Whitetopped Asphalt Pavements"]
model.runs[batch.name == "Whitetopped Asphalt Pavements", new.name := "Concrete & Whitetopped Asphalt Pavements"]
model.runs[batch.name == "Asphalt Overlays on PCC Pavements", new.name := "Asphalt Overlaid PCC Pavements"]


# create new ordered titles with a:d for facet titles
p.names <- unique(model.runs[, new.name])
model.runs[new.name == p.names[2], new.name.t := paste("(a)", p.names[2])]
model.runs[new.name == p.names[4], new.name.t := paste("(b)", p.names[4])]
model.runs[new.name == p.names[3], new.name.t := paste("(c)", p.names[3])]
model.runs[new.name == p.names[1], new.name.t := paste("(d)", p.names[1])]
model.runs[, new.name.t := as.factor(new.name.t)]

# RMSE & MAPE
model.runs[new.name == p.names[1], RMSE.batch := RMSE(Modeled, Observed)]
model.runs[new.name == p.names[2], RMSE.batch := RMSE(Modeled, Observed)]
model.runs[new.name == p.names[3], RMSE.batch := RMSE(Modeled, Observed)]
model.runs[new.name == p.names[4], RMSE.batch := RMSE(Modeled, Observed)]
model.runs[, RMSE.batch := paste("RMSE =", signif(RMSE.batch, 3))]

model.runs[new.name == p.names[1], MAPE.batch := mean(p.err, na.rm = T)]
model.runs[new.name == p.names[2], MAPE.batch := mean(p.err, na.rm = T)]
model.runs[new.name == p.names[3], MAPE.batch := mean(p.err, na.rm = T)]
model.runs[new.name == p.names[4], MAPE.batch := mean(p.err, na.rm = T)]
model.runs[, MAPE.batch := paste0("MAPE = ", signif(MAPE.batch, 3) * 100, "%")]

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
                    strip.text.x = element_text(size = 12, hjust = 0, vjust = 1, face = "bold"),
                    legend.position = c(0.45, -0.175),
                    legend.text = element_text(size = 12),
                    legend.direction ="horizontal",
                    legend.background = element_blank())
            )

# save plot
dir.create(paste0(folder, "/figures/"), showWarnings = F)
ggsave(paste0(folder, "/figures/modeled-observed", 
              format(strptime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), 
                     format = "%Y%m%d_%H%M%S"),".png"), my.plot.f, 
       device = "png", scale = 1.5, width = 6, height = 5, dpi = 300, units = "in")


##################
# HEAT FLUX PLOT #
##################

# import data
all.surface.data <- readRDS(paste0(folder, "/all_pave_surface_data.rds"))

# flux comparisons by type
all.surface.data[, mean(q.rad + q.cnv), by = c("pave.name", "batch.name", "albedo")][order(V1)]

# new names
all.surface.data[, new.name := batch.name]
all.surface.data[batch.name == "Concrete Pavements", new.name := "Concrete & Whitetopped Asphalt Pavements"]
all.surface.data[batch.name == "Whitetopped Asphalt Pavements", new.name := "Concrete & Whitetopped Asphalt Pavements"]
all.surface.data[batch.name == "Asphalt Overlays on PCC Pavements", new.name := "Asphalt Overlaid PCC Pavements"]


# calc flux vars
all.surface.data[, inc.sol := ((1 - albedo) * SVF * solar)]
all.surface.data[, ref.sol := albedo * SVF * solar]
all.surface.data[, net.flux := -inc.sol + q.rad + q.cnv]

# filter out any unrealistic pavements
#all.surface.data <- all.surface.data[!(pave.name %in% c("Portland Cement Concrete #5","Whitetopped Asphalt #5")),]
#all.surface.data <- all.surface.data[!(run.n %in% c(40:42) & batch.id == "A")]

# summarize data
surface.data.a <- all.surface.data[, .(hrs, mins, secs,
                                       dt = time.s,
                                       out.flux = mean(q.rad + q.cnv),
                                       inc.sol = mean(inc.sol),
                                       net.flux = mean(net.flux),
                                       ref.sol = mean(ref.sol)),
                                   by = c("time.s", "new.name", "season")]  

# force date for date.time for easier manipulation in ggplot, will ignore date
surface.data.a[, date.time := as.POSIXct("2019-01-01 00:00:00 MST") + seconds(time.s)] 

# plot min/maxes
min.x.flux <- min(surface.data.a$date.time, na.rm = T) 
max.x.flux <- ceiling_date(max(surface.data.a[, date.time]), unit = "hours")
min.y.flux <- 0 #round(min(surface.data.a[, out.flux] - 5), - 1) # round down to nearest multiple of 10
max.y.flux <- round(max(surface.data.a[, out.flux] + 5), - 1) # round up to nearest multiple of 10

# adjust y to by multiple of mult
min.y.flux <- ifelse(min.y.flux %% 20 == 0, min.y.flux, min.y.flux - 10)
max.y.flux <- ifelse(max.y.flux %% 20 == 0, max.y.flux, max.y.flux + 10)

# create different legend charateristics for plotting
surface.data.a[, new.name.f := factor(new.name, levels = c(p.names[4], p.names[3], p.names[1], p.names[2]))]
p.col <- c("#0C120C", "#0C120C", "#918D77", "#C1912A")  # order is HVA, LVA, C, BG
p.shp <- c(1, 32, 2, 32)  # order is HVA, BG, LVA, C
p.lty <- c("longdash", "solid", "solid", "twodash") # order is HVA, LVA, C, BG
names(p.col) <- p.names
names(p.shp) <- p.names
names(p.lty) <- p.names


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
             + labs(x = "Time of Day", y = bquote('Mean Outgoing Heat Flux ('*W/m^2*')'))
             + scale_color_manual(name = "", values = p.col, guide = guide_legend(reverse = T))
             + scale_linetype_manual(name = "", values = p.lty, guide = guide_legend(reverse = T))
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
                     plot.margin = margin(t = 5, r = 10, b = 85, l = 10, unit = "pt"),
                     panel.spacing.x = unit(7, "mm"),
                     panel.spacing.y = unit(4, "mm"),
                     axis.text.x = element_text(vjust = -1),
                     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)),
                     axis.ticks = element_line(color = "grey80", size = 0.28),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                     strip.text.x = element_text(size = 12, hjust = 0, vjust = 1, face = "bold"),
                     legend.position = c(0.525, -0.4),
                     legend.key.width = unit(30, "mm"),
                     legend.text = element_text(size = 10),
                     legend.spacing.y = unit(1, "mm"),
                     legend.direction ="vertical")
)

# save plot
ggsave(paste0(folder, "/figures/heat-flux-diff", 
              format(strptime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"),
                     format = "%Y%m%d_%H%M%S"),".png"), p.flux.a, 
       device = "png", scale = 1, width = 7, height = 4.5, dpi = 300, units = "in") 

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

# raw veh data
iflow <- fread(here("data/icarus/full_flow.csv"))
setnames(iflow, "V1", "id")

# import veh hourly raster data
veh.heat <- readRDS(here(paste0("data/outputs/rasters/master-veh-time-heat-", run.name, "-", res / 3.28084, "m.rds")))

# import daily avg raster data
r.all <- readRDS(here(paste0("data/outputs/rasters/master-pave-veh-heat-", run.name, "-", res / 3.28084, "m.rds")))

# calculate mean veh heat flux over road area by class and time step
# mean flux by layer
#veh.agg <- 

mean(values(veh.heat$vkt.local.74))
quantile(values(veh.heat$vkt.local.74))

# to prevent issues with extrememly high estaimtes of waste heat, force very small fractional areas to 0 (force less 5% to 0)
#values(r.all$avg.local.road) * ((res / 3.28084)^2) # area (m2) of local roads by cell local
vheat <- data.table("var" = c("local", "minor", "major", "highway"))
vheat[, paste0("t",1:(length(iflow)-1)) := 0]

for(i in 1:(length(iflow)-1)){
  vheat[var == "local", ]
}
test.min <- ifelse(values(r.all$min.hiway.road) < 0.05, 0, values(r.all$min.day.flux.hiway.veh) / values(r.all$min.hiway.road))
quantile(test.min)
test.max <- ifelse(values(r.all$max.hiway.road) < 0.05, 0, values(r.all$max.day.flux.hiway.veh) / values(r.all$max.hiway.road))
quantile(test.max)
test.avg <- (test.min + test.max) / 2
quantile(test.avg)

ifelse(values(veh.heat[[i]]) < 0.05, 0, values(veh.heat[[i]]) / values(veh.heat[[i]]))

plot(r.all[[c("avg.all.roads", "avg.all.park", "daily.vkt", "total.avg.day.flux")]])

#all.veh.heat <- rbind(cbind(veh.heat, "(a) Highway"), 
#                      cbind(veh.heat, "(b) Arterial"),
#                      cbind(veh.heat, "(c) Collector"),
#                      cbind(veh.heat, "(d) Local"))
#setnames(all.veh.heat, "V2", "id")

# convert day fraction into date.time (will ignore date)
all.veh.heat[, date.time := as.POSIXct("2019-01-01 00:00:00 MST") + seconds(day.frac * 24 * 60 * 60)]

# plot min/maxes
min.x.veh <- min(all.veh.heat$date.time, na.rm = T) 
max.x.veh <- ceiling_date(max(all.veh.heat[, date.time]), unit = "hours")
min.y.veh <- round(min(all.veh.heat[, min.flux] - 5), - 1) # round down to nearest multiple of 10
max.y.veh <- 240 #round(max(all.veh.heat[, max.flux] + 5), - 1) # round up to nearest multiple of 10

# adjust y to by multiple of mult
min.y.veh <- ifelse(min.y.veh %% 20 == 0, min.y.veh, min.y.veh - 10)
max.y.veh <- ifelse(max.y.veh %% 20 == 0, max.y.veh, max.y.veh + 10)

# create different legend charateristics for plotting
v.names <- c("(a) Highway", "(b) Arterial", "(c) Collector", "(d) Local")
all.veh.heat[, id := factor(id, levels = v.names)]
v.col <- c("#220901", "#621708", "#941B0C", "#BC3908")  
v.lty <- c("solid", "twodash","solid", "twodash")
names(v.col) <- v.names
names(v.lty) <- v.names

# temp scale down magnitudes
all.veh.heat[id == v.names[2], `:=`(min.flux = min.flux * 0.75, 
                                    mean.flux = mean.flux * 0.75, 
                                    max.flux = max.flux * 0.75)]
all.veh.heat[id == v.names[3], `:=`(min.flux = min.flux * 0.5, 
                                    mean.flux = mean.flux * 0.5, 
                                    max.flux = max.flux * 0.5)]
all.veh.heat[id == v.names[4], `:=`(min.flux = min.flux * 0.25, 
                                    mean.flux = mean.flux * 0.25, 
                                    max.flux = max.flux * 0.25)]

# create plot
p.flux.v <- (ggplot(data = all.veh.heat) 
             
             # custom border
             + geom_segment(aes(x = min.x.veh, y = min.y.veh, xend = max.x.veh, yend = min.y.veh))   # x border (x,y) (xend,yend)
             + geom_segment(aes(x = min.x.veh, y = min.y.veh, xend = min.x.veh, yend = max.y.veh))  # y border (x,y) (xend,yend)
             
             # line + point based on named factor of flux
             + geom_ribbon(aes(ymin = min.flux, ymax = max.flux, x = date.time),fill = "grey50")
             + geom_line(aes(y = mean.flux, x = date.time, color = id, linetype = id), size = 1)
             
             # plot/axis titles & second axis for solar rad units
             + labs(x = "Time of Day", y = bquote('Vehicle Waste Heat Flux ('*W/m^2*')'))
             + scale_color_manual(name = "", values = v.col) # , guide = guide_legend(reverse = T)
             + scale_linetype_manual(name = "", values = v.lty) # , guide = guide_legend(reverse = T)
             + facet_wrap(~id)
             
             # scales
             + scale_x_datetime(expand = c(0,0), limits = c(min.x.veh, max.x.veh), date_breaks = "3 hours", date_labels = "%H") #
             + scale_y_continuous(expand = c(0,0), limits = c(min.y.veh, max.y.veh), breaks = seq(min.y.veh, max.y.veh, 40))
             #+ guides(col = guide_legend(ncol = 2)) # two rows in legend
             
             # theme and formatting
             + theme_minimal()
             + theme(text = element_text(family = my.font, size = 12, colour = "black"), 
                     axis.text = element_text(colour = "black"),
                     plot.margin = margin(t = 5, r = 10, b = 15, l = 10, unit = "pt"), #b = 85
                     panel.spacing.x = unit(7, "mm"),
                     panel.spacing.y = unit(4, "mm"),
                     axis.text.x = element_text(vjust = -1),
                     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)),
                     axis.ticks = element_line(color = "grey50", size = 0.28),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                     strip.text.x = element_text(size = 12, hjust = 0, vjust = 1, face = "bold"),
                     legend.position = "none", #c(0.525, -0.255),
                     legend.key.width = unit(30, "mm"),
                     legend.text = element_text(size = 10),
                     legend.spacing.y = unit(1, "mm"),
                     legend.direction ="vertical")
)

# save plot
ggsave(paste0(folder, "/figures/veh-heat-flux-diff.png"), p.flux.v, 
       device = "png", scale = 1, width = 7, height = 6, dpi = 300, units = "in") 

####################
# RASTER HEAT MAPS #
####################

# import labels & borders 
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # Maricopa UZA (non-buffered) in EPSG:2223
phx.labels <- shapefile(here("data/shapefiles/other/phx_metro_labels.shp"))

# set 0 to NA to ignore in diveraging color palette
values(r.all$avg.day.flux.veh) <- ifelse(values(r.all$avg.day.flux.veh) < 1, NA, values(r.all$avg.day.flux.veh))
values(r.all$avg.day.flux.park) <- ifelse(values(r.all$avg.day.flux.park) < 1, NA, values(r.all$avg.day.flux.park))
values(r.all$avg.day.flux.roads) <- ifelse(values(r.all$avg.day.flux.roads) < 1, NA, values(r.all$avg.day.flux.roads))
values(r.all$total.avg.day.flux) <- ifelse(values(r.all$total.avg.day.flux) < 1, NA, values(r.all$total.avg.day.flux))

#mc.hill <- brick("C:/Users/cghoehne/Dropbox (ASU)/Data and Tools/GIS/AZ Files/maricopa_hillshade/maricopa_hillshade.tif")
#mc.hill <- crop(mc.hill, extent(r.all), snap="out")
#saveRDS(mc.hill, here("data/maricopa-hillshade.rds"))
mc.hill <- readRDS(here("data/maricopa-hillshade.rds")) # Maricopa County Hillashade EPSG 2223, cropped to extent of rasters

# crop rasters to uza 
r.veh.flux <- crop(r.all$avg.day.flux.veh, uza.border, snap = "out")
r.park.flux <- crop(r.all$avg.day.flux.park, uza.border, snap = "out")
r.road.flux <- crop(r.all$avg.day.flux.roads, uza.border, snap = "out")
r.all.flux <- crop(r.all$total.avg.day.flux, uza.border, snap = "out")

# define global tmap parameters
my.asp <- 1.3
my.palette <- rev(rainbow(255, end = 0.6)) # start = 0.5,
title.p <- c(0.18, 0.97)

# create individual plots 
p.road.flux <-   
  tm_shape(mc.hill) +
  tm_raster(palette = "-Greys", 
            legend.show = F, 
            alpha = 0.4) +
  tm_shape(r.road.flux) +
  tm_raster(palette = my.palette,
            style="cont",
            breaks = c(1, seq(5, 45, 5)),
            legend.show = T,
            title = "Heat Flux\n(W/m\u00B2)",
            colorNA = NULL) + # white
  tm_shape(uza.border) +
  tm_borders(lwd = 0.2, 
             lty = "solid",
             col = "grey40",
             alpha = 0.2) +
  #tm_scale_bar(position = c(0.2,0.10),
  #             #breaks = c(0,5,10,15,20),
  #             size = 1,
  #             color.light = "grey85") +
  #tm_compass(north = 0, 
  #           type = "4star", 
  #           size = 1.5,
  #           show.labels = 1, 
  #           position = c(0.9,0.85)) +
  tm_shape(phx.labels) +
  tm_text("name", 
          size = .5, 
          fontfamily = my.font,
          fontface = "bold.italic",
          just = "center") +
  #tm_shape(hist.loc) +
  #tm_markers(shape = tmap_icons(file = paste0("Projects/Maricopa Parking/Figures/histograms/",map.yr,"_tot_space_hist.png"),
  #                              just = c(1.2,1.2), 
  #                              keep.asp = T, 
  #                              width = 900, 
  #                              height = 900), size = 16) +
  tm_layout(fontfamily = my.font, 
            fontface = "italic", 
            bg.color = "grey95",
            title.size = 1.1, 
            title = c("(a) Roadway Pavement"),
            title.position = title.p,
            legend.position = c(0.014,0.35),
            outer.margins = c(0, 0, 0, 0),
            inner.margins = c(0.01, 0.01, 0.01, 0.01), # b, l, t, r
            frame = F,
            #frame.lwd = 1,
            asp = my.asp,
            legend.height = -0.45,
            #legend.width = -0.06,
            #legend.title.size = 1.5, 
            legend.text.size = 1) +
  tmap_options(max.raster = c(plot = 102951200, view = 102951200))

p.park.flux <-   
  tm_shape(mc.hill) +
  tm_raster(palette = "-Greys", 
            legend.show = F, 
            alpha = 0.4) +
  tm_shape(r.park.flux) +
  tm_raster(palette = my.palette, # start = 0.5,
            style="cont",
            breaks = c(1, seq(5, 50, 5)),
            legend.show = T,
            title = "Heat Flux\n(W/m\u00B2)",
            colorNA = NULL) + # white
  tm_shape(uza.border) +
  tm_borders(lwd = 0.2, 
             lty = "solid",
             col = "grey40",
             alpha = 0.7) +
  #tm_scale_bar(position = c(0.2,0.10),
  #             #breaks = c(0,5,10,15,20),
  #             size = 0.60,
  #             color.light = "grey85") +
  tm_compass(north = 0, 
             type = "4star", 
             size = 1.5,
             show.labels = 1, 
             position = c(0.8,0.8)) +
  tm_shape(phx.labels) +
  tm_text("name", 
          size = .5, 
          fontfamily = my.font,
          fontface = "bold.italic",
          just = "center") +
  #tm_shape(hist.loc) +
  #tm_markers(shape = tmap_icons(file = paste0("Projects/Maricopa Parking/Figures/histograms/",map.yr,"_tot_space_hist.png"),
  #                              just = c(1.2,1.2), 
  #                              keep.asp = T, 
  #                              width = 900, 
  #                              height = 900), size = 16) +
  tm_layout(fontfamily = my.font, 
            fontface = "italic", 
            bg.color = "grey95",
            title.size = 1.1, 
            title = c("(b) Parking Pavement"),
            title.position = title.p,
            legend.position = c(0.014,0.35),
            outer.margins = c(0, 0, 0, 0),
            inner.margins = c(0.01, 0.01, 0.01, 0.01), # b, l, t, r
            frame = F,
            #frame.lwd = 1,
            asp = my.asp,
            legend.height = -0.45,
            #legend.width = -0.06,
            #legend.title.size = 1.5, 
            legend.text.size = 1) +
  tmap_options(max.raster = c(plot = 102951200, view = 102951200))

p.veh.flux <-   
  tm_shape(mc.hill) +
  tm_raster(palette = "-Greys", 
            legend.show = F, 
            alpha = 0.4) +
  tm_shape(r.veh.flux) +
  tm_raster(palette = my.palette, #"YlOrRd", #"-Spectral", #rev(heat.colors(255))
            style="cont",
            breaks = c(1, seq(2.5, 20, 2.5)),
            legend.show = T,
            title = "Heat Flux\n(W/m\u00B2)",
            colorNA = NULL) + # white
  tm_shape(uza.border) +
  tm_borders(lwd = 0.2, 
             lty = "solid",
             col = "grey40",
             alpha = 0.7) +
  tm_scale_bar(position = c(0,0),
               #breaks = c(0,5,10,15,20),
               size = 0.7,
               color.light = "grey85") +
  #tm_compass(north = 0, 
  #           type = "4star", 
  #           size = 1.25,
  #           show.labels = 1, 
  #           position = c(0.9,0.85)) +
  tm_shape(phx.labels) +
  tm_text("name", 
          size = .5, 
          fontfamily = my.font,
          fontface = "bold.italic",
          just = "center") +
  #tm_shape(hist.loc) +
  #tm_markers(shape = tmap_icons(file = paste0("Projects/Maricopa Parking/Figures/histograms/",map.yr,"_tot_space_hist.png"),
  #                              just = c(1.2,1.2), 
  #                              keep.asp = T, 
  #                              width = 900, 
  #                              height = 900), size = 16) +
  tm_layout(fontfamily = my.font, 
            fontface = "italic", 
            bg.color = "grey95",
            title.size = 1.1, 
            title = c("(c) Vehicles"),
            title.position = title.p,
            legend.position = c(0.014,0.35),
            outer.margins = c(0, 0, 0, 0),
            inner.margins = c(0.01, 0.01, 0.01, 0.01), # b, l, t, r
            frame = F,
            #frame.lwd = 1,
            asp = my.asp,
            legend.height = -0.45,
            #legend.width = -0.06,
            #legend.title.size = 1.5, 
            legend.text.size = 1) +
  tmap_options(max.raster = c(plot = 102951200, view = 102951200))

p.all.flux <-   
  tm_shape(mc.hill) +
  tm_raster(palette = "-Greys", 
            legend.show = F, 
            alpha = 0.4) +
  tm_shape(r.all.flux) +
  tm_raster(palette = my.palette, #"YlOrRd", #"-Spectral", #rev(heat.colors(255))
            style="cont",
            breaks = c(1, seq(10, 60, 10)),
            legend.show = T,
            title = "Heat Flux\n(W/m\u00B2)",
            colorNA = NULL) + # white
  tm_shape(uza.border) +
  tm_borders(lwd = 0.2, 
             lty = "solid",
             col = "grey40",
             alpha = 0.7) +
  #tm_scale_bar(position = c(0.2,0.10),
  #             #breaks = c(0,5,10,15,20),
  #             size = 1,
  #             color.light = "grey85") +
  #tm_compass(north = 0, 
  #           type = "4star", 
  #           size = 1.5,
  #           show.labels = 1, 
  #           position = c(0.9,0.85)) +
  tm_shape(phx.labels) +
  tm_text("name", 
          size = .5, 
          fontfamily = my.font,
          fontface = "bold.italic",
          just = "center") +
  #tm_shape(hist.loc) +
  #tm_markers(shape = tmap_icons(file = paste0("Projects/Maricopa Parking/Figures/histograms/",map.yr,"_tot_space_hist.png"),
  #                              just = c(1.2,1.2), 
  #                              keep.asp = T, 
  #                              width = 900, 
  #                              height = 900), size = 16) +
  tm_layout(fontfamily = my.font, 
            fontface = "italic", 
            bg.color = "grey95",
            title.size = 1.1, 
            title = c("(d) Vehicles + Pavements"), 
            title.position = title.p,
            legend.position = c(0.014,0.35),
            outer.margins = c(0, 0, 0, 0),
            inner.margins = c(0.01, 0.01, 0.01, 0.01), # b, l, t, r
            frame = F,
            #frame.lwd = 1,
            asp = my.asp,
            legend.height = -0.45,
            #legend.width = -0.06,
            #legend.title.size = 1.5, 
            legend.text.size = 1) +
  tmap_options(max.raster = c(plot = 102951200, view = 102951200))

p.grid.flux <- tmap_arrange(p.road.flux, p.park.flux, p.veh.flux, p.all.flux,
                            outer.margins = c(0,0,0,0), asp = NA)


tmap_save(p.grid.flux, filename = here(paste0("figures/mean-daily-heat-flux-4grid-", run.name, "-", res / 3.28084, "m.png")))
# morning rush would be from: hour = 7.92 am to 8.88 am (6:54:12 am to 8:52:48 am); V34:V37
# evening rush would be from: hour = 5.04 pm  to 6 pm (5:02:24 pm to 6:00:00 pm); V72:V75
