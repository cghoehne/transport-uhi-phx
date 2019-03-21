# clear space and allocate memory
gc()
memory.limit(size = 56000) 
t.start <- Sys.time() # start script timestamp

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(zoo, lib.loc = lib.path, quietly = T)
library(lubridate, lib.loc = lib.path, quietly = T)
library(ggplot2, lib.loc = lib.path, quietly = T)
library(grid, lib.loc = lib.path, quietly = T)
library(gridExtra, lib.loc = lib.path, quietly = T)
library(data.table, lib.loc = lib.path, quietly = T)
library(here, lib.loc = lib.path, quietly = T)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # archive date for all used packages (besides checkpoint itself!)
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

# load windows fonts and store as string
windowsFonts(Century=windowsFont("TT Century Gothic"))
windowsFonts(Times=windowsFont("TT Times New Roman"))
my.font <- "Century"

# create RMSE function
RMSE = function(m, o){sqrt(mean((m - o)^2, na.rm = T))}

# define folder for data reterival
folder <- paste0(here("data/outputs"),"/run_metadata_20190320_192039/")

# IMPORT MODEL DATA
all.model.runs <- readRDS(paste0(folder, "stats_all_model_runs.rds"))


# filter if necessary / as desired
model.runs <- all.model.runs[p.err <= 0.35 & !is.na(p.err),] # remove poor performers or NAs if there are any 

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
model.runs[batch.name == "High Volume Asphalt Pavements", new.name := "High Stress Asphalt"]
model.runs[batch.name == "Low Volume Asphalt Pavements", new.name := "Low Stress Asphalt"]
model.runs[batch.name == "Concrete and Composite Concrete-Asphalt Pavements", new.name := "Concrete & Whitetopped Asphalt"]

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
ggsave("modeled-observed.png", my.plot.f, 
       device = "png", path = "figures", 
       scale = 1.5, width = 6, height = 5, dpi = 300, units = "in")


##################
# HEAT FLUX PLOT #
##################

# import dataa
all.surface.data <- readRDS(paste0(folder, "all_pave_surface_data.rds"))

# new names
all.surface.data[, new.name := batch.name]
all.surface.data[batch.name == "High Volume Asphalt Pavements", new.name := "High Stress Asphalt"]
all.surface.data[batch.name == "Low Volume Asphalt Pavements", new.name := "Low Stress Asphalt"]
all.surface.data[batch.name == "Concrete and Composite Concrete-Asphalt Pavements", new.name := "Concrete & Whitetopped Asphalt"]

# calc flux vars
all.surface.data[, inc.sol := ((1 - albedo) * SVF * solar)]
all.surface.data[, ref.sol := albedo * SVF * solar]
all.surface.data[, net.flux := -inc.sol + q.rad + q.cnv]

# summarize data
surface.data.a <- all.surface.data[, .(hrs, mins, secs,
                                       dt = time.s,
                                       out.flux = mean(q.rad + q.cnv),
                                       inc.sol = mean(inc.sol),
                                       net.flux = mean(net.flux),
                                       ref.sol = mean(ref.sol)),
                                   by = c("time.s", "new.name", "season")]  #

# force date for date.time for easier manipulation in ggplot, will ignore date
surface.data.a[, date.time := as.POSIXct("2019-01-01 00:00:00 MST") + seconds(time.s)] 

# plot min/maxes
min.x.flux <- min(surface.data.a$date.time, na.rm = T) 
max.x.flux <- ceiling_date(max(surface.data.a[, date.time]), unit = "hours")
min.y.flux <- round(min(surface.data.a[, out.flux] - 5), - 1) # round down to nearest multiple of 10
max.y.flux <- round(max(surface.data.a[, out.flux] + 5), - 1) # round up to nearest multiple of 10

# adjust y to by multiple of mult
min.y.flux <- ifelse(min.y.flux %% 20 == 0, min.y.flux, min.y.flux - 10)
max.y.flux <- ifelse(max.y.flux %% 20 == 0, max.y.flux, max.y.flux + 10)

# create different legend charateristics for plotting
surface.data.a[, new.name.f := factor(new.name, levels = c(p.names[2], p.names[4], p.names[3], p.names[1]))]
p.col <- c("#0C120C", "#C1912A", "#0C120C", "#918D77")  # order is HVA, BG, LVA, C
p.shp <- c(1, 32, 2, 32)  # order is HVA, BG, LVA, C
p.lty <- c("longdash", "solid", "solid", "twodash") # order is HVA, BG, LVA, C
names(p.col) <- p.names
names(p.shp) <- p.names
names(p.lty) <- p.names


# create better labels for season factor
setattr(surface.data.a$season,"levels", c("(a) Spring", "(b) Summer", "(c) Fall", "(d) Winter"))

# create plot
p.flux.a <- (ggplot(data = surface.data.a) 
             
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
                     legend.position = c(0.525, -0.255),
                     legend.key.width = unit(30, "mm"),
                     legend.text = element_text(size = 10),
                     legend.spacing.y = unit(1, "mm"),
                     legend.direction ="vertical")
)

# save plot
ggsave(here("figures/heat-flux-diff.png"), p.flux.a, 
       device = "png", scale = 1, width = 7, height = 6, dpi = 300, units = "in") 
