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
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

# IMPORT MODEL DATA
all.model.runs <- readRDS(here("data/outputs/stats_all_model_runs.rds"))

# filter if necessary / as desired
model.runs <- all.model.runs[p.err <= 0.35]

# check rankings of RMSE to see under what circumstances temps were most accurate
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("end.day")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("valid.site")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("layer.profile", "batch.name")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("station.name")][order(V1)]

# check rankings of RMSE to see under what circumstances temps were most accurate
model.runs[!is.na(p.err), mean(p.err), by = c("end.day")][order(V1)]
model.runs[!is.na(p.err), mean(p.err), by = c("valid.site")][order(V1)]
model.runs[!is.na(p.err), mean(p.err), by = c("layer.profile", "batch.name")][order(V1)]
model.runs[!is.na(p.err), mean(p.err), by = c("station.name")][order(V1)]

###################
# create predicted vs modeled plots

for(p.name in unique(model.runs[, batch.name])){
  
  p.runs <- model.runs[!is.na(p.err) & batch.name == p.name,]
  
  min.x <- 0  # round(min(valids[,.(T.degC, T.degC.sat)] - 5), - 1)  
  max.x <- round(max(p.runs[,.(Modeled,Observed)], na.rm = T) + 5, - 1)  #valids.long[, temp]
  min.y <- min.x
  max.y <- max.x
  p.letter <- paste0("(", chartr("1234", "abcd", which(p.name == unique(model.runs[, batch.name]))), ")")
  
  # create different legend charateristics for plotting
  m.o.names <- levels(p.runs[, day.sea])
  m.o.shp <- c(rep(16, 4),rep(18, 4))
  m.o.col <- c("#0062DB", "#1E964E", "#CC0E2A", "#602E00", "#0062DB", "#1E964E", "#CC0E2A", "#602E00")
  names(m.o.shp) <- m.o.names
  names(m.o.col) <- m.o.names

  my.plot <- (ggplot(data = p.runs) 
              
              # custom border
              + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
              + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
              
              # points for modeled vs observed + ref line
              + geom_point(aes(y = Modeled, x = Observed, color = day.sea, shape = day.sea), size = 2.5)
              + geom_abline(intercept = 0, slope = 1)
              
              # plot/axis titles & second axis for solar rad units
              + labs(x = "Observed Surface Temperature (deg C)", y = "Modeled Surface Temperature (deg C)")
              + annotate("text", label = paste("RMSE =", signif(RMSE(p.runs[!is.na(Observed), Modeled], 
                                                                     p.runs[!is.na(Observed), Observed]), 3)), 
                         x = max.x * 0.85, y = max.y / 12, family = my.font)
              + annotate("text", label = paste0("MAPE = ", signif(mean(p.runs[, p.err]), 3) * 100, "%"), 
                         x = max.x * 0.85, y = max.y / 6, family = my.font)
              + scale_color_manual(name = "Season & Time of Day", values = m.o.col)
              + scale_shape_manual(name = "Season & Time of Day", values = m.o.shp)
              
              # scales
              + scale_x_continuous(expand = c(0,0), limits = c(min.x,max.x), breaks = seq(min.x,max.x,10))
              + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,10))
              
              # theme and formatting
              + theme_minimal()
              + theme(text = element_text(family = my.font, size = 10, colour = "black"),
                      axis.text = element_text(colour = "black"),
                      plot.margin = margin(t = 10, r = 10, b = 15, l = 10, unit = "pt"),
                      plot.title = element_text(hjust = 0.75),
                      axis.title.x = element_text(margin = margin(t = 4, r = 0, b = 0, l = 0)),
                      axis.ticks.x = element_line(color = "black", size = 0.25),
                      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                      legend.position = "none")
  )
  
  my.title <- textGrob(label = paste(p.letter, p.name),
                       x = unit(4, "mm"), y = unit(2, "mm"), hjust = 0, vjust = 0, # right and bottom align inside grob
                       gp = gpar(fontfamily = my.font, fontsize = 10, fontface = "bold"))
  my.plot <- arrangeGrob(my.plot, top = my.title) # title
  assign(paste0("my.plot.", unique(model.runs[batch.name == p.name, batch.id])), my.plot)
}

# legend
my.legend <- legendGrob(labels = m.o.names, nrow = 2, pch = m.o.shp, 
                        vgap = unit(2, "mm"),
                        gp = gpar(col = m.o.col, fill = m.o.col, size = 0.5, fontfamily = my.font, fontsize = 10))

# combine into multi-plot
my.plots <- lapply(unique(model.runs[, batch.id]), function(.x) get(paste0("my.plot.", .x)))
meta.plot <- grid.arrange(layout_matrix = rbind(c(1,2), c(3,4)), grobs = my.plots)
meta.plot <- arrangeGrob(meta.plot, bottom = my.legend)
ggsave("modeled-observed.png", meta.plot, # save plot
       device = "png", path = "figures", 
       scale = 1.5, width = 6, height = 5, dpi = 300, units = "in")


###############################
# HEAT FLUX PLOT

# import dataa
all.surface.data <- readRDS(here("data/outputs/all_pave_surface_data_20190319_172032.rds"))

# calc flux vars
all.surface.data[, inc.sol := ((1 - albedo) * SVF * solar)]
all.surface.data[, ref.sol := albedo * SVF * solar]
all.surface.data[, net.flux := -inc.sol + q.rad + q.cnv]

# summarize data
surface.data.a <- all.surface.data[, .(dt = time.s,
                                       out.flux = mean(q.rad + q.cnv),
                                       inc.sol = mean(inc.sol),
                                       net.flux = mean(net.flux),
                                       ref.sol = mean(ref.sol)),
                                   by = c("time.s", "batch.name", "season")]  #

surface.data.a[, date.time := as.POSIXct("2019-01-01 00:00:00 MST") + seconds(time.s)] # force date, will ignore 

# plot min/maxes
min.x.flux <- min(surface.data.a$date.time, na.rm = T)
max.x.flux <- ceiling_date(max(surface.data.a[, date.time]), unit = "hours")
min.y.flux <- round(min(surface.data.a[, out.flux] - 5), - 1) # round down to nearest multiple of 10
max.y.flux <- round(max(surface.data.a[, out.flux] + 5), - 1) # round up to nearest multiple of 10

# adjust y to by multiple of mult
min.y.flux <- ifelse(min.y.flux %% 20 == 0, min.y.flux, min.y.flux - 10)
max.y.flux <- ifelse(max.y.flux %% 20 == 0, max.y.flux, max.y.flux + 10)

# create different legend charateristics for plotting
p.names <- unique(surface.data.a[,batch.name]) # "Incoming Solar Radiation", 
p.col <- c("#0C120C","#E2CEA2")
#p.col <- c("#0C120C", "#0C120C", "#49473E","#E2CEA2") # asphalt, asphalt, conc, ground
p.shp <- c(1, 2, 32, 32)
names(p.col) <- p.names
names(p.shp) <- p.names
surface.data.a[, batch.name := as.factor(batch.name)]
#surface.data.a[, time := ]

p.flux.a <- (ggplot(data = surface.data.a) 
             
             # custom border
             + geom_segment(aes(x = min.x.flux, y = min.y.flux, xend = max.x.flux, yend = min.y.flux))   # x border (x,y) (xend,yend)
             + geom_segment(aes(x = min.x.flux, y = min.y.flux, xend = min.x.flux, yend = max.y.flux))  # y border (x,y) (xend,yend)
             
             # line + point based on named factor of flux
             + geom_line(aes(y = out.flux, x = date.time, color = batch.name), size = 2.5)
             #+ geom_point(aes(y = out.flux, x = date.time, color = batch.name, shape = batch.name), size = 3.5, data = surface.data.a[mins %in% c(0) & secs == 0,])
             #+ geom_ribbon(aes(ymax = max(out.flux), ymin = min(out.flux), x = date.time), fill = "grey50", alpha = 0.4)
             
             # plot/axis titles & second axis for solar rad units
             #+ ggtitle("Modeled Surface Pavement Temperature")
             + labs(x = "Time of Day", y = bquote('Outgoing Heat Flux ('*W/m^2*')'))
             + scale_color_manual(name = "", values = p.col)
             #+ scale_shape_manual(name = "", values = p.shp)
             + facet_wrap(~season)
             
             # scales
             + scale_x_datetime(expand = c(0,0), limits = c(min.x.flux, max.x.flux), date_breaks = "6 hours", date_labels = "%H") #
             + scale_y_continuous(expand = c(0,0), limits = c(min.y.flux, max.y.flux), breaks = seq(min.y.flux, max.y.flux, 40))
             
             # theme and formatting
             + theme_minimal()
             + theme(text = element_text(family = my.font, size = 12, colour = "black"), 
                     axis.text = element_text(colour = "black"),
                     plot.margin = margin(t = 10, r = 20, b = 40, l = 40, unit = "pt"),
                     plot.title = element_text(hjust = 0.75),
                     #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                     axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                     axis.ticks.x = element_line(color = "black", size = 0.25),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                     legend.position = c(0.45, -0.24),
                     #panel.grid.minor = element_line(color = "gray20"), # poster
                     #panel.grid.major = element_line(color = "gray17"), # poster
                     legend.text = element_text(size = 12),
                     legend.title = element_text(size = 11),
                     legend.direction ="vertical",
                     legend.background = element_blank())
)

p.flux.a

ggsave(here("figures/heat-flux-diff.png"), p.flux.a, # save plot
       device = "png", scale = 1, width = 8, height = 6, dpi = 300, units = "in") 



