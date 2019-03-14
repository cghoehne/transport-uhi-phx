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

# first get latest updated output folder to pull model run data from most recent run (can change)
out.folder <- as.data.table(file.info(list.dirs(here("data/outputs/1D-heat-model-runs/"), recursive = F)), keep.rownames = T)[ctime == max(ctime), rn] # last changed
#out.folder <- paste0(here("data/outputs/1D-heat-model-runs"), "/20190311_171304_model_outputs") 

#20190311_171312_model_outputs

# load model simulation metadata
model.runs <- readRDS(paste0(out.folder,"/model_runs_metadata.rds"))
layer.profiles <- readRDS(paste0(out.folder,"/layer_profiles.rds"))

# load validation site data 
valid.dates <- readRDS(here("data/aster/my-aster-data.rds")) # remote sensed temps at valiation sites on specified dates
my.sites <- readRDS(paste0(out.folder,"/validation_sites.rds")) # other validation sites info 

# add station.name to model summary
model.runs <- merge(model.runs, unique(my.sites[,.(station.name,Location)]), by.x = "valid.site", by.y = "Location", all.x = T)

# create plots?
should.plot <- "yes" # "yes" or "no"

# create output directory for plots
dir.create(paste0(out.folder,"/figures"), showWarnings = FALSE) # creates output figure folder if it doesn't already exist

# loop through loading simulated pavement temperature data for run 
# and summaring/ploting as necessary
for(run in 1:max(model.runs[, .N])){
  tryCatch({  # catch and print errors, avoids stopping model run 
  
  # skip if run didn't complete, otherwise continue
  if(file.exists(paste0(out.folder,"/run_",run,"_output.rds")) == F){next}
    
  # read simulation data
  pave.time <- readRDS(paste0(out.folder,"/run_",run,"_output.rds"))
  
  # record avg max temp and final day max temp at surface
  n.days <- model.runs[run.n == run, n.days]

  # minute and second variable to filter for weather obs
  pave.time[, mins := minute(date.time)][, secs := second(date.time)] 
  
  # add air temp in deg c to modeled data
  pave.time[, air.temp.c := T.inf - 273.15]
  
  # obtain date.time from validation to match pavement time to
  obs.valid <- valid.dates[site == model.runs[run.n == run, valid.site] & date(date.time) == model.runs[run.n == run, end.day], ]
  
  
  # retrieve modeled pavement data for relevant variables at nearest time
  model.valid <- pave.time[which(abs(difftime(pave.time[,date.time], obs.valid[, date.time])) == 
                               min(abs(difftime(pave.time[,date.time], obs.valid[, date.time])))), ]
  
  # store info in model run metadata
  model.runs[run.n == run, date.time.obs := obs.valid[, date.time]]
  model.runs[run.n == run, date.time.mod := model.valid[node == 0, date.time]]
  model.runs[run.n == run, dif.sec := abs(difftime(date.time.obs, date.time.mod, units = "sec"))]
  model.runs[run.n == run, Modeled := model.valid[node == 0, T.degC]]
  model.runs[run.n == run, Observed := obs.valid[, LST]]
  model.runs[run.n == run, error := abs(Modeled - Observed)]
  model.runs[run.n == run, p.err := error / Observed]
  model.runs[run.n == run, air.temp.c := model.valid[node == 0, air.temp.c]]

  # begin plotting if desired
  if(should.plot == "yes"){
    
    # SURFACE TEMPS PLOT
    # specify plot info 
    p1.data <- pave.time[node == 0]
    min.x <- min(p1.data$date.time, na.rm = T)
    max.x <- ceiling_date(max(p1.data[!is.na(T.degC), date.time]), unit = "hours")
    min.y <- 0 # solar rad is always 0 at night
    max.y <- round(max(p1.data[, solar/10], na.rm = T), - 1) + 5
    surf.col <- c("Modeled Pavement \nSurface Temperature" = "#0D1B1E", "Observed Air Temperature" = "#10316B", "Observed Solar Radiation" = "#BF1C3D")
    surf.shp <- c("Modeled Pavement \nSurface Temperature" = 0, "Observed Air Temperature" = 4, "Observed Solar Radiation" = 2)
    surf.siz <- c("Modeled Pavement \nSurface Temperature" = 1.25, "Observed Air Temperature" = 0.75, "Observed Solar Radiation" = 0.75)
    
    p.surf <- (ggplot(data = p1.data) 
               
           # custom border
           + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
           + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
           
           # Observederved air temperature
           + geom_line(aes(y = air.temp.c, x = date.time, color = "Observed Air Temperature", size = "Observed Air Temperature"))
           + geom_point(aes(y = air.temp.c, x = date.time, color = "Observed Air Temperature", shape = "Observed Air Temperature"), data = p1.data[mins %in% c(0,30) & secs == 0,])
           
           # Observederved solar radiation
           + geom_line(aes(y = solar/10, x = date.time, color = "Observed Solar Radiation", size = "Observed Solar Radiation"))
           + geom_point(aes(y = solar/10, x = date.time, color = "Observed Solar Radiation", shape = "Observed Solar Radiation"), data = p1.data[mins %in% c(0,30) & secs == 0,])
           
           # modeled pavement surface temperature
           + geom_line(aes(y = T.degC, x = date.time, color = "Modeled Pavement \nSurface Temperature", size = "Modeled Pavement \nSurface Temperature"))
           + geom_point(aes(y = T.degC, x = date.time, color = "Modeled Pavement \nSurface Temperature", shape = "Modeled Pavement \nSurface Temperature"))
           
           # plot/axis titles & second axis for solar rad units
           #+ ggtitle("Modeled Surface Pavement Temperature")
           + labs(x = "Time of Day", y = "Temperature (deg C)")
           + scale_color_manual(name = "", values = surf.col, labels = c("Modeled Pavement \nSurface Temperature", "Observed Air Temperature", "Observed Solar Radiation"))
           + scale_shape_manual(name = "", values = surf.shp, labels = c("Modeled Pavement \nSurface Temperature", "Observed Air Temperature", "Observed Solar Radiation"))
           + scale_size_manual(name = "", values = surf.siz, labels = c("Modeled Pavement \nSurface Temperature", "Observed Air Temperature", "Observed Solar Radiation"))
           
           # scales
           + scale_x_datetime(expand = c(0,0), limits = c(min.x,max.x), date_breaks = "6 hours")
           + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,20),
                                sec.axis = sec_axis(~.*10, breaks = seq(min.y*10, max.y*10, 200), name = bquote('Solar Rad ('*W/m^2*')'))) # solar radiation axis
          
           # theme and formatting
           + theme_minimal()
           + theme(text = element_text(family = my.font, size = 12, colour = "black"),
                   axis.text = element_text(colour = "black"),
                   plot.margin = margin(t = 10, r = 20, b = 40, l = 40, unit = "pt"),
                   plot.title = element_text(hjust = 0.75),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                   axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                   axis.ticks.x = element_line(color = "black", size = 0.25),
                   axis.title.y.left = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                   axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
                   legend.position = c(0.5, -0.575),
                   legend.direction ="horizontal",
                   legend.background = element_blank())
           )

    p.surf <- arrangeGrob(p.surf, bottom = textGrob(paste0("Pavement Type: ", model.runs[run.n == run, pave.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add pave name
    p.surf <- arrangeGrob(p.surf, bottom = textGrob(paste0("Weather Station: ", model.runs[run.n == run, station.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add station name
    ggsave(paste0("run_", run, "_1D-modeled-surface-temp.png"), p.surf, # save plot
           device = "png", path = paste0(out.folder,"/figures/"), scale = 1, width = 8, height = 5, dpi = 300, units = "in") 
    
    # DEPTH TEMPS PLOT
    # specify plot info
    my.node <- c(0,4,8)
    p1.data <- pave.time[node %in% my.node]
    min.x <- min(p1.data$date.time, na.rm = T)
    max.x <- ceiling_date(max(p1.data[!is.na(T.degC), date.time]), unit = "hours")
    min.y <- round(min(p1.data[, T.degC] - 5), - 1) # round down to nearest multiple of 10
    max.y <- round(max(p1.data[, T.degC] + 5), - 1) # round up to nearest multiple of 10
    
    # create different legend charateristics for plotting
    depth.names <- paste(unique(signif(p1.data$depth.m, 2) * 1000), "mm")
    depth.col <- c("#67000D", "#D42020", "#FC7050")
    depth.shp <- c(0:2)
    depth.siz <- c(0.6, 0.6, 0.6)
    names(depth.col) <- depth.names
    names(depth.shp) <- depth.names
    names(depth.siz) <- depth.names
    p1.data[, names := factor(paste(signif(depth.m, 2) * 1000, "mm"), levels = depth.names)]
    
    p.depth <- (ggplot(data = p1.data) 
               
               # custom border
               + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
               + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)

               # line + point based on named factor of depth
               + geom_line(aes(y = T.degC, x = date.time, color = names, size = names))
               + geom_point(aes(y = T.degC, x = date.time, color = names, shape = names), data = p1.data[mins %in% c(0,30) & secs == 0,])

               # plot/axis titles & second axis for solar rad units
               + labs(x = "Time of Day", y = "Temperature (deg C)")
               + scale_color_manual(name = "Pavement Depth", values = depth.col)
               + scale_shape_manual(name = "Pavement Depth", values = depth.shp)
               + scale_size_manual(name = "Pavement Depth", values = depth.siz)
               
               # scales
               + scale_x_datetime(expand = c(0,0), limits = c(min.x,max.x), date_breaks = "6 hours")
               + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,5))
               
               # theme and formatting
               + theme_minimal()
               + theme(text = element_text(family = my.font, size = 12, colour = "black"),
                       axis.text = element_text(colour = "black"),
                       plot.margin = margin(t = 10, r = 20, b = 25, l = 40, unit = "pt"),
                       plot.title = element_text(hjust = 0.75),
                       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                       axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                       axis.ticks.x = element_line(color = "black", size = 0.25),
                       axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                       legend.position = c(0.45, -0.55), 
                       legend.direction ="horizontal",
                       legend.background = element_blank())
      )
    
    p.depth <- arrangeGrob(p.depth, bottom = textGrob(paste0("Pavement Type: ", model.runs[run.n == run, pave.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add pave name
    p.depth <- arrangeGrob(p.depth, bottom = textGrob(paste0("Weather Station: ", model.runs[run.n == run, station.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add station name
    ggsave(paste0("run_", run,"_1D-modeled-depth-temps.png"), p.depth, # save plot
           device = "png", path = paste0(out.folder,"/figures/"), scale = 1, width = 8, height = 5, dpi = 300, units = "in") 
    
    
    # TEMP BY DEPTH DYNAMIC PLOT
    # calc Q1:3 quants and mean min max
    pave.time[,`:=`(T.degC.75 = quantile(T.degC, probs = 0.75), 
                    T.degC.50 = quantile(T.degC, probs = 0.50),
                    T.degC.25 = quantile(T.degC, probs = 0.25),
                    T.degC.mean = mean(T.degC),
                    T.degC.min = min(T.degC),
                    T.degC.max = max(T.degC)),
              by = depth.m]
    
    # aggregate through time to unique by depth
    pave.time.agg <- unique(pave.time[, .(depth.m, T.degC.min, T.degC.25, T.degC.50, T.degC.mean, T.degC.75, T.degC.max)])
    
    # Deviation (boxplot) of single scenario by nodes
    min.x <- 0
    max.x <- 1 #pave.time[, max(depth.m)] # (sum(layer.profiles[[model.runs$layer.profile[run]]]$thickness[1:2])) * 3
    min.y <- round(min(pave.time[, T.degC] - 5), - 1) # round down to nearest multiple of 10
    max.y <- round(max(pave.time[, T.degC] + 5), - 1) # round up to nearest multiple of 10

    boundary.nodes <- pave.time[layer == "boundary" & time.s == 0, node] # to mark the boundaries
    layers <- nrow(layer.profiles[[model.runs$layer.profile[run]]])

    # legend formating for different line type/color/size
    area.col <- c("Max Temperature" = "#BF1C3D", "Median Temperature" = "#0D1B1E", "Min Temperature" = "#10316B")
    area.typ <- c("Max Temperature" = "dotdash", "Median Temperature" = "solid", "Min Temperature" = "longdash")
    area.siz <- c("Max Temperature" = 1, "Median Temperature" = 1, "Min Temperature" = 1)
    
    p.area <- (ggplot(data = pave.time.agg[depth.m < max.x])
               + geom_ribbon(aes(ymin = T.degC.25, ymax = T.degC.75, x = depth.m, fill = "IQR"))
               + geom_line(aes(x = depth.m, y = T.degC.min, color = names(area.col[3]), size = names(area.siz[3]), linetype = names(area.typ[3]))) # min
               + geom_line(aes(x = depth.m, y = T.degC.50, color = names(area.col[2]), size = names(area.siz[2]), linetype = names(area.typ[2]))) # median
               + geom_line(aes(x = depth.m, y = T.degC.max, color = names(area.col[1]), size = names(area.siz[1]), linetype = names(area.typ[1]))) # max
               + geom_vline(xintercept = unique(pave.time[node %in% boundary.nodes & depth.m != 0, depth.m]), linetype = "dotted")
               + labs(x = "Pavement Depth (m)", y = "Temperature (deg C)")
               + scale_color_manual(name = "", values = area.col)
               + scale_size_manual(name = "", values = area.siz)
               + scale_linetype_manual(name = "", values = area.typ)
               + guides(color = guide_legend(reverse = T), size = guide_legend(reverse = T), linetype = guide_legend(reverse = T))
               + scale_fill_manual("", values = c("IQR" = "grey70"))
               + theme_light()
               + coord_flip() # flip and rotate x axis to get depth as 0 down 
               + scale_x_reverse(expand = c(0,0), limits = c(max.x, min.x), breaks = seq(min.x, max.x, 0.1)) 
               + scale_y_continuous(expand = c(0,0), limits = c(min.y, max.y), breaks = seq(min.y, max.y, 5)) 
               + theme(text = element_text(family = my.font, size = 12, colour = "black"),
                       axis.text = element_text(colour = "black"),
                       plot.margin = margin(t = 10, r = 20, b = 15, l = 40, unit = "pt"),
                       axis.title.x = element_text(vjust = -2),
                       axis.title.y = element_text(vjust = 5),
                       legend.position = c(0.2, 0.1),
                       legend.background = element_blank(),
                       legend.box.background = element_blank(),
                       legend.spacing.y = unit(-4, "lines"),
                       legend.spacing.x = unit(1, "mm"),
                       legend.key.size = unit(8, "mm"))
    )
    
    for(l in 1:layers){ # add text annotating each layer
      an.x <- ifelse(l == max(layers), # if last layer, different label x position
                     max.x - (0.5 * (max.x - sum(layer.profiles[[model.runs$layer.profile[run]]]$thickness[1:(l-1)]))),
                     sum(layer.profiles[[model.runs$layer.profile[run]]]$thickness[1:l],-0.5*layer.profiles[[model.runs$layer.profile[run]]]$thickness[l]))
      
      p.area <- p.area + annotate("text", label = layer.profiles[[model.runs$layer.profile[run]]]$layer[l],
                                          x = an.x, y = 0.88 * max.y, family = my.font, angle = 0)
    }
    
    p.area <- arrangeGrob(p.area, bottom = textGrob(paste0("Pavement Type: ", model.runs[run.n == run, pave.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add pave name
    p.area <- arrangeGrob(p.area, bottom = textGrob(paste0("Weather Station: ", model.runs[run.n == run, station.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add station name
    ggsave(paste0("run_", run, "_1D-modeled-pave-temp-box_0-0.3m.png"), p.area,  # save plot
           device = "png", path = paste0(out.folder,"/figures/"), scale = 1, width = 8, height = 5, dpi = 300, units = "in") 
    
    
    # HEAT FLUX PLOT
    # specify plot info
    p1.data <- pave.time[node == 0] 
    albedo <- model.runs$albedo[run]
    SVF <- model.runs$SVF[run]
    p1.data[, inc.sol := ((1 - albedo) * SVF * solar)]
    p1.data[, ref.sol := albedo * SVF * solar]
    p1.data <- melt(p1.data[, .(date.time, mins, secs, q.rad, q.cnv, inc.sol, ref.sol)], id.vars = c("date.time","mins","secs"), value.name = "flux")
    #p1.data[, net.flux := inc.sol + q.rad + q.cnv]
    min.x <- min(p1.data$date.time, na.rm = T)
    max.x <- ceiling_date(max(p1.data[, date.time]), unit = "hours")
    min.y <- round(min(p1.data[, flux] - 5), - 1) # round down to nearest multiple of 10
    max.y <- round(max(p1.data[, flux] + 5), - 1) # round up to nearest multiple of 10
    
    # create different legend charateristics for plotting
    flux.names <- c("Incoming Solar Radiation", "Reflected Solar Radiation", "Convection", "Infrared Radiation")
    flux.col <- c("#BF1C3D", "#10316B", "#28502E", "#F58A07")
    flux.shp <- c(0:3)
    names(flux.col) <- flux.names
    names(flux.shp) <- flux.names
    p1.data[, names := factor(variable, labels = flux.names, levels = c("inc.sol", "ref.sol", "q.cnv", "q.rad"))]
    
    p.flux <- (ggplot(data = p1.data) 
                
                # custom border
                + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
                + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
                
                # line + point based on named factor of flux
                + geom_line(aes(y = flux, x = date.time, color = names))
                + geom_point(aes(y = flux, x = date.time, color = names, shape = names), data = p1.data[mins %in% c(0,30) & secs == 0,])
                
                # plot/axis titles & second axis for solar rad units
                #+ ggtitle("Modeled Surface Pavement Temperature")
                + labs(x = "Time of Day", y = "Heat Flux (W/m^2)", parse = T)
                + scale_color_manual(name = "Heat Flux", values = flux.col)
                + scale_shape_manual(name = "Heat Flux", values = flux.shp)
                
                # scales
                + scale_x_datetime(expand = c(0,0), limits = c(min.x,max.x), date_breaks = "6 hours")
                + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,50))
                
                # theme and formatting
                + theme_minimal()
                + theme(text = element_text(family = my.font, size = 12, colour = "black"),
                        axis.text = element_text(colour = "black"),
                        plot.margin = margin(t = 10, r = 20, b = 25, l = 40, unit = "pt"),
                        plot.title = element_text(hjust = 0.75),
                        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                        axis.ticks.x = element_line(color = "black", size = 0.25),
                        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                        legend.position = c(0.45, -0.25), 
                        legend.direction ="horizontal",
                        legend.background = element_blank())
    )
    
    p.flux <- arrangeGrob(p.flux, bottom = textGrob(paste0("Pavement Type: ", model.runs[run.n == run, pave.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add pave name
    p.flux <- arrangeGrob(p.flux, bottom = textGrob(paste0("Weather Station: ", model.runs[run.n == run, station.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add station name
    ggsave(paste0("run_", run,"_1D-modeled-flux-temps.png"), p.flux, # save plot
           device = "png", path = paste0(out.folder,"/figures/"), scale = 1, width = 8, height = 8, dpi = 300, units = "in") 
    
    
  } # end optional plotting
  }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")}) # print error message if model run had error
}

# add winter/summer night/day ids to model.meta data
# define function to assign season
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates (b/c leap yr)
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

model.runs[, season := factor(getSeason(date.time.mod), levels = c("Winter","Spring","Summer","Fall"))]
model.runs[, daytime := factor(ifelse(hour(date.time.mod) %in% c(7:18), "Day", "Night"), levels = c("Day","Night"))]
model.runs[, day.sea := factor(paste(season, daytime), levels = c("Winter Day","Spring Day","Summer Day","Fall Day",
                                                                   "Winter Night","Spring Night","Summer Night","Fall Night"))]

# write out data
write.csv(model.runs, paste0(out.folder,"/model_runs_metadata_stats.csv"), row.names = F) # output model run metadata

###################
# create predicted vs modeled plots

min.x <- 0  # round(min(valids[,.(T.degC, T.degC.sat)] - 5), - 1)  
max.x <- round(max(model.runs[,.(Modeled,Observed)], na.rm = T) + 5, - 1)  #valids.long[, temp]
min.y <- min.x
max.y <- max.x

# create different legend charateristics for plotting
m.o.names <- levels(model.runs[, day.sea])
m.o.shp <- c(rep(16, 4),rep(18, 4))
m.o.col <- c("#0062DB", "#1E964E", "#CC0E2A", "#602E00", "#0062DB", "#1E964E", "#CC0E2A", "#602E00")
names(m.o.shp) <- m.o.names
names(m.o.col) <- m.o.names

#p.name <- "Low Volume Asphalt Pavements" 
#p.name <- "High Volume Asphalt Pavements" 
#p.name <- "Concrete and Composite Concrete-Asphalt Pavements" 
p.name <- "Bare Ground / Desert Soil" 

p.mod_obs <- (ggplot(data = model.runs[!is.na(p.err)]) 
            
            # custom border
            + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
            + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
            
            # points for modeled vs observed + ref line
            + geom_point(aes(y = Modeled, x = Observed, color = day.sea, shape = day.sea), size = 2)
            + geom_abline(intercept = 0, slope = 1)
            
            # plot/axis titles & second axis for solar rad units
            + labs(x = "Observed Surface Temperature (deg C)", y = "Modeled Surface Temperature (deg C)")
            + annotate("text", label = paste("RMSE =", signif(RMSE(model.runs[!is.na(Observed), Modeled], 
                                                                   model.runs[!is.na(Observed), Observed])
                                                              , 3)), x = max.x * 0.85, y = 5, family = my.font)
            + scale_color_manual(name = "Season & Time of Day", values = m.o.col)
            + scale_shape_manual(name = "Season & Time of Day", values = m.o.shp)
            
            # scales
            + scale_x_continuous(expand = c(0,0), limits = c(min.x,max.x), breaks = seq(min.x,max.x,5))
            + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,5))
            
            # theme and formatting
            + theme_minimal()
            + theme(text = element_text(family = my.font, size = 12, colour = "black"),
                    axis.text = element_text(colour = "black"),
                    plot.margin = margin(t = 10, r = 10, b = 15, l = 10, unit = "pt"),
                    plot.title = element_text(hjust = 0.75),
                    axis.text.x = element_text(vjust = 1, hjust = 1),
                    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                    axis.ticks.x = element_line(color = "black", size = 0.25),
                    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                    legend.position = c(0.85, 0.35), 
                    #legend.direction ="horizontal",
                    legend.background = element_blank())
)
p.mod_obs

p.mod_obs <- arrangeGrob(p.mod_obs, bottom = textGrob(paste0("Material Type: ", p.name), 
                                                      gp = gpar(fontfamily = my.font))) # add pave name
ggsave(paste0(gsub(" ", "-", gsub(" / ", "-", tolower(p.name))), ".png"), p.mod_obs, # save plot
       device = "png", path = paste0(out.folder,"/figures"), 
       scale = 1, width = 6, height = 5, dpi = 300, units = "in") 

# check rankings of RMSE to see under what circumstances temps were most accurate
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("end.day")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("valid.site")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("layer.profile")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("station.name")][order(V1)]
