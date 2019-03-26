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

# IMPORT MODEL DATA

# first get latest updated output folders to pull model run data from most recent run (can change)
out.folders <- as.data.table(file.info(list.dirs(here("data/outputs/1D-heat-model-runs/"), 
                                                recursive = F)), keep.rownames = T)[(.N-3):(.N), rn]
# create plots?
should.plot <- "no" # "yes" or "no"

# loop through each folder of simulation runs
# and then loop through each run loading simulated pavement temperature data
# and summaring/ploting as necessary
for(f in 1:length(out.folders)){
  
  out.folder <- out.folders[f]
  
  # load model simulation metadata
  model.runs <- readRDS(paste0(out.folder,"/model_runs_metadata.rds"))
  layer.profiles <- readRDS(paste0(out.folder,"/layer_profiles.rds"))
  
  # load validation site data 
  valid.dates <- readRDS(here("data/aster/my-aster-data.rds")) # remote sensed temps at valiation sites on specified dates
  my.sites <- readRDS(paste0(out.folder,"/validation_sites.rds")) # other validation sites info 
  
  # create output directory for plots
  dir.create(paste0(out.folder,"/figures"), showWarnings = FALSE) # creates output figure folder if it doesn't already exist

  # add season
  model.runs[, season := factor(getSeason(end.day), levels = c("Spring", "Summer", "Fall", "Winter"))]

  for(run in 1:max(model.runs[, .N])){ # 
    tryCatch({  # catch and print errors, avoids stopping model run 
      
      # skip if run didn't complete, otherwise continue
      if(file.exists(paste0(out.folder,"/run_",run,"_output.rds")) == F){next}
      
      # read simulation data
      pave.time <- readRDS(paste0(out.folder,"/run_",run,"_output.rds"))
      
      # record number of days
      n.days <- model.runs[run.n == run, n.days]
      
      # minute and second variable to filter for weather obs
      pave.time[, hrs := hour(date.time)][, mins := minute(date.time)][, secs := second(date.time)] 
      
      # add air temp in deg c to modeled data
      pave.time[, air.temp.c := T.inf - 273.15]

      # calc Q1:3 quants and mean min max by depth
      pave.time[,`:=`(T.degC.75 = quantile(T.degC, probs = 0.75), 
                      T.degC.50 = quantile(T.degC, probs = 0.50),
                      T.degC.25 = quantile(T.degC, probs = 0.25),
                      T.degC.mean = mean(T.degC),
                      T.degC.min = min(T.degC),
                      T.degC.max = max(T.degC)),
                by = depth.m]
      
      # find nearest depths to 0.5m and 1.0m and record min, max and quantile temps
      my.depths <- unique(pave.time[, depth.m])
      my.depths <- my.depths[c(which.min(abs(my.depths - 1)), which.min(abs(my.depths - 0.5)))]
      
      model.runs[run.n == run, min.T_0.5m := unique(pave.time[depth.m %in% my.depths[1], T.degC.min]) ]
      #model.runs[run.n == run, p25.T_0.5m := unique(pave.time[depth.m %in% my.depths[1], T.degC.25]) ]
      #model.runs[run.n == run, p50.T_0.5m := unique(pave.time[depth.m %in% my.depths[1], T.degC.50]) ]
      model.runs[run.n == run, avg.T_0.5m := unique(pave.time[depth.m %in% my.depths[1], T.degC.mean]) ]
      #model.runs[run.n == run, p75.T_0.5m := unique(pave.time[depth.m %in% my.depths[1], T.degC.75]) ]
      model.runs[run.n == run, max.T_0.5m := unique(pave.time[depth.m %in% my.depths[1], T.degC.max]) ]
      
      model.runs[run.n == run, min.T_1.0m := unique(pave.time[depth.m %in% my.depths[2], T.degC.min]) ]
      #model.runs[run.n == run, p25.T_1.0m := unique(pave.time[depth.m %in% my.depths[2], T.degC.25]) ]
      #model.runs[run.n == run, p50.T_1.0m := unique(pave.time[depth.m %in% my.depths[2], T.degC.50]) ]
      model.runs[run.n == run, avg.T_1.0m := unique(pave.time[depth.m %in% my.depths[2], T.degC.mean]) ]
      #model.runs[run.n == run, p75.T_1.0m := unique(pave.time[depth.m %in% my.depths[2], T.degC.75]) ]
      model.runs[run.n == run, max.T_1.0m := unique(pave.time[depth.m %in% my.depths[2], T.degC.max]) ]
      
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
      
      # in addition to season (added earlier) add time of day (night/day) and combined season + day as factors for plotting later
      model.runs[run.n == run, daytime := factor(ifelse(hour(date.time.obs) %in% c(7:18), "Day", "Night"), levels = c("Day","Night"))]
      model.runs[run.n == run, day.sea := factor(paste(season, daytime), levels = c("Spring Day","Spring Night","Summer Day","Summer Night",
                                                                        "Fall Day","Fall Night","Winter Day","Winter Night"))]
      
      # create only surface data summary file if doesn't exist
      if(exists("all.surface.data", where = .GlobalEnv) == F){all.surface.data <- cbind(pave.time[node == 0], 
                                                                                        model.runs[run.n == run, .(run.n,SVF,albedo,pave.name,layer.profile,end.day,time.step,
                                                                                                                   batch.id,batch.name,station.name,valid.site,season,daytime,day.sea)])
      } else { # add to already created pave surface summary data
        all.surface.data <- rbind(all.surface.data, cbind(pave.time[node == 0], 
                                                          model.runs[run.n == run, .(run.n,SVF,albedo,pave.name,layer.profile,end.day,time.step,
                                                                                     batch.id,batch.name,station.name,valid.site,season,daytime,day.sea)]))
      }
      

      
      # begin plotting if desired
      if(should.plot == "yes"){
        
        # SURFACE TEMPS PLOT
        # specify plot info 
        p.surf.data <- pave.time[node == 0]
        min.x.surf <- min(p.surf.data$date.time, na.rm = T)
        max.x.surf <- ceiling_date(max(p.surf.data[!is.na(T.degC), date.time]), unit = "hours")
        min.y.surf <- 0 # solar rad is always 0 at night
        max.y.surf <- round(max(p.surf.data[, solar/10], na.rm = T), - 1) + 5
        surf.col <- c("Modeled \nSurface Temperature" = "#0D1B1E", "Observed \nAir Temperature" = "#10316B", "Observed \nSolar Radiation" = "#BF1C3D")
        surf.shp <- c("Modeled \nSurface Temperature" = 0, "Observed \nAir Temperature" = 5, "Observed \nSolar Radiation" = 2)
        surf.siz <- c("Modeled \nSurface Temperature" = 1, "Observed \nAir Temperature" = 1, "Observed \nSolar Radiation" = 1)
        
        p.surf <- (ggplot(data = p.surf.data) 
                   
                   # custom border
                   + geom_segment(aes(x = min.x.surf, y = min.y.surf, xend = max.x.surf, yend = min.y.surf))   # x border (x,y) (xend,yend)
                   + geom_segment(aes(x = min.x.surf, y = min.y.surf, xend = min.x.surf, yend = max.y.surf))  # y border (x,y) (xend,yend)
                   
                   # Observederved air temperature
                   + geom_line(aes(y = air.temp.c, x = date.time, color = "Observed \nAir Temperature", size = "Observed \nAir Temperature"))
                   + geom_point(aes(y = air.temp.c, x = date.time, color = "Observed \nAir Temperature", shape = "Observed \nAir Temperature"), 
                                data = p.surf.data[mins %in% c(0) & secs == 0,])
                   
                   # Observederved solar radiation
                   + geom_line(aes(y = solar/10, x = date.time, color = "Observed \nSolar Radiation", size = "Observed \nSolar Radiation"))
                   + geom_point(aes(y = solar/10, x = date.time, color = "Observed \nSolar Radiation", shape = "Observed \nSolar Radiation"), 
                                data = p.surf.data[mins %in% c(0) & secs == 0,])
                   
                   # modeled pavement surface temperature
                   + geom_line(aes(y = T.degC, x = date.time, color = "Modeled \nSurface Temperature", size = "Modeled \nSurface Temperature"))
                   + geom_point(aes(y = T.degC, x = date.time, color = "Modeled \nSurface Temperature", shape = "Modeled \nSurface Temperature"), 
                                data = p.surf.data[mins %in% c(0) & secs == 0,])
                   
                   # plot/axis titles & second axis for solar rad units
                   #+ ggtitle("Modeled Surface Pavement Temperature")
                   + labs(x = "Time of Day", y = "Temperature (deg C)")
                   + scale_color_manual(name = "", values = surf.col, labels = c("Modeled \nSurface Temperature", "Observed \nAir Temperature", "Observed \nSolar Radiation"))
                   + scale_shape_manual(name = "", values = surf.shp, labels = c("Modeled \nSurface Temperature", "Observed \nAir Temperature", "Observed \nSolar Radiation"))
                   + scale_size_manual(name = "", values = surf.siz, labels = c("Modeled \nSurface Temperature", "Observed \nAir Temperature", "Observed \nSolar Radiation"))
                   
                   # scales
                   + scale_x_datetime(expand = c(0,0), limits = c(min.x.surf, max.x.surf), date_breaks = "6 hours", date_labels = "%H")
                   + scale_y_continuous(expand = c(0,0), limits = c(min.y.surf, max.y.surf), breaks = seq(min.y.surf, max.y.surf, 20),
                                        sec.axis = sec_axis(~.*10, breaks = seq(min.y.surf * 10, max.y.surf * 10, 200), 
                                                            name = bquote('Solar Rad ('*W/m^2*')'))) # solar radiation axis
                   
                   # theme and formatting
                   + theme_minimal()
                   + theme(text = element_text(family = my.font, size = 12, colour = "black"),
                           axis.text = element_text(colour = "black"),
                           plot.margin = margin(t = 10, r = 20, b = 40, l = 40, unit = "pt"),
                           plot.title = element_text(hjust = 0.75),
                           #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                           axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                           axis.ticks.x = element_line(color = "black", size = 0.25),
                           axis.title.y.left = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                           axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
                           legend.position = c(0.5, -0.25),
                           legend.text = element_text(size = 10),
                           legend.title = element_text(size = 11),
                           legend.direction ="horizontal",
                           legend.background = element_blank())
        )
        
        #p.surf <- arrangeGrob(p.surf, bottom = textGrob(paste0("Pavement Type: ", model.runs[run.n == run, pave.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add pave name
        #p.surf <- arrangeGrob(p.surf, bottom = textGrob(paste0("Weather Station: ", model.runs[run.n == run, station.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add station name
        #ggsave(paste0("run_", run, "_1D-modeled-surface-temp.png"), p.surf, # save plot
        #       device = "png", path = paste0(out.folder,"/figures/"), scale = 1, width = 8, height = 5, dpi = 300, units = "in") 
        
        # DEPTH TEMPS PLOT
        # specify plot info
        my.node <- c(0,4,8)
        p.depth.data <- pave.time[node %in% my.node]
        min.x.depth <- min(p.depth.data$date.time, na.rm = T)
        max.x.depth <- ceiling_date(max(p.depth.data[!is.na(T.degC), date.time]), unit = "hours")
        min.y.depth <- round(min(p.depth.data[, T.degC] - 5), - 1) # round down to nearest multiple of 10
        max.y.depth <- round(max(p.depth.data[, T.degC] + 5), - 1) # round up to nearest multiple of 10
        
        # create different legend charateristics for plotting
        depth.names <- paste(unique(signif(p.depth.data$depth.m, 2) * 1000), "mm")
        depth.col <- c("#67000D", "#D42020", "#FC7050")
        depth.shp <- c(0:2)
        depth.siz <- c(0.6, 0.6, 0.6)
        names(depth.col) <- depth.names
        names(depth.shp) <- depth.names
        names(depth.siz) <- depth.names
        p.depth.data[, names := factor(paste(signif(depth.m, 2) * 1000, "mm"), levels = depth.names)]
        
        p.depth <- (ggplot(data = p.depth.data) 
                    
                    # custom border
                    + geom_segment(aes(x = min.x.depth, y = min.y.depth, xend = max.x.depth, yend = min.y.depth))   # x border (x,y) (xend,yend)
                    + geom_segment(aes(x = min.x.depth, y = min.y.depth, xend = min.x.depth, yend = max.y.depth))  # y border (x,y) (xend,yend)
                    
                    # line + point based on named factor of depth
                    + geom_line(aes(y = T.degC, x = date.time, color = names, size = names))
                    + geom_point(aes(y = T.degC, x = date.time, color = names, shape = names), data = p.depth.data[mins %in% c(0) & secs == 0,])
                    
                    # plot/axis titles & second axis for solar rad units
                    + labs(x = "Time of Day", y = "Temperature (deg C)")
                    + scale_color_manual(name = "Depth", values = depth.col)
                    + scale_shape_manual(name = "Depth", values = depth.shp)
                    + scale_size_manual(name = "Depth", values = depth.siz)
                    
                    # scales
                    + scale_x_datetime(expand = c(0,0), limits = c(min.x.depth, max.x.depth), date_breaks = "6 hours", date_labels = "%H")
                    + scale_y_continuous(expand = c(0,0), limits = c(min.y.depth, max.y.depth), breaks = seq(min.y.depth, max.y.depth, 5))
                    
                    # theme and formatting
                    + theme_minimal()
                    + theme(text = element_text(family = my.font, size = 12, colour = "black"),
                            axis.text = element_text(colour = "black"),
                            plot.margin = margin(t = 10, r = 20, b = 25, l = 40, unit = "pt"),
                            plot.title = element_text(hjust = 0.75),
                            #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                            axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                            axis.ticks.x = element_line(color = "black", size = 0.25),
                            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                            legend.position = c(0.45, -0.2),
                            legend.text = element_text(size = 10),
                            legend.title = element_text(size = 11),
                            legend.direction ="horizontal",
                            legend.background = element_blank())
        )
        
        #p.depth <- arrangeGrob(p.depth, bottom = textGrob(paste0("Pavement Type: ", model.runs[run.n == run, pave.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add pave name
        #p.depth <- arrangeGrob(p.depth, bottom = textGrob(paste0("Weather Station: ", model.runs[run.n == run, station.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add station name
        #ggsave(paste0("run_", run,"_1D-modeled-depth-temps.png"), p.depth, # save plot
        #       device = "png", path = paste0(out.folder,"/figures/"), scale = 1, width = 8, height = 5, dpi = 300, units = "in") 
        
        
        # TEMP BY DEPTH DYNAMIC PLOT
        
        # aggregate through time to unique by depth
        pave.time.agg <- unique(pave.time[, .(depth.m, T.degC.min, T.degC.25, T.degC.50, T.degC.mean, T.degC.75, T.degC.max)])
        
        # AREA DEPTH PLOT
        min.x.area <- 0
        max.x.area <- 1 #pave.time[, max(depth.m)] # (sum(layer.profiles[[model.runs$layer.profile[run]]]$thickness[1:2])) * 3
        min.y.area <- round(min(pave.time[, T.degC] - 5), - 1) # round down to nearest multiple of 10
        max.y.area <- round(max(pave.time[, T.degC] + 5), - 1) # round up to nearest multiple of 10
        
        boundary.nodes <- pave.time[layer == "boundary" & time.s == 0, node] # to mark the boundaries
        layers <- nrow(layer.profiles[[model.runs$layer.profile[run]]])
        
        # legend formating for different line type/color/size
        area.col <- c("Max Temperature" = "#BF1C3D", "Median Temperature" = "#0D1B1E", "Min Temperature" = "#10316B")
        area.typ <- c("Max Temperature" = "dotdash", "Median Temperature" = "solid", "Min Temperature" = "longdash")
        area.siz <- c("Max Temperature" = 1, "Median Temperature" = 1, "Min Temperature" = 1)
        
        p.area <- (ggplot(data = pave.time.agg[depth.m < max.x.area])
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
                   #+ theme_minimal()
                   + coord_flip() # flip and rotate x axis to get depth as 0 down 
                   + scale_x_reverse(expand = c(0,0), limits = c(max.x.area, min.x.area), breaks = seq(min.x.area, max.x.area, 0.1)) 
                   + scale_y_continuous(expand = c(0,0), limits = c(min.y.area, max.y.area), breaks = seq(min.y.area, max.y.area, 5)) 
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
                         max.x.area - (0.5 * (max.x.area - sum(layer.profiles[[model.runs$layer.profile[run]]]$thickness[1:(l-1)]))),
                         sum(layer.profiles[[model.runs$layer.profile[run]]]$thickness[1:l],-0.5*layer.profiles[[model.runs$layer.profile[run]]]$thickness[l]))
          
          p.area <- p.area + annotate("text", label = layer.profiles[[model.runs$layer.profile[run]]]$layer[l],
                                      x = an.x, y = 0.88 * max.y.area, family = my.font, angle = 0)
        }
        
        #p.area <- arrangeGrob(p.area, bottom = textGrob(paste0("Pavement Type: ", model.runs[run.n == run, pave.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add pave name
        #p.area <- arrangeGrob(p.area, bottom = textGrob(paste0("Weather Station: ", model.runs[run.n == run, station.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add station name
        #ggsave(paste0("run_", run, "_1D-modeled-pave-temp-box_0-0.3m.png"), p.area,  # save plot
        #       device = "png", path = paste0(out.folder,"/figures/"), scale = 1, width = 8, height = 5, dpi = 300, units = "in") 
        
        
        # HEAT FLUX PLOT
        # specify plot info
        p.flux.data <- pave.time[node == 0] 
        albedo <- model.runs$albedo[run]
        SVF <- model.runs$SVF[run]
        p.flux.data[, inc.sol := ((1 - albedo) * SVF * solar)]
        p.flux.data[, ref.sol := albedo * SVF * solar]
        p.flux.data <- melt(p.flux.data[, .(date.time, mins, secs, q.rad, q.cnv, ref.sol)], id.vars = c("date.time","mins","secs"), value.name = "flux")  # inc.sol
        #p.flux.data[, net.flux := inc.sol + q.rad + q.cnv]
        min.x.flux <- min(p.flux.data$date.time, na.rm = T)
        max.x.flux <- ceiling_date(max(p.flux.data[, date.time]), unit = "hours")
        min.y.flux <- round(min(p.flux.data[, flux] - 5), - 1) # round down to nearest multiple of 10
        max.y.flux <- round(max(p.flux.data[, flux] + 5), - 1) # round up to nearest multiple of 10
        
        # create different legend charateristics for plotting
        flux.names <- c("Reflected Solar Radiation", "Convection", "Infrared Radiation") # "Incoming Solar Radiation", 
        flux.col <- c("#10316B", "#28502E", "#F58A07")  # "#BF1C3D"
        flux.shp <- c(0,1,6)
        names(flux.col) <- flux.names
        names(flux.shp) <- flux.names
        p.flux.data[, names := factor(variable, labels = flux.names, levels = c("ref.sol", "q.cnv", "q.rad"))]   # "inc.sol", 
        
        p.flux <- (ggplot(data = p.flux.data) 
                   
                   # custom border
                   + geom_segment(aes(x = min.x.flux, y = min.y.flux, xend = max.x.flux, yend = min.y.flux))   # x border (x,y) (xend,yend)
                   + geom_segment(aes(x = min.x.flux, y = min.y.flux, xend = min.x.flux, yend = max.y.flux))  # y border (x,y) (xend,yend)
                   
                   # line + point based on named factor of flux
                   + geom_line(aes(y = flux, x = date.time, color = names))
                   + geom_point(aes(y = flux, x = date.time, color = names, shape = names), data = p.flux.data[mins %in% c(0) & secs == 0,])
                   
                   # plot/axis titles & second axis for solar rad units
                   #+ ggtitle("Modeled Surface Pavement Temperature")
                   + labs(x = "Time of Day", y = bquote('Heat Flux ('*W/m^2*')'))
                   + scale_color_manual(name = "", values = flux.col)
                   + scale_shape_manual(name = "", values = flux.shp)
                   
                   # scales
                   + scale_x_datetime(expand = c(0,0), limits = c(min.x.flux, max.x.flux), date_breaks = "6 hours", date_labels = "%H")
                   + scale_y_continuous(expand = c(0,0), limits = c(min.y.flux, max.y.flux), breaks = seq(min.y.flux, max.y.flux, 50))
                   
                   # theme and formatting
                   + theme_minimal()
                   + theme(text = element_text(family = my.font, size = 12, colour = "black"),
                           axis.text = element_text(colour = "black"),
                           plot.margin = margin(t = 10, r = 20, b = 25, l = 40, unit = "pt"),
                           plot.title = element_text(hjust = 0.75),
                           #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                           axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                           axis.ticks.x = element_line(color = "black", size = 0.25),
                           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                           legend.position = c(0.45, -0.2),
                           legend.text = element_text(size = 10),
                           legend.title = element_text(size = 11),
                           legend.direction ="horizontal",
                           legend.background = element_blank())
        )
        
        #p.flux <- arrangeGrob(p.flux, bottom = textGrob(paste0("Pavement Type: ", model.runs[run.n == run, pave.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add pave name
        #p.flux <- arrangeGrob(p.flux, bottom = textGrob(paste0("Weather Station: ", model.runs[run.n == run, station.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add station name
        #ggsave(paste0("run_", run,"_1D-modeled-flux-temps.png"), p.flux, # save plot
        #       device = "png", path = paste0(out.folder,"/figures/"), scale = 1, width = 8, height = 8, dpi = 300, units = "in") 
        
        
        p.all <- grid.arrange(grobs = list(p.surf, p.depth, p.flux, p.area), layout_matrix = rbind(c(1,2), c(3,4)))
        p.all <- arrangeGrob(p.all, bottom = textGrob(paste0("Pavement Type: ", model.runs[run.n == run, pave.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add pave name
        p.all <- arrangeGrob(p.all, bottom = textGrob(paste0("Weather Station: ", model.runs[run.n == run, station.name]), gp = gpar(fontfamily = my.font, fontsize = 11))) # add station name

        
        ggsave(paste0("run_", run,"_modeled-summary-plots.png"), p.all, # save plot
               device = "png", path = paste0(out.folder,"/figures/"), scale = 1.4, width = 8, height = 6, dpi = 300, units = "in") 
        
        
        
      } # end optional plotting
    }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")}) # print error message if model run had error
  }

  # write out summary data
  write.csv(model.runs, paste0(out.folder,"/model_runs_metadata_stats.csv"), row.names = F)
  saveRDS(model.runs, paste0(out.folder,"/model_runs_metadata_stats.rds"))
  
  # merge all model run data together
  if(exists("all.model.runs", where = .GlobalEnv) == F){all.model.runs <- model.runs[0]}
  all.model.runs <- rbind(all.model.runs, model.runs)
  
} 

# filter surface pave data to only the 3rd (last day), as that is all we will need
all.surface.data <- all.surface.data[time.s >= (60*60*24*2),]

# create summaries of error bu variables to identify best/worst 
RMSE = function(m, o){sqrt(mean((m - o)^2, na.rm = T))} # RMSE function
day.err <- model.runs[, .(RMSE = RMSE(Modeled, Observed), MAPE = mean(p.err, na.rm = T)), by = "end.day"][order(RMSE)]
site.err <- model.runs[, .(RMSE = RMSE(Modeled, Observed), MAPE = mean(p.err, na.rm = T)), by = "valid.site"][order(RMSE)]
layer.err <- model.runs[, .(RMSE = RMSE(Modeled, Observed), MAPE = mean(p.err, na.rm = T)), by = c("layer.profile", "batch.name")][order(RMSE)]
station.err <- model.runs[, .(RMSE = RMSE(Modeled, Observed), MAPE = mean(p.err, na.rm = T)), by = "station.name"][order(RMSE)]

# create metadata out folder
meta.folder <- here(paste0("data/outputs/run_metadata_", 
                           format(strptime(Sys.time(),format = "%Y-%m-%d %H:%M:%S"), 
                                  format = "%Y%m%d_%H%M%S")))
dir.create(meta.folder, showWarnings = FALSE) # creates output figure folder if it doesn't already exist

# save all run metadata
saveRDS(all.model.runs, paste0(meta.folder, "/stats_all_model_runs.rds"))
saveRDS(all.surface.data, paste0(meta.folder, "/all_pave_surface_data.rds"))
write.csv(day.err, paste0(meta.folder, "/error-day.csv"))
write.csv(site.err, paste0(meta.folder, "/error-site.csv"))
write.csv(layer.err, paste0(meta.folder, "/error-layer.csv"))
write.csv(station.err, paste0(meta.folder, "/error-station.csv"))

# combine with all previous runs if desired
#all.model.runs[, run.date.time := Sys.time()] # add run.date.time id for group of batched runs
#all.model.runs.a <- readRDS(here("data/outputs/stats_all_model_runs.rds")) # import all previous summary data
#all.model.runs.a <- rbind(all.model.runs.a, all.model.runs) # merge together
#saveRDS(all.model.runs.a, here("data/outputs/stats_all_model_runs.rds")) # resave
