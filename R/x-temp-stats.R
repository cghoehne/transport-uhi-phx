vheat <- readRDS(here("data/outputs/veh-heat-time-summarized.rds"))

vheat[, mean(mean.add.flux), by = class.type]
vheat[, max(max.add.flux), by = class.type]

quants$min.day.flux.roads
quants$avg.day.flux.roads
quants$max.day.flux.roads

quants$max.day.flux.hiway


pheat[, mean(mean.add.flux), by = pave.class]
pheat[, max(mean.add.flux), by = pave.class]

pheat[, max(max.add.flux), by = pave.class]

vheat[, mean(mean.add.flux), by = pave.class]
vheat[, max(mean.add.flux), by = pave.class]

vheat[, max(max.add.flux), by = pave.class]

# import runs for thermal variying thermal inertia
all.surface.data <- readRDS(here("data/outputs/run_metadata_20190524_112541_varied_TI/all_pave_surface_data.rds"))
all.model.runs <- readRDS(here("data/outputs/run_metadata_20190524_112541_varied_TI/stats_all_model_runs.rds"))

# add thermal inertia values for 1st and 2nd layer to surface data
all.surface.data <- merge(all.surface.data, unique(all.model.runs[, .(L1.TI, L2.TI), by = pave.name]), by = "pave.name")

# flux comparisons by type
all.surface.data[, mean(q.rad + q.cnv), by = c("pave.name", "batch.name", "albedo")][order(V1)]

# calc flux vars
all.surface.data[, inc.sol := ((1 - albedo) * SVF * solar)]
all.surface.data[, ref.sol := albedo * SVF * solar]
all.surface.data[, net.flux := -inc.sol + q.rad + q.cnv]

surface.data.ti <- all.surface.data[, .(batch.name = batch.name,
                                       out.flux = mean(q.rad + q.cnv),
                                       inc.sol = mean(inc.sol),
                                       net.flux = mean(net.flux),
                                       ref.sol = mean(ref.sol)),
                                   by = c("date.time", "pave.name", "SVF")]  
surface.data.ti <- unique(surface.data.b[SVF == 1.0,])

# mean outgoing heat fluxes by pavement type for thermal interia
surface.data.ti[, mean(out.flux), by = pave.name]
surface.data.ti[, group := substr(pave.name, nchar(pave.name) - 6, nchar(pave.name))]
surface.data.ti[, mean(out.flux), by = group]

# delay in max flux btwn high/low thermal intertia pavements
surface.data.ti[out.flux == surface.data.ti[, max(out.flux), by = pave.name][1, V1], date.time]
surface.data.ti[out.flux == surface.data.ti[, max(out.flux), by = pave.name][2, V1], date.time]

surface.data.ti[out.flux == surface.data.ti[, max(out.flux), by = pave.name][5, V1], date.time]
surface.data.ti[out.flux == surface.data.ti[, max(out.flux), by = pave.name][6, V1], date.time]


# calc the percent of heat flux from roads relative to roads + pavements
# exclude all cells with less than 1% area of roads or less than 10 vehicles
prct.road.flx <- ifelse(values(r.all$avg.all.roads) < 0.01 | values(r.all$daily.vkt) < 10, NA, values(r.all$avg.day.flux.roads) / (values(r.all$avg.day.flux.roads) + values(r.all$avg.day.flux.veh)))

min(prct.road.flx, na.rm = T)
mean(prct.road.flx, na.rm = T)
max(prct.road.flx, na.rm = T)

# total daily contributions to urban heat
tot.road.flx <- sum(values(r.all$avg.day.flux.roads))
tot.veh.flx <- sum(values(r.all$avg.day.flux.veh))
tot.park.flx <- sum(values(r.all$avg.day.flux.park))

#tot.road.flx / (tot.road.flx + tot.veh.flx) # percent of roads as roads + veh
#tot.park.flx / (tot.road.flx + tot.park.flx) # percents of park as pavements

# percents of total heat
tot.park.flx / (tot.road.flx + tot.park.flx + tot.veh.flx)
tot.road.flx / (tot.road.flx + tot.park.flx + tot.veh.flx)
tot.veh.flx / (tot.road.flx + tot.park.flx + tot.veh.flx)

all.roads <- ifelse(values(r.all$avg.all.roads) < 0.01, NA, values(r.all$avg.all.roads))
all.park <- ifelse(values(r.all$avg.all.park) < 0.01, NA, values(r.all$avg.all.park))

sum(all.roads, na.rm = T) / length(all.roads[!is.na(all.roads)])
sum(all.park, na.rm = T) / length(all.park[!is.na(all.park)])

sum(values(r.all$avg.all.park)) / length(r.all$avg.all.park)
sum(values(r.all$avg.all.roads)) / length(r.all$avg.all.roads)

# rush hour percents of total




veh.heat
names(veh.heat)[1501:1506] <- c("min.flux.veh.all.8.9am", "max.flux.veh.all.8.9am", "avg.flux.veh.all.8.9am",
                                 "min.flux.veh.all.5.6pm", "max.flux.veh.all.5.6pm", "avg.flux.veh.all.5.6pm")

tot.veh.am <- sum(values(veh.heat$avg.flux.veh.all.8.9am))
tot.pave.am <- sum(values(r.all$avg.pave)) * pheat[hour(date.time) %in% c(8), mean(mean.add.flux)]
tot.veh.pm <- sum(values(veh.heat$avg.flux.veh.all.5.6pm))
tot.pave.pm <- sum(values(r.all$avg.pave)) * pheat[hour(date.time) %in% c(17), mean(mean.add.flux)]

tot.veh.am / (tot.veh.am + tot.pave.am)
tot.veh.pm / (tot.veh.am + tot.pave.pm)

veh.am <- ifelse(values(r.all$avg.all.roads) < 0.01 | values(r.all$daily.vkt) < 1, NA, values(veh.heat$avg.flux.veh.all.8.9am))


# max possible percent of veh flux to to roadway
max(veh.am, na.rm = T) / (max(veh.am, na.rm = T) + pheat[hour(date.time) %in% c(8), mean(mean.add.flux)])


