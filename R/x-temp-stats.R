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
