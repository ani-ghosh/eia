library(tmap)
library(tmaptools)
library(sf)
library(raster)
library(terra)
library(stars)
library(geodata)
library(rvest)

# https://keen-swartz-3146c4.netlify.app/

tmap_options(max.categories = 200)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

# low resolution country boundaries
vdir <- "G:/My Drive/work/ciat/cg-prioritization"

# this after combining Central American Dry Corridor with Dixon maps
cgv <- shapefile(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))

# country
iso <- "COD"

# bit of cleaning
cgv$FORMAL_ <- NULL
cgv$ECONOMY <- NULL
cgv$INCOME_ <- NULL
cgv$ar_sqkm <- NULL
names(cgv)[names(cgv) == "frmng_s"] <- "farming_system"
names(cgv)[names(cgv) == "cgregin"] <- "cgregion"
cgv$farming_system <- gsub("[[:digit:]]+","", cgv$farming_system)
cgv$farming_system <- gsub("\\.","", cgv$farming_system)
cgv$farming_system <- trimws(cgv$farming_system)

fs <- cgv[cgv$ISO_A3 %in% iso,]

# country boundary
v1 <- raster::getData("GADM", country = iso, level = 1, 
                     path = file.path(datadir, "input/boundary/country"))

v <- raster::getData("GADM", country = iso, level = 2, 
                     path = file.path(datadir, "input/boundary/country"))

vt <- v[v$TYPE_2 == "Territoire",]
vt$ID <- as.numeric(as.factor((vt$NAME_2)))
vt$legend <- paste(vt$ID, ":", vt$NAME_2)

# # read the list of 145 territories table from wikipedia
# url <- "https://en.wikipedia.org/wiki/List_of_territories_of_the_Democratic_Republic_of_the_Congo"
# 
# lt <- read_html(url) %>% 
#   html_node(xpath = "//*[@id='mw-content-text']/div[1]/table[2]") %>% 
#   html_table(fill = T)
# lt <- lt[1:145,]

cpt <- shapefile("G:/My Drive/work/ciat/eia/DRC/data/drc/capitals/capitals.shp")

set.seed(1)
om <- tm_shape(v1) +
  tm_borders(lwd = 2, col = "#feb24c", lty = "solid") +
  tm_shape(vt) +
  tm_borders(lwd = 0.5, col = "#bd0026") +
  # tm_text("ID", size = 0.5) +
  tm_shape(v1) +
  tm_text("NAME_1", size = 0.5, auto.placement = TRUE, bg.color = "white", bg.alpha = 1) +
  tm_shape(cpt) +
  tm_dots(col="Capital", palette = "red", size = 0.1, shape = 16, legend.show = FALSE) +
  tm_add_legend("line", col = c("#feb24c", "#bd0026"), lwd = c(2, 0.5),
                lty = c("solid", "solid"), labels = c("Province", "Terriotry"),
                title = "Administrative")  + 
  tm_scale_bar(width = 0.4, lwd = 0.3, size = 0.5) +
  tm_compass(type = "arrow", size = 2, position = c("left", "top"))

p1 <- tm_shape(fs) + 
  tm_polygons(col = "farming_system", palette = c("#1f78b4", "#fdbf6f", "#33a02c", "#8dd3c7", "#ffff33", "#e5d8bd"),
              title = "Farming System", border.alpha = 0) 
p1m <- p1 + om +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 0.8,
            inner.margins=c(0.1,0.15,0.05,0.03))

tmap_save(p1m, filename =  file.path(datadir, "DRC_farming_system_territory.png"),
          width = 5, height = 5, dpi = 300)


# soil
soil <- st_read("G:/My Drive/work/ciat/eia/DRC/data/drc/fao_soils/fao_soils.shp")
p2 <- tm_shape(soil) + 
  tm_polygons(col = "MAJOR__SOI", palette = c(get_brewer_pal("Pastel1", n = 11), "darkblue"),
              title = "Soil Type", border.alpha = 0) +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 0.8,
            inner.margins=c(0.1,0.2,0.05,0.03))
p2m <- p2 + om
tmap_save(p2m, filename =  file.path(datadir, "DRC_soil_type_territory.png"),
          width = 5, height = 5, dpi = 300)


# aez
dd <- jsonlite::fromJSON("https://gaez-services.fao.org/server/rest/services/LR/ImageServer/legend?bandIds=&renderingRule=&f=pjson")
ds <- as.data.frame(dd$layers$legend)
ds <- data.frame(pixelvalue = c(1:(nrow(ds)-1)), classname = ds$label[-1])

# v0 <- raster::getData("GADM", country = iso, level = 0, 
#                             path = file.path(datadir, "input/boundary/country"), type = "sf")
# 
# aez <- read_stars("G:/My Drive/work/ciat/eia/DRC/data/AEZ16 r2.0 - TIF/AEZ16_CLAS--SSA.tif")
# saez <- aez[v0]
# droplevels(saez)
r <- raster("G:/My Drive/work/ciat/eia/DRC/data/gaez4_33_classes_v9v2red_5m_CRUTS32_Hist_8110_100_avg.tif")
r <- crop(r, v1)
r <- mask(r, v1)
x <- unique(values(r))
dsx <- ds[ds$pixelvalue %in% x,]

# fr <- ratify(r)
# rat <- levels(fr)[[1]]
# rat$aez <- dsx$classname
# levels(r) <- rat

p3 <- tm_shape(r) +
  tm_raster(style = "cat", 
            palette = c(get_brewer_pal("Pastel1", n = 10), "#253494"), labels = dsx$classname,
            title = "AEZ") +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 0.5,
          inner.margins=c(0.15,0.3,0.05,0.03)) +
  tm_credits("Source: Global Agro-Ecological Zoning version 4 (GAEZ v4) (https://gaez.fao.org)",
               size = 0.5, position = c("right", "bottom"))

p3m <- p3 + om 
tmap_save(p3m, filename =  file.path(datadir, "DRC_aez_territory.png"),
          width = 5, height = 5, dpi = 300)


# plot for rural pop
p1m <- p1 + om +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 0.8,
            inner.margins=c(0.1,0.15,0.05,0.03))

tmap_save(p1m, filename =  file.path(datadir, "DRC_farming_system_territory.png"),
          width = 5, height = 5, dpi = 300)


# distance to market
rr <- rast(file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"))
rr <- crop(rr, vect(v1))
rr <- terra::mask(rr, vect(v1))
r1 <- subset(rr, "rural_population_sum")
r1 <- r1/(10*10) # convert to 1 sqkm
r1 <- raster(r1)

pr1 <- tm_shape(r1) +
  tm_raster(style = "fisher", n = 6, title = "Rural Population Density/km^2",
            palette = colorRampPalette(c("darkolivegreen4","yellow", "brown"))(12))

# access
r2 <- subset(rr, "2015_accessibility_to_cities_v1.0")
r2 <- round(r2, 1)
r2 <- raster(r2)
pr2 <- tm_shape(r2) +
  tm_raster(style = "jenks", n = 5, title = "Travel time to cities \n(in hours)",
            palette = get_brewer_pal("BuPu", n = 5))

pr2
# ag land
cc <- subset(rr, c("cropland_fraction"))
ca <- app(cc, "prod")
plot(cc)

# yield 
ff <- list.files(file.path(datadir, "input/mapspam/spam2017v2r1_ssa_yield.geotiff"),
                 pattern = "*_A.tif", full.names = TRUE)

# major crops of DRC
crps <- c("maiz", "cass", "sugc")

############################################################################################################
# other summary variables
library(terra)
library(sf)
library(raster)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"
iso <- "COD"
cgv <- vect(raster::getData("GADM", country = iso, level = 2, 
                            path = file.path(datadir, "input/boundary/country")))
cgv <- cgv[cgv$TYPE_2 == "Territoire",]
####################################################################################################################################
# level 1
# stunting
s <- rast(file.path(datadir, 
                    "input/ihme/child_growth_failure/IHME_LMIC_CGF_2000_2017_STUNTING_PREV_MEAN_2017_Y2020M01D08.TIF"))

# under 5 mortality
m <- rast(file.path(datadir, 
                    "input/ihme/infant_mortality/IHME_LMICS_U5M_2000_2017_Q_UNDER5_MEAN/IHME_LMICS_U5M_2000_2017_Q_UNDER5_MEAN_Y2019M10D16.TIF"))

# extract only for 2017
m <- m[[nlyr(m)]]
s <- crop(s, m)
sm <- c(s, m)

# extract indicators
health <- terra::extract(sm, cgv, fun = mean, na.rm = TRUE)
health$ID <- NULL
health <- round(health*100)
names(health) <- c("under5_stunting_prevalence_2017(percentage)", "under5_mortality_rate_2017(percentage)")

##############################
# rural population
rpop <- rast(paste0(datadir, "/outdir/rural_population/global_rural_pop_10km.vrt"))
pop <- terra::extract(rpop, cgv, fun = sum, na.rm = TRUE)
pop$ID <- NULL
pop <- round(pop)
names(pop) <- "rural_population"

# other raster
rr <- rast(file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"))
# r <- subset(rr, c("poverty_avg5yr_imputed","povmap_global_subnational_infant_mortality_rates_v2_01"))
# cropland fraction and area
cc <- subset(rr, c("cropland_fraction", "area_sqkm"))
ca <- app(cc, "prod")
cropland <- terra::extract(ca, cgv, fun = sum, na.rm = TRUE)
cropland$ID <- NULL
# cropland <- cropland*100/100 # diving by 100 because the cropland_fraction is in %, then multiplying 100 to ha
names(cropland) <- "cropland_total(ha)"

cropland_percapita <- unname(round(cropland/pop, 2))
names(cropland_percapita) <- "cropland_percapita(ha/person)"

cropland_perfamily <- cropland_percapita*6
names(cropland_perfamily) <- "cropland_perfamily(ha/family)"

# cropland_percapita <- ifelse(!is.finite(cropland_percapita), NA, cropland_percapita)

# poverty
povr <- rast(file.path(datadir, "outdir/worldbank/poor_ppp19_nov18.tif"))
poverty <- terra::extract(povr, cgv, fun = mean, na.rm = TRUE)
poverty$ID <- NULL
names(poverty) <- "poverty(percentage_subnational_ppp190)"


level1 <- data.frame(pop, round(cropland), 
                     cropland_percapita,
                     cropland_perfamily,
                     round(poverty),
                     health, check.names = FALSE)

level1[sapply(level1, is.nan)] <- NA
level1 <- round(level1, 2)
# names(level1) <- paste0("level1_", names(level1))

######################################################################################################################
# level 2
# production trends
yld <- subset(rr, grep("yield", names(rr), value = TRUE))
yldf <- terra::extract(yld, cgv, fun = mean, na.rm = TRUE)
yldf$ID <- NULL
level2 <- round(yldf, 2)
level2[sapply(level2, is.nan)] <- NA

######################################################################################################################
# level 3
# Travel to the nearest market from any cropland pixel
cf <- subset(rr, "cropland_fraction")
acc <- subset(rr, "2015_accessibility_to_cities_v1.0")
acc <- mask(acc, cf, maskvalues = 0)

acs <- terra::extract(acc, cgv, fun = mean, na.rm = TRUE)
acs$ID <- NULL
names(acs) <- "travel_time_market(hours)"

# access to network
# only interested in networks where there is population
ntw <- subset(rr, "network_coverage")
# convert all netwrok to binary class
ntwb <- ntw
ntwb[ntwb > 0] <- 1
# ntwpop <- c(ntwb, rpop)
# ntwpop <- extend(ntwpop, rr)

# % of rural population having access to network
pctUnderNetwork <- function(i, cgv, ntwb, rpop){
  cat("Processing", i, "of", nrow(cgv), "\n")
  v <- cgv[i,]
  y1 <- terra::crop(ntwb, v)
  y1 <- terra::mask(y1, v)
  y2 <- terra::crop(rpop, v)
  y2 <- terra::mask(y2, v)
  y1 <- resample(y1, y2)
  yy <- c(y1, y2)
  yd <- app(yy, "prod") # total population with network
  ntwf <- as.numeric(global(yd, sum, na.rm = T))/as.numeric(global(y2, sum, na.rm = T)) # fraction of population with network
  return(round(ntwf,4)*100)
}

ntwstat <- sapply(1:nrow(cgv), pctUnderNetwork, cgv, ntwb, rpop)
ntwstat <- data.frame(pct_of_rural_pop_internet_access = ntwstat)
# ntwstat <- data.frame(network_access_pct = ntwstat)

# occurrences of conflicts in the last 5 years
conf <- rast(file.path(datadir, "outdir/all_raster/lmic_conflicts_2016-21.tif"))
# now only focusing on the total number of conflicts in the rural areas
conf <- terra::crop(conf, rpop)
conf <- resample(conf, rpop)
conf <- terra::mask(conf, rpop)
confs <- terra::extract(conf[[1:2]], cgv, fun = sum, na.rm = TRUE)
confs$ID <- NULL
names(confs) <- c("total_conflict_incidents_2016-21", "total_conflict_deaths_2016-21")

level3 <- data.frame(acs, ntwstat, confs, check.names = FALSE)
level3[sapply(level3, is.nan)] <- NA
level3 <- round(level3, 2)


#########################################################################################################
# combine information
# merge all
kpi <- cbind(cgv[, c("NAME_1", "NAME_2")], level1, level2, level3)

kpi <- as.data.frame(kpi)
kpi$`conflicts per 100,000 rural person` <- round(100000*kpi$`total_conflict_incidents_2016-21`/kpi$rural_population)
kpi$`conflicts_deaths per 100,000 rural person` <- round(100000*kpi$`total_conflict_deaths_2016-21`/kpi$rural_population)

write.csv(kpi,  file.path(datadir, "DRC_summary_stat_territory.csv"), row.names = FALSE)
