# for ESA indicators
# install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
library(sf)
library(raster)
library(rgeos)
rgeos::set_RGEOS_CheckValidity(2L)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

# this after combining Central American Dry Corridor with Dixon maps
tiso <- c("BWA", "BDI", "ERI", "SWZ", "ETH", "KEN", "LSO", "MDG", "MWI", "MOZ",
          "NAM", "RWA", "SOM", "SSD", "TZA", "UGA", "ZMB", "ZWE", "ZAF",
          "COM", "MUS", "SYC", "COD", "AGO")
# cgv <- vect(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))
# cgv <- cgv[cgv$ISO_A3 %in% tiso,]

###################################################################################################
vdir <- "G:\\My Drive\\work\\ciat\\cg-prioritization"
fs <- shapefile(file.path(vdir, "vector/farming_system_combined_2001_12.shp"))

# low resolution boundaries from WB https://datacatalog.worldbank.org/dataset/world-bank-official-boundaries
furl <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/779551/wb_boundaries_geojson_lowres.zip"
zfile <- file.path(vdir, "vector", basename(furl))
if(!file.exists(zfile)){
  download.file(furl, zfile, mode = "wb")
  unzip(zfile, exdir = dirname(zfile))
}

################################################################################################
# simplified vector boundaries
# vdir <- "G:\\My Drive\\work\\ciat\\cg-prioritization"
# cb <- file.path(vdir, "vector/WB_Boundaries_GeoJSON_lowres/WB_countries_Admin0_lowres.geojson")
# cb <- st_read(cb)
# cb <- as_Spatial(cb)
# cbs <- cb[cb$ISO_A3 %in% tiso, ]
# 
# # combine with farming systems
# combineFSregion <- function(i, cbs, fs){
#   cs <- cbs[i,]
#   cat("processing ", cs$ISO_A3, "\n")
#   fsv <- crop(fs, cs)
#   if(!is.null(fsv)){  
#     fsv <- aggregate(fsv, by = "frm_sys")
#     names(fsv) <- "farming_system"
#     a <- round(area(fsv)/(1000*1000))
#     fsv@data <- data.frame(cs@data, fsv@data, area_sqkm = a)
#     return(fsv)
#   }
# }
# 
# vv <- lapply(1:nrow(cbs), combineFSregion, cbs, fs)
# vv[sapply(vv, is.null)] <- NULL
# vs <- do.call(rbind,vv)
# vs <- vs[,c("FORMAL_EN","ISO_A3","farming_system")]
# names(vs)[1] <- "NAME_EN" 
# shapefile(vs, file.path(datadir, "input/boundary/country_farming_system_esa_regions.shp"),
#           overwrite = TRUE)

cgv <- vect(file.path(datadir, "input/boundary/country_farming_system_esa_regions.shp"))

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

# library(ggplot2)
# ggplot(health, aes(x=under5_stunting_prevalence_2017,y=under5_mortality_rate_2017,colour=cg)) + 
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   theme_bw()

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

# hungermap
wfp <- rast(file.path(datadir, "outdir/wfp/fcs_rcsi.tif"))
wfps <- terra::extract(wfp, cgv, fun = mean, na.rm = TRUE)
wfps <- wfps[,c("fcs_score", "rcsi_ratio")]
wfps$rcsi_ratio <- wfps$rcsi_ratio/100
names(wfps) <- c("food_consumption_score(fcs)", "reduced_coping_strategies_index(rCSI)" )
wfps <- round(wfps, 2)

level1 <- data.frame(pop, round(cropland), 
                     cropland_percapita,
                     cropland_perfamily,
                     round(poverty),
                     health, wfps, check.names = FALSE)

level1[sapply(level1, is.nan)] <- NA
level1 <- round(level1, 2)
# names(level1) <- paste0("level1_", names(level1))

######################################################################################################################
# level 2
# climate stress indicator
# haz <- rast(file.path("G:\\My Drive\\work\\ciat\\cg-prioritization/input/combi_haz_table_cg.asc/combi_haz_table_cg.asc"))  
# writeRaster(haz, file.path(datadir, "outdir/combi_haz_cg.tif"), gdal=c("COMPRESS=LZW"), overwrite = TRUE)
haz <- rast(file.path(datadir, "outdir/combi_haz_cg.tif"))

# mask non-cropland
# cf <- subset(rr, "cropland_fraction")
# cf <- crop(cf, haz)
# haz <- mask(haz, cf, maskvalues = 0)

# proportional area of each hazard category
getFracarea <- function(i, cgv, haz){
  cat("processing", i, "\n")
  v <- cgv[i,]
  hr <- terra::crop(haz, v)
  hr <- terra::mask(hr, v)
  f <- terra::freq(hr)
  p <- data.frame(f, frac_area = f[,3]/sum(f[,3]))
  p$frac_area <- round(p$frac_area,2)*100
  tp <- data.frame(t(p))
  names(tp) <- paste0("X_", p$value)
  tp <- tp[4,]
  tp <- data.frame(as.data.frame(v), tp)
  rownames(tp) <- NULL
  return(tp)
}

hzv <- lapply(1:nrow(cgv), getFracarea, cgv, haz)
hzd <-dplyr::bind_rows(hzv) 

# we need a few, haz_1 == drought, haz_3 = climate_variability, haz_1+haz_4, haz_3+haz_4, haz_3+haz_8
hzd$drought <- rowSums(hzd[,c("X_1","X_4")], na.rm = TRUE)
hzd$clim_variability <- rowSums(hzd[,c("X_3","X_4", "X_8")], na.rm = TRUE)
hzd <- hzd[,c("NAME_EN", "ISO_A3", "frmng_s","drought", "clim_variability")]


# crs(haz) <- crs(cgv)
# vals <- c("drought", "flood", "climate_variability", "drought_climate_variability",
#                 "growing_season_reductions", "high_growing_season_temperatures",
#                 "flood_high_growing_season_temp", "climate_variability_high_growing_season_temp", "others")


######################################################################################################
# Suitability change estimates
changeFunction <- function(a,b){
  # a == future
  # b == current
  # becomes suitable -> new suitable
  c1 <- ifelse(a>=40 & b<40, 1, 0)
  # stay suitable but above current -> more suitable
  c2 <- ifelse(a>b & a>=40 & b>=40, 2, 0)
  #  stay suitable equal to current -> same suitable
  c3 <- ifelse(a==b & a>=40 & b>=40, 3, 0)
  # stay suitable but below current -> less suitable
  c4 <- ifelse(a<b & a>=40 & b>=40, 4, 0)
  # becomes unsuitable -> new unsuitable
  c5 <- ifelse(a<40 & b>=40, 5, 0)
  # both poorly suitable -> poor suitability
  c61 <- ifelse(a>0 & a<40 & b>0 & b<40, 6, 0)
  c62 <- ifelse(a==0 & b>0 & b<40, 6, 0)
  c63 <- ifelse(a>0 & a<40 & b==0, 6, 0)
  # remain unsuitable -> unsuitable
  c7 <- ifelse(a==0 & b==0, 7, 0)
  # combine conditions
  cnd <- apply(cbind(c1,c2,c3,c4,c5,c61,c62,c63,c7), 1, FUN = function(x){sum(x, na.rm = TRUE)})
  # cc <- c(c1,c2,c3,c4,c5,c6,c7)
  # cnd <- cc[which(cc>0)]
  return(cnd) 
}

# maize area reduction
mcur <- rast("G:/My Drive/work/ciat/ecocrop/worldclim/2_5min/maize_all_worldclim.tif")
# mfut30 <- rast("G:/My Drive/work/ciat/ecocrop/future/2_5min/maize_all_8_5_2030.tif")
mfut50 <- rast("G:/My Drive/work/ciat/ecocrop/future/2_5min/maize_all_8_5_2050.tif")

mcur <- raster(terra::crop(mcur, cgv))
mfut50 <- raster(terra::crop(mfut50, cgv))

suitchg <- overlay(mfut50, mcur, fun = changeFunction)
suitchg <- rast(suitchg)
chgstat <- lapply(1:nrow(cgv), getFracarea, cgv, suitchg)
chgd <- dplyr::bind_rows(chgstat) 
chgd$maize_suit_decline <- rowSums(chgd[,c("X_4","X_5")], na.rm = TRUE)
chgd <- chgd[,c("NAME_EN", "ISO_A3", "frmng_s", "maize_suit_decline")]

level2 <- merge(hzd, chgd)

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
# ease of doing business
eba <- file.path(datadir,"outdir/worldbank/eba_cleaned_country_farming_system_cg_regions.shp")
eba <- vect(eba)
eba <- as.data.frame(eba[,c("ISO_A3", "frmng_s",  "eb_scr_")])
eba$eb_scr_ <- round(eba$eb_scr_)
names(eba)[3] <- "ease_of_doing_business_score"

# combine information
# merge all
v <- cbind(cgv, level1, level3)
# merge with ease of doing business
v <- merge(v, level2)
v <- merge(v, eba, all.x = TRUE)

v <- as.data.frame(v)
v$`conflicts per 100,000 rural person` <- round(100000*v$`total_conflict_incidents_2016-21`/v$rural_population)
v$`conflicts_deaths per 100,000 rural person` <- round(100000*v$`total_conflict_deaths_2016-21`/v$rural_population)

names(v)[names(v) == "frmng_s"] <- "farming_system"

v$farming_system <- gsub("[[:digit:]]+","", v$farming_system)
v$farming_system <- gsub("\\.","", v$farming_system)
v$farming_system <- trimws(v$farming_system)

# delete units with no population/cropland
k <- which(v$rural_population == 0 | v$`cropland_total(ha)` == 0) 
vs <- v[-k, ]

library(tidyverse)
vss <- vs %>% as.data.frame() %>%
  select(-c(farming_system)) %>%
  group_by(NAME_EN, ISO_A3) %>% 
  summarise_all(.funs = funs(mean = round(mean(., na.rm = T), 2), total = round(sum(.,na.rm = TRUE), 2))) 

tokeep <- c("NAME_EN", "ISO_A3", "rural_population_total", "cropland_total(ha)_total",
            "cropland_percapita(ha/person)_mean", "cropland_perfamily(ha/family)_mean",
            "poverty(percentage_subnational_ppp190)_mean", "under5_stunting_prevalence_2017(percentage)_mean", 
            "under5_mortality_rate_2017(percentage)_mean", "food_consumption_score(fcs)_mean",                 
            "reduced_coping_strategies_index(rCSI)_mean", "travel_time_market(hours)_mean",                   
            "pct_of_rural_pop_internet_access_mean", 
            "drought_mean", "clim_variability_mean",
            "maize_suit_decline_mean", "ease_of_doing_business_score_mean",
            "conflicts per 100,000 rural person_total", "conflicts_deaths per 100,000 rural person_total")

vss <- vss[, tokeep]
oname <- paste0("outdir/level_stat/esa_region_country_farming_system_", format(Sys.time(), "%Y-%m-%d %H-%M"),
                ".xlsx")
tmp <- writexl::write_xlsx(list(esa_stat = as.data.frame(vs),
                           summary_country = vss), 
                           path = file.path(datadir, oname))


# xx <- aggregate(.~ISO_A3, v, sum, na.rm = TRUE)
vsf <- st_as_sf(vs)
st_write(vsf, file.path(datadir, "outdir/level_stat/esa_region_country_farming_system.geojson"), 
         delete_layer = TRUE)
# st_write(vsf, file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.csv"),
#          delete_layer = TRUE)

write.csv(as.data.frame(vs), file.path(datadir, "outdir/level_stat/esa_region_country_farming_system.csv"), 
          row.names = FALSE)

# save each region as different excel sheet
dsf <- as.data.frame(vs)
cgs <- unique(dsf$cgregion)
dcg <- lapply(unique(cgs), function(cg){
  x <- dsf[dsf$cgregion == cg, ]
  return(x)
})


oname <- paste0("outdir/level_stat/level123_cgregion_country_farming_system_", format(Sys.time(), "%Y-%m-%d %H-%M"),
                ".xlsx")
tmp <- writexl::write_xlsx(list(SAE = dcg[[1]], LAC = dcg[[2]],
                                SA = dcg[[3]], CWANA = dcg[[4]],
                                ESA = dcg[[5]], WCA = dcg[[6]]), 
                           path = file.path(datadir, oname))
