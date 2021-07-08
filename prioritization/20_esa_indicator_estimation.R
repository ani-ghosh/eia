# for ESA indicators
# install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
library(sf)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

# this after combining Central American Dry Corridor with Dixon maps
cgv <- vect(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))
cgv <- cgv[cgv$cgregin == "ESA",]

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
health <- extract(sm, cgv, fun = mean, na.rm = TRUE)
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
pop <- extract(rpop, cgv, fun = sum, na.rm = TRUE)
pop$ID <- NULL
pop <- round(pop)
names(pop) <- "rural_population"

# other raster
rr <- rast(file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"))
# r <- subset(rr, c("poverty_avg5yr_imputed","povmap_global_subnational_infant_mortality_rates_v2_01"))
# cropland fraction and area
cc <- subset(rr, c("cropland_fraction", "area_sqkm"))
ca <- app(cc, "prod")
cropland <- extract(ca, cgv, fun = sum, na.rm = TRUE)
cropland$ID <- NULL
# cropland <- cropland*100/100 # diving by 100 because the cropland_fraction is in %, then multiplying 100 to ha
names(cropland) <- "cropland_total(ha)"

cropland_percapita <- unname(round(cropland/pop, 2))
names(cropland_percapita) <- "cropland_percapita(ha/person)"

cropland_perfamily <- cropland_percapita*6
names(cropland_perfamily) <- "cropland_perfamily(ha/family)"

# cropland_percapita <- ifelse(!is.finite(cropland_percapita), NA, cropland_percapita)

# poverty
povr <- rast(file.path(datadir, "outdir/worldbank/poor_ppp19_est.tif"))
poverty <- extract(povr, cgv, fun = mean, na.rm = TRUE)
poverty$ID <- NULL
names(poverty) <- "poverty(percentage_subnational_ppp190)"

# hungermap
wfp <- rast(file.path(datadir, "outdir/wfp/fcs_rcsi.tif"))
wfps <- extract(wfp, cgv, fun = mean, na.rm = TRUE)
wfps <- wfps[,c("fcs_score", "rcsi_ratio")]
wfps$rcsi_ratio <- wfps$rcsi_ratio/100
names(wfps) <- c("food_consumption_score(fcs)", "reduced_coping_strategies_index(rCSI)" )
wfps <- round(wfps, 2)

level1 <- data.frame(pop, round(cropland), 
                     cropland_percapita,
                     cropland_perfamily,
                     poverty,
                     health, check.names = FALSE)

level1[sapply(level1, is.nan)] <- NA
level1 <- round(level1, 2)
names(level1) <- paste0("level1_", names(level1))

######################################################################################################################
# level 2
# climate stress indicator
haz <- rast(file.path("G:\\My Drive\\work\\ciat\\cg-prioritization/input/combi_haz_table_cg.asc/combi_haz_table_cg.asc"))  
writeRaster(haz, file.path(datadir, "outdir/combi_haz_cg.tif"), gdal=c("COMPRESS=LZW"), overwrite = TRUE)
haz <- rast(file.path(datadir, "outdir/combi_haz_cg.tif"))

# mask non-cropland
# cf <- subset(rr, "cropland_fraction")
# cf <- crop(cf, haz)
# haz <- mask(haz, cf, maskvalues = 0)

# proportional area of each hazard category
getFracarea <- function(i, cgv, haz){
  cat("processing", i, "\n")
  v <- cgv[i,]
  hr <- crop(haz, v)
  hr <- mask(hr, v)
  f <- freq(hr)
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
hzd <- hzd[,c("NAME_EN", "ISO_A3", "cgregin", "frmng_s","drought", "clim_variability")]


# crs(haz) <- crs(cgv)
# vals <- c("drought", "flood", "climate_variability", "drought_climate_variability",
#                 "growing_season_reductions", "high_growing_season_temperatures",
#                 "flood_high_growing_season_temp", "climate_variability_high_growing_season_temp", "others")


######################################################################################################
# Suitability change plot
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
mcur <- raster("G:/My Drive/work/ciat/ecocrop/worldclim/2_5min/maize_all_worldclim.tif")
# mfut30 <- rast("G:/My Drive/work/ciat/ecocrop/future/2_5min/maize_all_8_5_2030.tif")
mfut50 <- raster("G:/My Drive/work/ciat/ecocrop/future/2_5min/maize_all_8_5_2050.tif")

suitchg <- overlay(mfut50, mcur, fun = changeFunction)
suitchg <- rast(suitchg)
chgstat <- lapply(1:nrow(cgv), getFracarea, cgv, suitchg)
chgd <- dplyr::bind_rows(chgstat) 
chgd$maize_suit_decline <- rowSums(chgd[,c("X_4","X_5")], na.rm = TRUE)
chgd <- chgd[,c("NAME_EN", "ISO_A3", "cgregin", "frmng_s", "maize_suit_decline")]

level2 <- merge(hzd, chgd)

######################################################################################################################
# level 3
# Travel to the nearest market from any cropland pixel
cf <- subset(rr, "cropland_fraction")
acc <- subset(rr, "2015_accessibility_to_cities_v1.0")
acc <- mask(acc, cf, maskvalues = 0)

acs <- extract(acc, cgv, fun = mean, na.rm = TRUE)
acs$ID <- NULL
names(acs) <- "travel_time_market(hours)"

# access to network
# only interested in networks where there is population
ntw <- subset(rr, "network_coverage")
ntw <- crop(ntw, rpop)
ext(ntw) <- ext(rpop)
ntw <- mask(ntw, rpop)
# convert all netwrok to binary class
ntwb <- ntw
ntwb[ntwb > 0] <- 1
ntwpop <- c(ntwb, rpop)

# % of rural population having access to network
pctUnderNetwork <- function(i, cgv, ntwpop){
  cat("Processing", i, "of", nrow(cgv), "\n")
  v <- cgv[i,]
  y <- crop(ntwpop, v)
  y <- mask(y, v)
  yd <- app(y, "prod") # total population with network
  ntwf <- as.numeric(global(yd, sum, na.rm = T))/as.numeric(global(y[[2]], sum, na.rm = T)) # fraction of population with network
  return(round(ntwf*100))
}

ntwstat <- sapply(1:nrow(cgv), pctUnderNetwork, cgv, ntwpop)
# ntwstat <- data.frame(network_access_pct = ntwstat)

# occurrences of conflicts in the last 5 years
conf <- rast(file.path(datadir, "outdir/all_raster/lmic_conflicts_2016-21.tif"))
# now only focusing on the total number of conflicts
confs <- extract(conf[[1]], cgv, fun = sum, na.rm = TRUE)
confs$ID <- NULL
names(confs) <- "total_conflict_incidents_2016-21"

level3 <- data.frame(acs, percentage_population_network_access = ntwstat, confs, check.names = FALSE)
level3[sapply(level3, is.nan)] <- NA
level3 <- round(level3, 2)
names(level3) <- paste0("level3_", names(level3))

#########################################################################################################
# ease of doing business
eba <- file.path(datadir,"outdir/worldbank/eba_cleaned_country_farming_system_cg_regions.shp")
eba <- vect(eba)
eba <- as.data.frame(eba[,c("ISO_A3","NAME_EN","cgregin","frmng_s",  "eb_scr_")])
names(eba)[5] <- "level3_ease_of_doing_business_score"

# combine information
# merge all
v <- cbind(cgv, level1, level2, level3)
# merge with ease of doing business
v <- merge(v, eba)
v$FORMAL_ <- NULL
v$ECONOMY <- NULL
v$INCOME_ <- NULL
v$ar_sqkm <- NULL
names(v)[names(v) == "frmng_s"] <- "farming_system"
names(v)[names(v) == "cgregin"] <- "cgregion"

v$farming_system <- gsub("[[:digit:]]+","", v$farming_system)
v$farming_system <- gsub("\\.","", v$farming_system)
v$farming_system <- trimws(v$farming_system)

# delete units with no population/cropland
k <- which(v$level1_rural_population == 0 | v$`level1_cropland_total(ha)` == 0) 
vs <- v[-k, ]

# xx <- aggregate(.~ISO_A3, v, sum, na.rm = TRUE)
vsf <- st_as_sf(vs)
st_write(vsf, file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.geojson"), 
         delete_layer = TRUE)
# st_write(vsf, file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.csv"),
#          delete_layer = TRUE)

write.csv(as.data.frame(vs), file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.csv"), 
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
