# install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
library(sf)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

# this after combining Central American Dry Corridor with Dixon maps
cgv <- vect("G:/My Drive/work/ciat/eia/analysis/input/boundary/country_farming_system_cg_regions.shp")

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
names(sm) <- c("under5_stunting_prevalence_2017", "under5_mortality_rate_2017")

# extract indicators
health <- extract(sm, cgv, fun = mean, na.rm = TRUE)
health$ID <- NULL
health <- round(health*100)

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
names(cropland) <- "cropland_total_ha"

cropland_percapita <- unname(round(cropland/pop, 2))

# cropland_percapita <- ifelse(!is.finite(cropland_percapita), NA, cropland_percapita)

level1 <- data.frame(pop, round(cropland), cropland_percapita = cropland_percapita, health)

level1[sapply(level1, is.nan)] <- NA
level1 <- round(level1, 2)
names(level1) <- paste0("level1_", names(level1))
######################################################################################################################
# level 2

# ag indicators
mvar <- c("nue_avg5yr_imputed", "soilhealth30cmmean", "ph30cmmean", 
          "cassava_yieldgap", "maize_yieldgap", "millet_yieldgap", "potato_yieldgap",
          "rice_yieldgap", "sorghum_yieldgap", "wheat_yieldgap1",
          "yieldtrend_percentage_maize", "yieldtrend_percentage_rice","yieldtrend_percentage_wheat",                           
          "yieldvariability_coeff_maize", "yieldvariability_coeff_rice","yieldvariability_coeff_wheat")

rag <- subset(rr, mvar)

ag <- extract(rag, cgv, fun = mean, na.rm = TRUE)
ag$ID <- NULL

# group yield gap by crop type
d <- ag
d[sapply(d, is.nan)] <- NA
cerealyldgap <- rowMeans(d[,c("maize_yieldgap","millet_yieldgap", "rice_yieldgap", "sorghum_yieldgap", "wheat_yieldgap1")], na.rm = T)

rtbyldgap <- rowMeans(d[,c("cassava_yieldgap","potato_yieldgap")], na.rm = T)

cerealyldtrend <- rowMeans(d[,c("yieldtrend_percentage_maize", "yieldtrend_percentage_rice","yieldtrend_percentage_wheat")], na.rm = T)
 
cerealyldvar <- rowMeans(d[,c("yieldvariability_coeff_maize", "yieldvariability_coeff_rice","yieldvariability_coeff_wheat")], na.rm = T)

level2 <- data.frame(nue_avg5yr = ag$nue_avg5yr_imputed, 
                     soilhealth = d$soilhealth30cmmean, ph = d$ph30cmmean,
                     cereal_yield_gap = cerealyldgap, rtb_yield_gap = rtbyldgap, 
                     cereal_yield_trend = cerealyldtrend, cereal_yield_variability = cerealyldvar)
level2[sapply(level2, is.nan)] <- NA
level2 <- round(level2, 2)
names(level2) <- paste0("level2_", names(level2))
######################################################################################################################
# level 3
# Travel to the nearest market from any cropland pixel
cf <- subset(rr, "cropland_fraction")
acc <- subset(rr, "2015_accessibility_to_cities_v1.0")
acc <- mask(acc, cf, maskvalues = 0)

acs <- extract(acc, cgv, fun = mean, na.rm = TRUE)
acs$ID <- NULL
names(acs) <- "accessibility_to_cities_2015"

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
ntwstat <- data.frame(network_access_pct = ntwstat)

# occurrences of conflicts in the last 5 years
conf <- rast(file.path(datadir, "outdir/all_raster/lmic_conflicts_2016-21.tif"))
# now only focusing on the total number of conflicts
confs <- extract(conf[[1]], cgv, fun = sum, na.rm = TRUE)
confs$ID <- NULL
names(confs) <- "total_conflicts_2016-21"

level3 <- data.frame(acs, ntwstat, confs)

level3[sapply(level3, is.nan)] <- NA
level3 <- round(level3, 2)
names(level3) <- paste0("level3_", names(level3))

#########################################################################################################
# combine information
# merge all
v <- cbind(cgv, level1, level2, level3)
v$FORMAL_ <- NULL
v$ECONOMY <- NULL
v$INCOME_ <- NULL
v$ar_sqkm <- NULL
names(v)[names(v) == "frmng_s"] <- "farming_system"
names(v)[names(v) == "cgregin"] <- "cgregion"

v$farming_system <- gsub("[[:digit:]]+","", v$farming_system)
v$farming_system <- gsub("\\.","", v$farming_system)
v$farming_system <- trimws(v$farming_system)

# xx <- aggregate(.~ISO_A3, v, sum, na.rm = TRUE)
vsf <- st_as_sf(v)
st_write(vsf, file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.geojson"), 
         delete_layer = TRUE)
st_write(vsf, file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.csv"),
         delete_layer = TRUE)

# save each region as different excel sheet
dsf <- as.data.frame(v)
cgs <- unique(dsf$cgregion)
dcg <- lapply(unique(cgs), function(cg){
  x <- dsf[dsf$cgregion == cg, ]
  return(x)
})


tmp <- writexl::write_xlsx(list(SAE = dcg[[1]], LAC = dcg[[2]],
                                SA = dcg[[3]], CWANA = dcg[[4]],
                                ESA = dcg[[5]], WCA = dcg[[6]]), 
                           path = file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.xlsx"))


# d1 <- dd %>% 
#   select(-c(NAME_EN, ISO_A3)) %>%
#   group_by(cgregion, farming_system) %>%
#   summarize_all(mean, na.rm = T) %>%
#   mutate(across(c(stunting_2017, poverty_5years, infant_mortality_rates), round)) %>%
#   ungroup()
# 
# d2 <- dd %>% 
#   group_by(cgregion, farming_system, NAME_EN, ISO_A3) %>%
#   summarize_all(mean, na.rm = T) %>%
#   mutate(across(c(stunting_2017, poverty_5years, infant_mortality_rates), round)) %>%
#   ungroup()
# 
# tmp <- write_xlsx(list(cg_fs = d1, country_fs = d2), path = file.path(datadir, "output/summary_table_level1.xlsx"))

# eia selection summary is the final layer made by Jordan after removing the FS we are not interested in
# but is looks like we need to keep all the LS but remove the countries
# rv <- readRDS(file.path(datadir, "output/eia_selection_summary.rds"))
# tiso <- sort(unique(rv$ISO_A3))
# cgv <- vect(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))
# cgv <- cgv[cgv$ISO_A3 %in% tiso, ]

