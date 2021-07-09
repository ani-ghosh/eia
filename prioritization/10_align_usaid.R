# install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
library(sf)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

# this after combining Central American Dry Corridor with Dixon maps
cgv <- vect(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))
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

# target countries
tiso <- c('AFG','AGO','ARM','AZE','BDI','BEN','BFA','BGD','BLZ','BOL','BTN','BWA','CAF','CIV','CMR','COD','COG','COL','CRI','CUB',
          'DMA','DOM','DZA','ECU','EGY','ERI','ETH','GAB','GHA','GIN','GMB','GNB','GRD','GTM','GUY','HND','HTI','IDN','IND','IRN','IRQ',
          'JAM','KAZ','KEN','KGZ','KHM','LAO','LBN','LBR','LBY','LKA','LSO','MAR','MDG','MEX','MLI','MMR','MOZ','MRT','MWI',
          'NAM','NER','NGA','NIC','NPL','PAK','PAN','PER','PHL','PNG','PRY','PSE','RWA','SDN','SEN','SLB','SLE','SLV','SOM','SSD','SUR','SWZ','SYR',
          'TCD','TGO','THA','TJK','TKM','TLS','TUN','TZA','UGA','UZB','VCT','VEN','VNM','YEM','ZMB','ZWE')
cgv <- cgv[cgv$ISO_A3 %in% tiso,]
cgv$UID <- paste0(cgv$farming_system, "_", cgv$ISO_A3)

# exclude farming systems
rfc <- readxl::read_excel("data/irrelevant_country_farming_systems.xlsx", sheet = 1)
cgv <- cgv[!cgv$UID %in% rfc$UID,]
cgv <- cgv[cgv$farming_system != "",]
cgv$UID <- NULL

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

# TODO save as aligned with the reference raster

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

# TODO save as aligned with the reference raster 
ca <- app(cc, "prod")
cropland <- extract(ca, cgv, fun = sum, na.rm = TRUE)
cropland$ID <- NULL
cropland <- round(cropland)
# cropland <- cropland*100/100 # diving by 100 because the cropland_fraction is in %, then multiplying 100 to ha
names(cropland) <- "cropland_total(ha)"

cropland_percapita <- unname(round(cropland/pop, 2))
names(cropland_percapita) <- "cropland_percapita(ha/person)"

cropland_perfamily <- cropland_percapita*6
names(cropland_perfamily) <- "cropland_perfamily(ha/family)"

# cropland_percapita <- ifelse(!is.finite(cropland_percapita), NA, cropland_percapita)

# poverty
pov <- rast(file.path(datadir, "outdir/worldbank/poor_ppp19_est.tif"))
# pov1 <- subset(rr, c("poverty_avg5yr_imputed"))
poverty <- extract(pov, cgv, fun = mean, na.rm = TRUE)
poverty$ID <- NULL
names(poverty) <- "poverty(percentage_subnational)"
povert <- round(poverty)

level1 <- data.frame(pop, cropland, 
                     cropland_percapita,
                     cropland_perfamily,
                     poverty,
                     health, check.names = FALSE)

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

level2 <- data.frame(`nue_avg5yr(percentage)` = ag$nue_avg5yr_imputed*100, 
                     soilhealth = d$soilhealth30cmmean, ph = d$ph30cmmean,
                     `cereal_yield_gap(ton/ha)` = cerealyldgap, `rtb_yield_gap(ton/ha)` = rtbyldgap, 
                     `cereal_yield_trend(percentage)` = cerealyldtrend, 
                     coeff_cereal_yield_variability = cerealyldvar, check.names = FALSE)
level2[sapply(level2, is.nan)] <- NA
level2 <- round(level2, 2)
names(level2) <- paste0("level2_", names(level2))

######################################################################################################################
# level 3
# Travel to the nearest market from any cropland pixel
cf <- subset(rr, "cropland_fraction")
acc <- subset(rr, "2015_accessibility_to_cities_v1.0")

# TODO save as aligned with the reference raster
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
ntwstat <- data.frame(pct_of_rural_pop_internet_access = ntwstat)
# ntwstat <- data.frame(network_access_pct = ntwstat)

# occurrences of conflicts in the last 5 years
conf <- rast(file.path(datadir, "outdir/all_raster/lmic_conflicts_2016-21.tif"))
# now only focusing on the total number of conflicts
confs <- extract(conf[[1]], cgv, fun = sum, na.rm = TRUE)
confs$ID <- NULL
names(confs) <- "total_conflict_incidents_2016-21"

level3 <- data.frame(acs, ntwstat, confs, check.names = FALSE)
level3[sapply(level3, is.nan)] <- NA
level3 <- round(level3, 2)
names(level3) <- paste0("level3_", names(level3))

#########################################################################################################
# ease of doing business
eba <- file.path(datadir,"outdir/worldbank/eba_cleaned_country_farming_system_cg_regions.shp")
eba <- vect(eba)
eba <- as.data.frame(eba[,c("ISO_A3","NAME_EN","cgregin","frmng_s",  "eb_scr_")])
names(eba) <- c("ISO_A3","NAME_EN","cgregion","farming_system","level3_ease_of_doing_business_score")

eba$farming_system <- gsub("[[:digit:]]+","", eba$farming_system)
eba$farming_system <- gsub("\\.","", eba$farming_system)
eba$farming_system <- trimws(eba$farming_system)

eba$UID <- paste0(eba$farming_system, "_", eba$ISO_A3)
eba <- eba[!eba$UID %in% rfc$UID,]
eba <- eba[eba$farming_system != "",]
eba$UID <- NULL

# combine information
# merge all
v <- cbind(cgv, level1, level2, level3)
v$`level3_conflicts per 100,000 rural person` <- 100000*v$`level3_total_conflict_incidents_2016-21`/v$level1_rural_population
# merge with ease of doing business
v <- merge(v, eba, all.x = TRUE)

# delete units with no population/cropland
k <- which(v$level1_rural_population == 0 | v$`level1_cropland_total(ha)` == 0) 
vs <- v[-k, ]
# vs$level1_ID <- NULL
# ranking
cond1 <- ifelse(vs$`level1_cropland_total(ha)` >= 500000, 1, 0)
cond2 <- ifelse(vs$`level1_cropland_perfamily(ha/family)` < 5, 1, 0)
cond3 <- ifelse(vs$`level3_travel_time_market(hours)` <= 4, 1, 0)
cond4 <- ifelse(vs$level3_ease_of_doing_business_score >= 40, 1, 0)
# cond5 <- ifelse(vs$`level3_conflicts per 100,000 rural person` <= 15, 1, 0)
cond <- data.frame(cond1, cond2, cond3, cond4)
cond$fcond <- ifelse(rowSums(cond[,c(1:4)], na.rm = T) == 4, 1, 0)
cond$fcond <- ifelse(rowSums(cond[,c(1:4)], na.rm = T) == 3 & cond$cond4 == 0, 2, cond$fcond)
cond$fcond <- ifelse(rowSums(cond[,c(1:4)], na.rm = T) == 2 & cond$cond4 == 0 & cond$cond3 == 0, 3, cond$fcond)

vs$rank <- cond$fcond

# save each region as different excel sheet
dsf <- as.data.frame(vs)
cgs <- unique(dsf$cgregion)
dcg <- lapply(unique(cgs), function(cg){
  x <- dsf[dsf$cgregion == cg, ]
  return(x)
})

rankrule <- data.frame(rule = 
                c("more than 500k ha cropland & less than 5 ha per household & less than 4 hrs to markets & more then 40 ease of doing business",
                  "more than 500k ha cropland & less than 5 ha per household & less than 4 hrs to markets & less then 40 ease of doing business",
                  "more than 500k ha cropland & less than 5 ha per household & more than 4 hrs to markets & less then 40 ease of doing business"),
                rank = 1:3)

oname <- paste0("outdir/level_stat/level123_cgregion_country_farming_system_", format(Sys.time(), "%Y-%m-%d %H-%M"),
                ".xlsx")
tmp <- writexl::write_xlsx(list(SAE = dcg[[1]], LAC = dcg[[2]],
                                SA = dcg[[3]], CWANA = dcg[[4]],
                                ESA = dcg[[5]], WCA = dcg[[6]], rankingrule = rankrule), 
                           path = file.path(datadir, oname))




# xx <- aggregate(.~ISO_A3, v, sum, na.rm = TRUE)
vsf <- st_as_sf(vs)
st_write(vsf, file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.geojson"), 
         delete_layer = TRUE)
# st_write(vsf, file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.csv"),
#          delete_layer = TRUE)

write.csv(as.data.frame(vs), file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.csv"), 
          row.names = FALSE)


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

