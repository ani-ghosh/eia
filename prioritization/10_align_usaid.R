# install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

# eia selection summary is the final layer made by Jordan after removing the FS we are not interested in
# but is looks like we need to keep all the LS but remove the countries
# rv <- readRDS(file.path(datadir, "output/eia_selection_summary.rds"))
# tiso <- sort(unique(rv$ISO_A3))
# cgv <- vect(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))
# cgv <- cgv[cgv$ISO_A3 %in% tiso, ]

# this after combining Central American Dry Corridor with Dixon maps
cgv <- vect("G:/My Drive/work/ciat/eia/analysis/input/boundary/country_farming_system_cg_regions.shp")

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
wstat <- extract(sm, cgv, fun = mean, na.rm = TRUE)
wstat <- wstat[,2:ncol(wstat)]
wstat <- round(wstat*100)
# wstat$cg <- cgv$cgregin

# library(ggplot2)
# ggplot(wstat, aes(x=under5_stunting_prevalence_2017,y=under5_mortality_rate_2017,colour=cg)) + 
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   theme_bw()

# rural population
rpop <- rast(paste0(datadir, "/outdir/rural_population/global_rural_pop_10km.vrt"))
wstat1 <- extract(rpop, cgv, fun = sum, na.rm = TRUE)
wstat1$ID <- NULL
# wstat1 <- wstat1[,2:ncol(wstat1)]
wstat1 <- round(wstat1)
names(wstat1) <- "rural_population"

# other raster
rr <- rast(file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"))
# r <- subset(rr, c("poverty_avg5yr_imputed","povmap_global_subnational_infant_mortality_rates_v2_01"))
# cropland fraction and area
cc <- subset(rr, c("cropland_fraction", "area_sqkm"))
ca <- app(cc, "prod")
wstat2 <- extract(ca, cgv, fun = sum, na.rm = TRUE)
wstat2$ID <- NULL
wstat2 <- wstat2*100/100 # diving by 100 because the cropland_fraction is in %, then multiplying 100 to ha
names(wstat2) <- "cropland_total_ha"

# merge all
v <- cbind(cgv, wstat, wstat1, wstat2)
v$FORMAL_ <- NULL
v$ECONOMY <- NULL
v$INCOME_ <- NULL
v$ar_sqkm <- NULL
names(v)[names(v) == "frmng_s"] <- "farming_system"
names(v)[names(v) == "cgregin"] <- "cgregion"

v$farming_system <- gsub("[[:digit:]]+","", v$farming_system)
v$farming_system <- gsub("\\.","", v$farming_system)
v$farming_system <- trimws(v$farming_system)

v$cropland_percapita <- v$cropland_total_ha/v$rural_population
v$cropland_percapita <- ifelse(!is.finite(v$cropland_percapita), NA, v$cropland_percapita)

# xx <- aggregate(.~ISO_A3, v, sum, na.rm = TRUE)
vsf <- st_as_sf(v)
st_write(vsf, file.path(datadir, "outdir/level_stat/level1_cgregion_country_farming_system.geojson"), 
         delete_layer = TRUE)
st_write(vsf, file.path(datadir, "outdir/level_stat/level1_cgregion_country_farming_system.csv"),
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
                  path = file.path(datadir, "outdir/level_stat/level1_cgregion_country_farming_system.xlsx"))


# all other should be mean except population
mvar <- c("nue_avg5yr_imputed", "elevation", "X2015_accessibility_to_cities_v1.0", "npptrend",
          "soilhealth30cmmean", "cassava_yieldgap", "maize_yieldgap", "millet_yieldgap", "potato_yieldgap",
          "rice_yieldgap", "sorghum_yieldgap", "wheat_yieldgap1",
          "yieldtrend_percentage_maize", "yieldtrend_percentage_rice","yieldtrend_percentage_wheat",                           
          "yieldvariability_coeff_maize", "yieldvariability_coeff_rice","yieldvariability_coeff_wheat")

wstat1 <- extract(rr, cgv, fun = mean, na.rm = TRUE)
wstat1 <- wstat1[,2:ncol(wstat1)]
cgv$poverty_5years <- round(wstat1$poverty_avg5yr_imputed, 0)
cgv$infant_mortality_rates <- round(wstat1$povmap_global_subnational_infant_mortality_rates_v2_01, 0)

cgv$FORMAL_ <- NULL
cgv$ECONOMY <- NULL
cgv$INCOME_ <- NULL
cgv$ar_sqkm <- NULL
names(cgv)[names(cgv) == "frmng_s"] <- "farming_system"
names(cgv)[names(cgv) == "cgregin"] <- "cgregion"

# two excel sheets: one with the simple CG/frmng_s
library(writexl)
library(dplyr)
dd <- as.data.frame(cgv)
dd$farming_system <- gsub("[[:digit:]]+","", dd$farming_system)
dd$farming_system <- gsub("\\.","", dd$farming_system)
dd$farming_system <- trimws(dd$farming_system)

d1 <- dd %>% 
  select(-c(NAME_EN, ISO_A3)) %>%
  group_by(cgregion, farming_system) %>%
  summarize_all(mean, na.rm = T) %>%
  mutate(across(c(stunting_2017, poverty_5years, infant_mortality_rates), round)) %>%
  ungroup()


d2 <- dd %>% 
  group_by(cgregion, farming_system, NAME_EN, ISO_A3) %>%
  summarize_all(mean, na.rm = T) %>%
  mutate(across(c(stunting_2017, poverty_5years, infant_mortality_rates), round)) %>%
  ungroup()

tmp <- write_xlsx(list(cg_fs = d1, country_fs = d2), path = file.path(datadir, "output/summary_table_level1.xlsx"))



