# USAID request for RWA, UGA, BDI, DRC

# install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
library(sf)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

dd <- read.csv(file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.csv"))
iso <- c("BDI", "RWA", "UGA", "COD")
d1 <- dd[dd$ISO_A3 %in% iso, ]
# d1 <- d1[grep("Highland", d1$farming_system),]

write.csv(d1[,1:11], file.path(datadir, "outdir/level_stat/great_lakes_highlands_usaid.csv"), 
          row.names = FALSE)


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

cgv1 <- cgv[cgv$ISO_A3 %in% iso, ]
cgv2 <- cgv1[grep("Highland", cgv1$farming_system),]

# plot with elevation
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
names(ref) <- "elevation"

r <- crop(ref, cgv1)
plot(r, type="interval", breaks=c(0,500,1000,1500,4000), 
     plg=list(legend=c("<500", "500-1000","1000-1500",">1500"), 
              title = "elevation range"))
plot(cgv1, add = T, border = "red", lwd = 1)
plot(cgv2, add = T, border = "blue", lwd = 1.3)


uga <- cgv[cgv$ISO_A3=="UGA",]
r1 <- crop(ref, uga)
r1 <- mask(r1, uga)


v2 <- cgv1[cgv1$ISO_A3=="UGA" & cgv1$farming_system=="Highland perennial",]

r2 <- crop(r1, v2)
r2 <- mask(r2, v2)
r2[r2 < 1500] <- NA

# only limit to the SW region
e <- ext(c(29.5833333333333, 31, -1.5, 1))
r2 <- crop(r2, e)
# convert SpatVector
r2[!is.na(r2)] <- 1
r2v <- as.polygons(r2)
cgv <- r2v

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

# rural population
rpop <- rast(paste0(datadir, "/outdir/rural_population/global_rural_pop_10km.vrt"))
pop <- extract(rpop, cgv, fun = sum, na.rm = TRUE)
pop$ID <- NULL
pop <- round(pop)
names(pop) <- "rural_population"

# other raster
rr <- rast(file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"))
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
pov <- rast(file.path(datadir, "outdir/worldbank/poor_ppp19_nov18.tif"))
# pov1 <- subset(rr, c("poverty_avg5yr_imputed"))
poverty <- extract(pov, cgv, fun = mean, na.rm = TRUE)
poverty$ID <- NULL
names(poverty) <- "poverty(percentage_subnational)"
poverty <- round(poverty)
poverty[poverty < 0] <- NA

region <- "uganda_sw_highlands"

level1 <- data.frame(region = region, pop, cropland, 
                     cropland_percapita,
                     cropland_perfamily,
                     poverty,
                     health, check.names = FALSE)

level1[sapply(level1, is.nan)] <- NA
names(level1) <- paste0("level1_", names(level1))

write.csv(level1, "C:/Users/anibi/Downloads/uganda_sw_highlands.csv", row.names = FALSE)

# plot(r1, type="interval", breaks=c(0,500,1000,1500,4000), 
#      plg=list(legend=c("<500", "500-1000","1000-1500",">1500"), 
#               title = "elevation range"))
# plot(cgv1, add = T, border = "red", lwd = 1)
# plot(cgv2, add = T, border = "blue", lwd = 1.3)
# text(cgv2, "farming_system")
# 
# # classify by elevation
# m <- c(0, 1000, 1,
#        1000, 1500, 2,
#        1500, 4000, 3)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# rc1 <- classify(r1, rclmat, include.lowest=TRUE)