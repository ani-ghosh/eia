# worldpop
library(terra)

getPopByUnit <- function(s, vr, pop, cr){
  cat("processing ", s , "of", nrow(vr), "\n")
  vs <- vr[s,]
  # population
  r1 <- crop(pop, vs)
  r1 <- mask(r1, vs)
  pp <- global(r1, fun="sum", na.rm = TRUE)
  
  # cropland area
  r2 <- crop(cr, vs)
  r2 <- mask(r2, vs)
  # fr2 <- as.data.frame(freq(r2))
  # cfr <- fr2[fr2$value %in% 1:5, ]
  # remove non-crop classes
  m <- c(-Inf, 0.5, NA, 0.5, 6, 1, 6, Inf, NA)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  rc1 <- classify(r2, rclmat, include.lowest=TRUE)
  
  # combine all information  
  dd <- data.frame(as.data.frame(vs), rural_pop = round(pp$sum), 
                   cropland_area_ha = round(area(rc1)*0.0001))
  dd$rural_pop_density_sq_km <- round(dd$rural_pop/dd$area_sq_km)
  dd$cropland_ha_capita <- round(dd$cropland_area_ha/dd$rural_pop, 3)
  vv <- merge(vs, dd)
  return(vv)
}

# input data
datadir <- "G:\\My Drive\\work\\ciat\\eia\\analysis"

# population
wpop <- rast(file.path(datadir, "input/worldpop", "ruralpop_2020_1km_Aggregated.tif"))

# global cropland from GSFAD
# https://lpdaac.usgs.gov/products/gfsad1kcmv001/
# https://e4ftl01.cr.usgs.gov/MEASURES/GFSAD1KCM.001/2007.01.01/
# 0:ocean; 1,2:Croplands, Irrigation; 3,4,5:Croplands, Rainfed; 9: Non-cropland
cr <- rast(file.path(datadir,"input/cropland/GFSAD1KCM.2010.001.2016348142550.tif"))

# farming system and region combined map
fsv <- shapefile(file.path(datadir, "input/boundary/farming_system_country_combined_filtered.shp"))
# need to have at least 100 sq km  
fsv <- fsv[fsv$area_sq_km >= 100, ]
# dissolve unique farming system for each country 
fsv <- vect(fsv)

# process for all polygons
vv <- lapply(1:nrow(fsv), getPopByUnit, fsv, wpop, cr)
vd <- do.call(c, vv)
sfd <- sf::st_as_sf(as.data.frame(vd, geom=TRUE), wkt="geometry", crs=crs(vd))
# sf::st_write(sfd, "app/level1_KPI.geojson")
saveRDS(sfd, "app/level1_KPI.rds" )


# # individual 
# burl <- "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj"
# year <- 2020
# u <- file.path(burl, year, iso, paste0(tolower(iso), "_ppp_", year, "_1km_Aggregated_UNadj.tif"))
# download.file(u, destfile = basename(u), mode = "wb")
