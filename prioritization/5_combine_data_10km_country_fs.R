library(terra)
library(raster)
library(sf)

###############################################################
# functions
yieldSummary <- function(rr, var = "yieldgap"){
  yg <- subset(rr, grep(var, names(rr)))
  mgp <- mean(yg, na.rm = TRUE)
  names(mgp) <- paste0("mean_", var)
  yg <- c(mgp, yg)
  ygm <- global(yg, fun="mean", na.rm = TRUE)
  colnames(ygm) <- NULL
  ygm <- data.frame(t(ygm), row.names = NULL)
  return(ygm)
}

getStatByUnit <- function(s, vv, r){
  cat("processing ", s , "of", nrow(vv), "\n")
  vs <- vv[s,]
  
  ##############################################################
  rr <- crop(r, vs)
  rr <- mask(rr, vs)
  ###############################################################
  # Level 1
  # population
  r1 <- subset(rr, "rural_population_sum")
  pp <- global(r1, fun="sum", na.rm = TRUE)
  
  # cropland area
  r2 <- subset(rr, "cropland_fraction")
  # get area in sq km
  ar2 <- area(r2, sum = FALSE)/(1000*1000)
  # multiply with fractional crop area/100
  fr2 <- ar2*r2/100
  # compute total area 
  crp <- global(fr2, fun="sum", na.rm = TRUE)
  
  # protected area cover
  pa <- subset(rr, "protected_area_cover")
  apa <- area(pa, sum = FALSE)/(1000*1000)
  fpa <- apa*pa/100
  apa <- global(fpa, fun="sum", na.rm = TRUE)
  
  ###############################################################
  # Level 2
  # soil
  # https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/access_on_gee.md
  soc <- subset(rr, "soc30cmmean") # dg/kg
  clay <- subset(rr, "clay30cmmean") # g/kg
  silt <- subset(rr, "silt30cmmean") # g/kg
  ph <- subset(rr, "ph30cmmean")/10 # scale 10! 
  sh <- soc/(clay + silt)
  soilhealth <- global(sh, fun="mean", na.rm = TRUE)
  soilph <- global(ph, fun="mean", na.rm = TRUE)
  
  # yield gap
  ygm <- yieldSummary(rr, var = "yieldgap")
  
  # yield trend
  ygt <- yieldSummary(rr, var = "yieldtrend_percentage")
  
  # yield variability
  ygv <- yieldSummary(rr, var = "yieldvariability_coeff")
  
  # npp
  npp <- subset(rr, "mean_nue_trend")
  npp <- global(npp, fun="mean", na.rm = TRUE)
  
  # combine all information  
  dd <- data.frame(rural_population = round(pp$sum), cropland_area_ha = crp$sum)
  dd$rural_population_density_sq_km <- round(dd$rural_population/vs$area_sqkm)
  dd$cropland_ha_capita <- round(dd$cropland_area_ha*100/dd$rural_population, 3)
  dd <- data.frame(dd, protected_area_fraction = round(apa$sum*100/vs$area_sqkm, 2),
                   soil_health = soilhealth$mean, soil_ph = soilph$mean,
                   ygm, ygt, ygv, npp_trend = npp$mean)
  vs <- as(vs, "Spatial")
  vm <- merge(vs, dd)
  return(vm)
}

#############################################################################################
# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"
# using already merged vector data
vv <- vect(file.path(datadir, "input/boundary/country/country_farming_system.shp"))
# combine farming systems within each country?
# vv <- vv[vv$area_pct >= 1, ] # reomve very small ones
# need to have at least 100 sq km  
vv <- vv[vv$area_pct >= 1, ]

# raster of all indicators
r <- rast(file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"))

# process for all polygons
vals <- lapply(1:nrow(vv), getStatByUnit, vv, r)
vd <- do.call(rbind, vals)
sfd <- sf::st_as_sf(vd)
sf::st_write(sfd, file.path(datadir, "outdir/all_kpi_summary.geojson"), delete_layer = TRUE)

