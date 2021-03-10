# estimate from EarthStat raster
library(terra)


getEarthstatSummary <- function(i, cfs, rr){
  v <- cfs[i,]
  cat("processing pop stat", v$uid, "\n")
  
  # summary stat for each district
  wstat <- extract(rr, v, fun = mean, na.rm = TRUE)
  wstat <- wstat[,2:ncol(wstat)]
  wstat <- data.frame(uid = v$uid, wstat, stringsAsFactors = FALSE)
  
  return(wstat)
}

######################################################################################
# working directory
datadir <- "G:\\My Drive\\work\\ciat\\eia\\analysis"


######################################################################################
# boundaries
cfs <- vect("input/boundary/country/country_farming_system.shp")

######################################################################################
# all raster
# yield trends
yldtrend <- list.files(file.path(datadir, "input/earthstat/YieldTrends_Geotiff/"), 
                       pattern = glob2rx("percentage_*.tif"), full.names = TRUE)
yldtrend <- rast(yldtrend)
names(yldtrend) <- paste0("yieldtrend_", names(yldtrend))

# yield variability
yldvar <- list.files("input/earthstat/YieldClimateVariability_Geotiff/", 
                     pattern = glob2rx("coeff_*.tif"), full.names = TRUE)
yldvar <- rast(yldvar)
names(yldvar) <- paste0("yieldvariability_", names(yldvar))

# yield gap fraction
# yield gap fraction = 1- actual yield/climatic potential yield
# places with a low yield gap (close to zero) have yields at or near their climatic potential

yldgap <- list.files("input/earthstat/YieldGapMajorCrops_Geotiff/", 
                     pattern = glob2rx("*_yieldgap.tif"), full.names = TRUE, recursive = TRUE)
yldgap <- rast(yldgap)
names(yldgap) <- paste0("yieldgap_", names(yldgap))

# combine all
rr <- c(yldtrend, yldvar, yldgap)
######################################################################################

# run summary extraction
dd <- lapply(1:nrow(cfs), getEarthstatSummary, cfs, rr)
dds <- data.table::rbindlist(dd, fill = TRUE)
write.csv(dds, "outdir/earthstat/yield_summary_earthstat.csv", row.names = FALSE)
