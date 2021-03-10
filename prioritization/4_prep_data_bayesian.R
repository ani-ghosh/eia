# code to prepare the data for Bayesian analysis
# first create/get a reference raster at 5min or 10 km 
# create a 10*10 km grid for the entire world to run the data manipulation for each grid
# select the indicators, resample them to each grid and save the results as dataframe

library(terra)
library(sf)

datadir <- "G:\\My Drive\\work\\ciat\\eia\\analysis"

# reference raster
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))

# NUE/RUE is in vector
# using already merged vector data
vv <- st_read(file.path(datadir, "outdir/all_kpi_summary.geojson"))
nue <- vect(vv[,c("iso3.x", "nue_trend")])
nuec <- aggregate(nue, by = "iso3.x", fun = "mean", na.rm = TRUE, dissolve = TRUE)
nuer <- rasterize(nuec, ref, "mean_nue_trend")
writeRaster(nuer, file.path(datadir, "outdir/all_raster/nue_trend_global_raster_10km.tif"), 
            gdal=c("COMPRESS=LZW", "TFW=YES","of=COG"))

# all raster data
# cropland fraction
cr <- rast(file.path(datadir,"input/cropland/GFSAD1KCM.2010.001.2016348142550.tif"))
m <- c(-Inf, 0.5, NA, 0.5, 6, 1, 6, Inf, NA)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(cr, rclmat, include.lowest=TRUE)
# compute fractional coverage
rc2 <- aggregate(rc1, fact = 10, cores = 4,
                 fun = function(x, ...)
                 { (sum(x == 1, na.rm = TRUE)/100)*100})
names(rc2) <- "cropland_fraction"
rc2 <- resample(rc2, ref, method="bilinear",
  file.path(datadir, "outdir/all_raster/cropland_fraction_global_raster_10km.tif"))

# population
wpop <- file.path(datadir, "input/worldpop", "ruralpop_2020_1km_Aggregated.tif")
wpop <- rast(wpop)
wpop <- aggregate(wpop, fact = 10, fun = "sum", na.rm = TRUE, cores = 4)
names(wpop) <- "rural_population_sum"
wpop <- resample(wpop, ref, method="bilinear",
                file.path(datadir, "outdir/all_raster/rural_population_global_raster_10km.tif"))

# soil-npp
snpp <- rast(file.path(datadir, "outdir/all_raster/soil_npp.tif"))
snpp <- resample(snpp, ref, method="bilinear",
                 file.path(datadir, "outdir/all_raster/soil_npp_global_raster_10km.tif"))

# protected area presence
wdpa <- rast(file.path(datadir, "input/protected_area/wdparas_land_1km.tif"))
wdpa <- aggregate(wdpa, fact = 10, cores = 4,
                 fun = function(x, ...)
                 {(sum(x == 1, na.rm = TRUE)/100)*100})
names(wdpa) <- "protected_area_cover"
wdpa <- resample(wdpa, ref, method="bilinear",
                 file.path(datadir, "outdir/all_raster/wdpa_global_raster_10km.tif"))

##########################################################################
# yield gap
yldgap <- list.files(file.path(datadir, "input/earthstat/YieldGapMajorCrops_Geotiff"), 
                     pattern = glob2rx("*_yieldgap*tif$"), full.names = TRUE, recursive = TRUE)
yldgap <- grep("rice|maize|soy|wheat", yldgap, value = TRUE)
yldgap <- rast(yldgap)


# yield trends
yldtrend <- list.files(file.path(datadir, "input/earthstat/YieldTrends_Geotiff"), 
                       pattern = glob2rx("percentage_*.tif"), full.names = TRUE)
yldtrend <- rast(yldtrend)
names(yldtrend) <- paste0("yieldtrend_", names(yldtrend))

# yield variability
yldvar <- list.files(file.path(datadir, "input/earthstat/YieldClimateVariability_Geotiff"), 
                     pattern = glob2rx("coeff_*.tif"), full.names = TRUE)
yldvar <- rast(yldvar)
names(yldvar) <- paste0("yieldvariability_", names(yldvar))

# others
nuer <- rast(file.path(datadir, "outdir/all_raster/nue_trend_global_raster_10km.tif"))
cropfrac <- rast(file.path(datadir, "outdir/all_raster/cropland_fraction_global_raster_10km.tif"))
rpop <- rast(file.path(datadir, "outdir/all_raster/rural_population_global_raster_10km.tif"))
soilnpp <- rast(file.path(datadir, "outdir/all_raster/soil_npp_global_raster_10km.tif"))
wdpa <- rast(file.path(datadir, "outdir/all_raster/wdpa_global_raster_10km.tif"))

# all raster data stack
rr <- c(cropfrac, rpop, soilnpp, yldgap, yldtrend, yldvar, nuer, wdpa)
writeRaster(rr, file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"), 
            gdal=c("COMPRESS=LZW", "TFW=YES","of=COG"), overwrite = TRUE)


dd <- as.data.frame(rr, xy=TRUE, cells=TRUE, na.rm=FALSE)
dd[sapply(dd, is.nan)] <- NA

k <- apply(dd[, 5:ncol(dd)], 1, function(x)(sum(is.na(x))))
k1 <- which(k <= 20)
ddk <- dd[k1, ] 
write.csv(ddk, file.path(datadir, "outdir/all_raster/vars_table_10km.csv"), row.names = FALSE)
