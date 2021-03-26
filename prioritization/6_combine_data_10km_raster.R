library(terra)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

# raster of all indicators
r1 <- rast(file.path(datadir, "outdir/all_raster/cropland_fraction_global_raster_10km.tif"))
r2 <- rast(file.path(datadir, "outdir/all_raster/rural_population_global_raster_10km.tif"))
r3 <- rast(file.path(datadir, "outdir/all_raster/wdpa_global_raster_10km.tif"))

# new soil layers
sn <- rast(file.path(datadir, "outdir/all_raster/soil_npp_global_raster_10km.tif"))
soc <- subset(sn, "soc30cmmean")/10 # dg/kg --> g/kg
clay <- subset(sn, "clay30cmmean") # g/kg
silt <- subset(sn, "silt30cmmean") # g/kg
ph <- subset(sn, "ph30cmmean")/10 # scale 10! 
sh <- soc/(clay + silt)
names(sh) <- "soilhealth30cmmean"
npp <- subset(sn, "npptrend")

# yield gap
yldgap <- list.files(file.path(datadir, "input/earthstat/YieldGapMajorCrops_Geotiff"), 
                     pattern = glob2rx("*_yieldgap*tif$"), full.names = TRUE, recursive = TRUE)
yldgap <- grep("cassava|maize|millet|potato|rice|sorghum|wheat", yldgap, value = TRUE)
yldgap <- rast(yldgap)

# attainable yield
yldpot <- list.files(file.path(datadir, "input/earthstat/YieldGapMajorCrops_Geotiff"), 
                     pattern = glob2rx("*_yieldpotential*tif$"), full.names = TRUE, recursive = TRUE)
yldpot <- grep("cassava|maize|millet|potato|rice|sorghum|wheat", yldpot, value = TRUE)
yldpot <- rast(yldpot)

# yield trends
yldtrend <- list.files(file.path(datadir, "input/earthstat/YieldTrends_Geotiff"), 
                       pattern = glob2rx("percentage_*.tif"), full.names = TRUE)
yldtrend <- grep("cassava|maize|millet|potato|rice|sorghum|wheat", yldtrend, value = TRUE)
yldtrend <- rast(yldtrend)
names(yldtrend) <- paste0("yieldtrend_", names(yldtrend))

# yield variability
yldvar <- list.files(file.path(datadir, "input/earthstat/YieldClimateVariability_Geotiff"), 
                     pattern = glob2rx("coeff_*.tif"), full.names = TRUE)
yldvar <- grep("cassava|maize|millet|potato|rice|sorghum|wheat", yldvar, value = TRUE)
yldvar <- rast(yldvar)
names(yldvar) <- paste0("yieldvariability_", names(yldvar))


# combine the ones we need
rr1 <- c(r1, r2, r3, npp, ph, sh, yldgap, yldpot, yldtrend, yldvar)

# others --- referenced to CGIAR regions
pnr <- rast(file.path(datadir, "outdir/all_raster/poverty_nue.tif"))

# crop and mask to cgiar regions
rr1 <- crop(rr1, pnr)
rr <- mask(rr1, rcg)
rr <- c(pnr, rr)

writeRaster(rr, file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"), 
            gdal=c("COMPRESS=LZW", "TFW=YES"), overwrite = TRUE, datatype="FLT4S")

#########################################################################################
library(terra)
cgv <- vect( "G:/My Drive/work/ciat/eia/analysis/input/boundary/country_farming_system_cg_regions.shp")
rr <- rast(file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"))
names(rr)

# convert specific attributes to raster
cgvr <- lapply(c("NAME_EN","ISO_A3","cgregin","frmng_s"), function(x) {rasterize(cgv, rr, x)})
cgvr <- do.call(c, cgvr)
writeRaster(cgvr, file.path(datadir, "outdir/all_raster/cgregion_country_fs_raster_10km.tif"), 
            gdal=c("COMPRESS=LZW", "TFW=YES"), overwrite = TRUE)

# combine them
rr <- c(cgvr, rr)

# convert to dataframe
dd <- as.data.frame(rr, xy=TRUE, cells=TRUE, na.rm=FALSE)
dd$NAME_EN <- as.character(dd$NAME_EN)
dd$ISO_A3 <- as.character(dd$ISO_A3)
dd$cgregin <- as.character(dd$cgregin)
dd$frmng_s <- dd$frmng_s

dds <- dd[!is.na(dd$NAME_EN),]
dds[sapply(dds, is.nan)] <- NA
dds$maize_yieldgap_scaled <- dds$maize_yieldgap/dds$maize_yieldpotential
dds$rice_yieldgap_scaled <- dds$rice_yieldgap/dds$rice_yieldpotential
dds$wheat_yieldgap1_scaled <- dds$wheat_yieldgap1/dds$wheat_yieldpotential

data.table::fwrite(dds, file.path(datadir, "outdir/all_raster/vars_table_10km.csv"))
