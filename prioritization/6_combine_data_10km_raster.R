library(terra)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

# reference raster
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
names(ref) <- "elevation"

# area of cells
area <- area(ref, sum = FALSE)
# mask NA
area <- mask(area, ref)
# convert to sqkm
area <- area*1e-6
names(area) <- "area_sqkm"

# raster of all indicators
#################################################################################################
# population
# r1 <- rast(file.path(datadir, "outdir/all_raster/cropland_fraction_global_raster_10km.tif"))
# rural-urban catchment
# ruc <- file.path(datadir, "input\\urban_rural\\Urban-Rural Catchment Areas (URCA).tif") 
# ruc <- rast(ruc)
# # urban center population
# e <- ext(c(-178,178,-58,82))
# pop <- crop(pop, e, snap = "in")
# ruc <- crop(ruc, e, snap = "in")
# ext(pop) <- e
# ext(ruc) <- e
# 
# # in catchment data, 1 are urban centers
# m <- c(0, 1, NA,
#        1.5, Inf, 1)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# rur <- classify(ruc, rclmat, include.lowest=TRUE)
# rpop <- mask(pop, ruc, maskvalue = 1, 
#              filename = file.path(datadir, "input\\worldpop\\ruralpop_2020_1km_Aggregated.tif"),
#              overwrite = TRUE, gdal=c("COMPRESS=LZW", "TFW=YES"))
# 
# rpop <- aggregate(rpop, fact = 10, fun = "sum", na.rm = TRUE, cores = 4)
# names(rpop) <- "rural_population_sum"
# rpop <- resample(rpop, ref, method="bilinear", overwrite = TRUE,
#                  file.path(datadir, "outdir/all_raster/rural_population_global_raster_10km.tif"))

rpop <- rast(file.path(datadir, "outdir/all_raster/rural_population_global_raster_10km.tif"))

#################################################################################################
imr <- rast(file.path(datadir, "outdir/all_raster/povmap-global-subnational-infant-mortality-rates-v2-01_10km.tif")) 

#################################################################################################
# access
acc <- rast(file.path(datadir, "outdir/all_raster/accessibility_to_cities_10km.tif"))

#################################################################################################
# wdpa layers
wdpa <- rast(file.path(datadir, "outdir/all_raster/wdpa_global_raster_10km.tif"))

#################################################################################################
# new soil layers
sn <- rast(file.path(datadir, "outdir/all_raster/soil_npp_global_raster_10km.tif"))
soc <- subset(sn, "soc30cmmean")/10 # dg/kg --> g/kg
clay <- subset(sn, "clay30cmmean") # g/kg
silt <- subset(sn, "silt30cmmean") # g/kg
ph <- subset(sn, "ph30cmmean")/10 # scale 10! 
sh <- soc/(clay + silt)
names(sh) <- "soilhealth30cmmean"
npp <- subset(sn, "npptrend")

#################################################################################################
# cropland area
cfrac <- list.files(file.path(datadir, "input/earthstat/CroplandPastureArea2000_Geotiff"), 
                     pattern = glob2rx("*Cropland2000_5m*tif$"), full.names = TRUE, recursive = TRUE)
cfrac <- rast(cfrac)
cfrac <- cfrac*100 # convert to %
names(cfrac) <- "cropland_fraction"

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

################################################################################################
# network coverage
ntw <- file.path(datadir, 
                 "input/digital_divide_agriculture/Mehrabi2020_SI_10.1038_s41893-020-00631-0/dat/SI_A/Data/Out/tech.all.eck4.tif")
ntw <- rast(ntw)
names(ntw) <- "network_coverage"
ntw <- resample(ntw, rpop, method = "near")

################################################################################################
# farmsize
fsz <- file.path(datadir, 
                 "input/digital_divide_agriculture/Mehrabi2020_SI_10.1038_s41893-020-00631-0/dat/SI_A/Data/Out/farmsize.tif")
fsz <- rast(fsz)

################################################################################################
# diversity
div <- file.path(datadir, 
                 "outdir/all_raster/diversity_HarvestedArea_175crops.tif")
div <- rast(div)
names(div) <- "diversity_175_crops"

# combine the ones we need
rr1 <- c(rpop, area, ref, cfrac, imr, acc, wdpa, npp, ph, sh, div, fsz, yldgap, yldpot, yldtrend, yldvar, ntw)

# others --- referenced to CGIAR regions
pnr <- rast(file.path(datadir, "outdir/all_raster/poverty_nue.tif"))

# crop and mask to cgiar regions
rr1 <- crop(rr1, pnr)
rr <- mask(rr1, pnr)
rr <- c(pnr, rr)

writeRaster(rr, file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"), 
            gdal=c("COMPRESS=LZW", "TFW=YES"), overwrite = TRUE, datatype="FLT4S")

#########################################################################################
cgr <- file.path(datadir, "outdir/all_raster/cgregion_country_fs_raster_10km.tif")

if(!file.exists(cgr)){
  cgv <- vect( "G:/My Drive/work/ciat/eia/analysis/input/boundary/country_farming_system_cg_regions.shp")
  rr <- rast(file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"))
  names(rr)
  
  # convert specific attributes to raster
  cgvr <- lapply(c("NAME_EN","ISO_A3","cgregin","frmng_s"), function(x) {rasterize(cgv, rr, x)})
  cgvr <- do.call(c, cgvr)
  writeRaster(cgvr, cgr, 
              gdal=c("COMPRESS=LZW", "TFW=YES"), overwrite = TRUE)
} else {
  cgvr <- rast(cgr)
}


# combine them
rr <- c(cgvr, rr)

# convert to dataframe
dd <- as.data.frame(rr, xy=TRUE, cells=TRUE, na.rm=FALSE)
dd$NAME_EN <- as.character(dd$NAME_EN)
dd$ISO_A3 <- as.character(dd$ISO_A3)
dd$cgregin <- as.character(dd$cgregin)
dd$frmng_s <- as.character(dd$frmng_s)

# dds <- dd[!is.na(dd$NAME_EN),]
# dds[sapply(dds, is.nan)] <- NA
dds <- dd[!is.na(dd$cropland_fraction),]
dds <- dds[dds$cropland_fraction > 0,]
dds$maize_yieldgap_scaled <- dds$maize_yieldgap/dds$maize_yieldpotential
dds$rice_yieldgap_scaled <- dds$rice_yieldgap/dds$rice_yieldpotential
dds$wheat_yieldgap1_scaled <- dds$wheat_yieldgap1/dds$wheat_yieldpotential

data.table::fwrite(dds, file.path(datadir, "outdir/all_raster/vars_table_10km.csv"))


x <- rr[[1]]
values(x) <- NA
x[dds$cell] <- dds$nue_avg10yr_imputed
x[dds$cell] <- dds$elevation
plot(x)

# raster stack for interview
rr <- rast(file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"))
tokeep <- c("area_sqkm","elevation",
            "cropland_fraction", "poverty_avg5yr_imputed","rural_population_sum",
            "povmap_global_subnational_infant_mortality_rates_v2_01", 
            "2015_accessibility_to_cities_v1.0","protected_area_cover",                                  
            "npptrend","nue_avg5yr_imputed","ph30cmmean","soilhealth30cmmean","cassava_yieldgap","maize_yieldgap","millet_yieldgap","potato_yieldgap",
            "rice_yieldgap","sorghum_yieldgap","wheat_yieldgap1",                                       
            "cassava_yieldpotential","maize_yieldpotential", "millet_yieldpotential","potato_yieldpotential",                                 
            "rice_yieldpotential","sorghum_yieldpotential","wheat_yieldpotential","yieldtrend_percentage_maize",                           
            "yieldtrend_percentage_rice","yieldtrend_percentage_wheat", "yieldvariability_coeff_maize",
            "yieldvariability_coeff_rice","yieldvariability_coeff_wheat")

rr1 <- subset(rr, tokeep, 
              filename = file.path(datadir, "outdir/all_raster/var_subset_LMIC_raster_10km.tif"),
              gdal=c("COMPRESS=LZW"))
