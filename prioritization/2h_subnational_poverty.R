library(raster)

# national level data for 1.9 USD/daily estimate
u <- "https://api.worldbank.org/v2/en/indicator/SI.POV.NAHC?downloadformat=excel"
pfile <- file.path(datadir, "input/worldbank/national_poor_ppp.xls")
if(!file.exists(pfile)) {download.file(u, pfile, mode = "wb")}
npov <- readxl::read_excel(pfile, sheet = 1, skip = 3)

# compute last 5 year average
p5 <- npov[, names(npov) %in% tail(names(npov), 10)]
p5 <- unlist(apply(p5, 1, max, na.rm = TRUE))
p5[!is.finite(p5)] <- NA
pov5 <- data.frame(npov[,1:2], poverty_avg5yr = round(p5,2))
names(pov5) <- c("country_name","iso3","poverty_avg5yr")

# replace missing/NA values in nub-national poverty with national estimates
snpov <- shapefile(file.path(datadir, "input/worldbank/global_subnational_poverty_nov2018/global_poverty_nov2018.shp"))
snpov19 <- snpov[,c("OBJECTID","CountryCod","ADM0_NAME","ADM1_NAME","poor_ppp19")]
names(snpov19)[2] <- "iso3"
snpov19$poor_ppp19 <- round(snpov19$poor_ppp19*100, 2)

library(rqdatatable)
ss <- natural_join(snpov19@data, pov5, by = "iso3", jointype = "LEFT")
ss$poor_ppp19_est <- ifelse(ss$poor_ppp19 < 0, ss$poverty_avg5yr, ss$poor_ppp19)
snpov19 <- merge(snpov19, ss[,c("OBJECTID","poverty_avg5yr","poor_ppp19_est")], by = "OBJECTID")

shapefile(snpov19, 
          file.path(datadir, "outdir/worldbank/global_subnational_poverty_nov2018_gapfilled.shp"),
          overwrite = TRUE)

povv <- vect(file.path(datadir, "outdir/worldbank/global_subnational_poverty_nov2018_gapfilled.shp"))
names(povv) <- c("OBJECTID","iso3","ADM0_NAME","ADM1_NAME","poor_ppp19","poverty_avg5yr","poor_ppp19_est")
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
names(ref) <- "elevation"
povr <- rasterize(povv, ref, "poor_ppp19_est", fun = mean, na.rm = T)
writeRaster(povr, file.path(datadir, "outdir/worldbank/poor_ppp19_est.tif"), 
            gdal=c("COMPRESS=LZW"), overwrite = TRUE)