# list of interesting datasets 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SEPATX

# subnational doing business https://www.doingbusiness.org/en/reports/subnational-reports
library(terra)

datadir <- "G:/My Drive/work/ciat/eia/analysis"

# convert sub-national poverty estimates to raster
snpov <- vect(file.path(datadir, "input/worldbank/global_subnational_poverty_nov2018/global_poverty_nov2018.shp"))
snpov19 <- snpov[,c("OBJECTID","CountryCod","ADM0_NAME","Name","poor_ppp19")]
# names(snpov19)[2] <- "iso3"
snpov19$poor_ppp19 <- round(snpov19$poor_ppp19*100, 2)

ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
names(ref) <- "elevation"
povr <- rasterize(snpov19, ref, "poor_ppp19", fun = mean, na.rm = T,
            filename = file.path(datadir, "outdir/worldbank/poor_ppp19_nov18.tif"), 
            gdal=c("COMPRESS=LZW"), overwrite = TRUE)


#################################################################################################################################
# CGIAR region countries
lac <- data.frame(cgregion = "LAC", ISO3 = c("MEX","GTM","SLV","NIC","HND","CRI","PAN","CUB","HTI","JAM","DOM","PRI",
                                             "TTO","GRD","COL","ECU","VEN","GUY","SUR","BRA","PER",
                                             "ARG","BOL","CHL","PRY","URY"))
wca <- data.frame(cgregion = "WCA", ISO3 = c("MRT","SEN","MLI","NER","NGA","TCD","GNB","GIN","SLE","LBR","CIV","TGO","GHA","BEN",
                                             "CMR","GNQ","GAB","COG","COD","AGO","BFA","CAF","GMB"))
esa <- data.frame(cgregion = "ESA", ISO3 = c("TZA","KEN","SSD","ERI","ETH","SOM","DJI","RWA","BDI","UGA","ZMB","MOZ","MWI","MDG",
                                             "ZWE","NAM","BWA","SWZ","LSO","ZAF","SYC","COM","SLB"))
cwana <- data.frame(cgregion = "CWANA", ISO3 = c("SDN","EGY","YEM","MAR","ESH","DZA","TUN","LBY","SAU","OMN","IRQ",
                                                 "IRN","JOR","LBN","SYR","PSE","TUR","GEO","AZE","ARM","TKM","UZB","KGZ","KAZ",
                                                 "TJK","AFG"))
sa <- data.frame(cgregion = "SA", ISO3 = c("IND","PAK","BGD","NPL","LKA","BTN"))
sea <- data.frame(cgregion = "SAE", ISO3 = c("CHN","MMR","THA","LAO","VNM","KHM","IDN","PHL","MYS","TLS","PNG"))
cgregions <- rbind(lac, wca, esa, cwana, sa, sea)


# national level data for 1.9 USD/daily estimate
# u <- "https://api.worldbank.org/v2/en/indicator/SI.POV.NAHC?downloadformat=excel"
u <- "https://api.worldbank.org/v2/en/indicator/SI.POV.DDAY?downloadformat=excel"
pfile <- file.path(datadir, "input/worldbank/national_poor_ppp190.xls")
if(!file.exists(pfile)) {download.file(u, pfile, mode = "wb")}
npov <- readxl::read_excel(pfile, sheet = 1, skip = 3)

# find the poverty values for countries missing poverty in the last 20 years
# npov <- npov[npov$`Country Code` %in% cgregions$ISO3, 
#              c("Country Name","Country Code","Indicator Name","Indicator Code",
#                2010:2020)]

npov <- npov[, c("Country Name","Country Code","Indicator Name","Indicator Code", 2010:2020)]

# compute last 5 year average
pp <- npov[ , as.character(2010:2020)]
pp <- unlist(apply(pp, 1, max, na.rm = TRUE))
pp <- data.frame(npov[,1:2], poverty_avg10yr = round(pp,2))
names(pp) <- c("country_name","iso3","poverty_max10yr")
write.csv(pp, gsub("national_poor_ppp190.xls", "national_poor_ppp190_max.csv", pfile), row.names = TRUE)

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


snpov <- vect(file.path(datadir, "input/worldbank/global_subnational_poverty_nov2018/global_poverty_nov2018.shp"))
cgv <- vect(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))
v <- cgv[cgv$ISO_A3 == "GHA",]

r <- rast(file.path(datadir, "outdir/worldbank/poor_ppp19_nov18.tif"))
r <- crop(r, v)
r <- mask(r, v)
sv <- crop(snpov, v)
plot(sv,"poor_ppp19", col = heat.colors(20))

plot(r, col = rev(heat.colors(10)))
plot(v, add = T)
text(v, v$frmng_s, cex = 1.25)
