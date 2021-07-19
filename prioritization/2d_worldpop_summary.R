# # individual 
# burl <- "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj"
# year <- 2020
# u <- file.path(burl, year, iso, paste0(tolower(iso), "_ppp_", year, "_1km_Aggregated_UNadj.tif"))
# download.file(u, destfile = basename(u), mode = "wb")

library(terra)

getRuralPopRaster <- function(i, wb, datadir, pop, retry = F){
  d <- wb[i,]
  iso <- d$ISO_A3
  
  oname <- file.path(datadir, "outdir/rural_population", paste0(iso, "_rural_pop_1km.tif"))
  if(retry) unlink(oname)
  
  if(!file.exists(oname)){
    cat("Processing", iso, "\n")
    th <- d$rural_pop_pct_mean/100
    v <- vect(raster::getData("GADM", country = iso, level = 0, path = file.path(datadir, "input/boundary/country")))
    r <- crop(pop, v)
    r <- mask(r, v)
    
    d$total_population_est <- as.numeric(global(r, sum, na.rm = T))
    
    # frequency table for each population density class
    f <- data.frame(freq(r))
    f$pop <-f$value*f$count
    f$pct <- f$pop/sum(f$pop)
    f$cumsum <- cumsum(f$pct)
    f <- f[order(f$cumsum, decreasing = TRUE),]
    pth <- f$value[DescTools::Closest(f$cumsum, th, which = TRUE, na.rm = TRUE)]
    
    # mask all values more than the threshold
    r[r > pth] <- NA
    d$rural_population_limit <- pth
    d$rural_population_est_1km <- as.numeric(global(r, sum, na.rm = T))
    
    dir.create(dirname(oname), F, T)
    writeRaster(r, filename = oname,
                overwrite = TRUE, gdal=c("COMPRESS=LZW"))
    rx <- aggregate(r, fact = 10, fun = "sum", na.rm = TRUE, filename = gsub("_1km.tif","_10km.tif",oname),
                    overwrite = TRUE, gdal=c("COMPRESS=LZW"))
    d$rural_population_est_10km <- as.numeric(global(rx, sum, na.rm = T))
    return(d) 
  }
} 

datadir <- "G:\\My Drive\\work\\ciat\\eia\\analysis"

pop <- rast(file.path(datadir, "input/worldpop/ppp_2020_1km_Aggregated.tif"))

# worldbank rural population %
# wbp <- "https://api.worldbank.org/v2/en/indicator/SP.RUR.TOTL.ZS?downloadformat=csv"
# download.file(wbp, destfile = file.path(datadir, "input/worldbank/worldbank_rpop.zip"), mode = "wb")
# unzip(file.path(datadir, "input/worldbank/worldbank_rpop.zip"), exdir = file.path(datadir, "input/worldbank/rpop"))
wb <- read.csv(file.path(datadir, "input/worldbank/rpop/API_SP.RUR.TOTL.ZS_DS2_en_csv_v2_2446153.csv"), skip = 4)
# mean of last 5 years for missing %
x <- rowMeans(wb[,paste0("X",2015:2019)], na.rm = TRUE)
x <- ifelse(is.na(x), rowMeans(wb[,paste0("X",2010:2019)], na.rm = TRUE), x)
wb$rural_pop_pct_mean <- x
wb <- wb[, c("Country.Name","Country.Code","Indicator.Name", "rural_pop_pct_mean")]
names(wb)[2] <- "ISO_A3"
wb <- wb[complete.cases(wb),]
wb <- wb[wb$rural_pop_pct_mean > 0, ]

# modified list of LMIC countires
# tiso <- c('AFG','AGO','ARM','AZE','BDI','BEN','BFA','BGD','BLZ','BOL','BTN','BWA','CAF','CHN','CIV','CMR','COD','COG','COL','CRI','CUB',
#           'DMA','DOM','DZA','ECU','EGY','ERI','ETH','GAB','GHA','GIN','GMB','GNB','GRD','GTM','GUY','HND','HTI','IDN','IND','IRN','IRQ',
#           'JAM','KAZ','KEN','KGZ','KHM','LAO','LBN','LBR','LBY','LKA','LSO','MAR','MDG','MEX','MLI','MMR','MOZ','MRT','MWI','MYS',
#           'NAM','NER','NGA','NIC','NPL','PAK','PAN','PER','PHL','PNG','PRY','PSE','RWA','SDN','SEN','SLB','SLE','SLV','SOM','SSD','SUR','SWZ','SYR',
#           'TCD','TGO','THA','TJK','TKM','TLS','TUN','TZA','UGA','UZB','VCT','VEN','VNM','YEM','ZMB','ZWE', 'ZAF')
# 
# wb <- wb[wb$ISO_A3 %in% tiso, ]
cc <- raster::ccodes()
wb <- wb[wb$ISO_A3 %in% cc$ISO3, ]

dd <- lapply(1:nrow(wb), getRuralPopRaster, wb, datadir, pop, retry = T)
# create table with the estimated population
dd <- do.call(rbind, dd)
write.csv(dd, file.path(datadir, "outdir/rural_population/global_rural_pop_summary.csv"), row.names = FALSE)

##################################################################################################
# convert to global 10km
rl <- list.files(file.path(datadir, "outdir/rural_population"),  
                 pattern = "_rural_pop_10km.tif", full.names = TRUE)
# rr <- lapply(rl, rast)
# rsrc <- src(rr)
# 
# m <- mosaic(rsrc, filename = paste0(datadir, "/outdir/rural_population/global_rural_pop_10km.tif"),
#             gdal=c("COMPRESS=LZW"), overwrite = TRUE)

# mosaic/merge keeps failing
vm <- vrt(rl, filename = paste0(datadir, "/outdir/rural_population/global_rural_pop_10km.vrt"),
          overwrite = TRUE)

###############################################################################################################

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

