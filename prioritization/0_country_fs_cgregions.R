# country*farming systems*CG regions 

library(raster)
library(rgeos)
library(sf)
library(rmapshaper)
library(tidyverse)

#https://gis.stackexchange.com/questions/379693/crop-wrld-simpl-gives-error-after-r-update
rgeos::set_RGEOS_CheckValidity(2L)

###################################################################################################
vdir <- "G:\\My Drive\\work\\ciat\\cg-prioritization"
fs <- shapefile(file.path(vdir, "vector/farming_system_combined_2001_12.shp"))

# low resolution boundaries from WB https://datacatalog.worldbank.org/dataset/world-bank-official-boundaries
furl <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/779551/wb_boundaries_geojson_lowres.zip"
zfile <- file.path(vdir, "vector", basename(furl))
if(!file.exists(zfile)){
  download.file(furl, zfile, mode = "wb")
  unzip(zfile, exdir = dirname(zfile))
}

# CG regions
rg <- shapefile(file.path(vdir, "vector/CGIAR_regions_shp/cgiar_regions.shp"))

# CGIAR region countries
lac <- data.frame(cgregion = "LAC", ISO3 = c("MEX","GTM","BLZ","SLV","NIC","HND","CRI","PAN","CUB","HTI","JAM","DOM","BHS","PRI",
                                             "VIR","VCT","DMA","BRB","TTO","GRD","TCA","COL","ECU","VEN","GUY","SUR","BRA","PER",
                                             "ARG","BOL","CHL","PRY","URY"))
wca <- data.frame(cgregion = "WCA", ISO3 = c("MRT","SEN","MLI","NER","NGA","TCD","GNB","GIN","SLE","LBR","CIV","TGO","GHA","BEN",
                                             "CMR","GNQ","GAB","COG","COD","AGO","BFA","CAF","GMB"))
esa <- data.frame(cgregion = "ESA", ISO3 = c("TZA","KEN","SSD","ERI","ETH","SOM","DJI","RWA","BDI","UGA","ZMB","MOZ","MWI","MDG",
                                             "ZWE","NAM","BWA","SWZ","LSO","ZAF","SYC","COM","SLB"))
cwana <- data.frame(cgregion = "CWANA", ISO3 = c("SDN","EGY","YEM","MAR","ESH","DZA","TUN","LBY","SAU","OMN","ARE","QAT","KWT","IRQ",
                                                 "IRN","JOR","LBN","SYR","ISR","PSE","TUR","GEO","AZE","ARM","TKM","UZB","KGZ","KAZ",
                                                 "TJK","AFG"))

sa <- data.frame(cgregion = "SA", ISO3 = c("IND","PAK","BGD","NPL","LKA","BTN"))
sea <- data.frame(cgregion = "SAE", ISO3 = c("CHN","MMR","THA","LAO","VNM","KHM","IDN","PHL","MYS","TLS","PNG","BRN"))

cgregions <- rbind(lac, wca, esa, cwana, sa, sea)

# modified list of LMIC countires
# tsio <- c('AFG','AGO','ARM','AZE','BDI','BEN','BFA','BGD','BLZ','BOL','BTN','BWA','CAF','CHN','CIV','CMR','COD','COG','COL','CRI','CUB',
#           'DMA','DOM','DZA','ECU','EGY','ERI','ETH','GAB','GHA','GIN','GMB','GNB','GRD','GTM','GUY','HND','HTI','IDN','IND','IRN','IRQ',
#           'JAM','KAZ','KEN','KGZ','KHM','LAO','LBN','LBR','LBY','LKA','LSO','MAR','MDG','MEX','MLI','MMR','MOZ','MRT','MWI','MYS',
#           'NAM','NER','NGA','NIC','NPL','PAK','PAN','PER','PHL','PNG','PRY','PSE','RWA','SDN','SEN','SLB','SLE','SLV','SOM','SSD','SUR','SWZ','SYR',
#           'TCD','TGO','THA','TJK','TKM','TLS','TUN','TZA','UGA','UZB','VCT','VEN','VNM','YEM','ZMB','ZWE')
# 
# cgregions <- cgregions[cgregions$ISO3 %in% tiso, ]

################################################################################################
# simplified vector boundaries
vdir <- "G:\\My Drive\\work\\ciat\\cg-prioritization"
cb <- file.path(vdir, "vector/WB_Boundaries_GeoJSON_lowres/WB_countries_Admin0_lowres.geojson")
cb <- st_read(cb)
cb <- as_Spatial(cb)

# merge country names
cgregions <- merge(cgregions, cb[,c("NAME_EN", "ISO_A3")], by.x = "ISO3", by.y = "ISO_A3", all.x = TRUE)
cgregions$ISO3[is.na(cgregions$NAME_EN)]
cgregions$NAME_EN[cgregions$ISO3 == "ESH"] <- "Western Sahara"

# convert to spatial
cgsp <- cb[cb$ISO_A3 %in% cgregions$ISO3,]
cgsp <- merge(cgsp, cgregions, by.x = "ISO_A3", by.y = "ISO3", all.x = TRUE)

#################################################################################################################
# famring system merge
# 2000 + 2012 Africa + CA dry corridor
dc <- shapefile(file.path(vdir, "vector/dry corridor CA/corredor_seco 4 countries.shp"))
dc <- spTransform(dc, crs(fs))
# remove small polygons (< 1% area)
a <- area(dc)*100/sum(area(dc))
dc <- dc[a >= 1,]

# simply boundaries
dc1 <- ms_simplify(dc, keep = 0.0025) 
dc1 <- gBuffer(dc1, byid=TRUE, width=0)
dc1 <- aggregate(dc1, by = "ORIG_FID")
dc1$frm_sys <- "Dry corridor Central America"
dc1$year <- 2021
dc1$ORIG_FID <- NULL

fs1 <- fs - dc1
fs1 <- bind(fs1, dc1)

shapefile(fs1, file.path(vdir, "vector/farming_system_combined_2001_12_ca_dry_corridor.shp"), overwrite = TRUE)

#########################################################################################
# combine with farming systems
combineFSregion <- function(i, cbs, fs){
  cs <- cbs[i,]
  cat("processing ", cs$ISO_A3, "\n")
  fsv <- crop(fs, cs)
  if(!is.null(fsv)){  
    fsv <- aggregate(fsv, by = "frm_sys")
    names(fsv) <- "farming_system"
    a <- round(area(fsv)/(1000*1000))
    fsv@data <- data.frame(cs@data, fsv@data, area_sqkm = a)
    return(fsv)
  }
}

vv <- lapply(1:nrow(cgsp), combineFSregion, cgsp, fs1)
vv[sapply(vv, is.null)] <- NULL
vs <- do.call(rbind,vv)
vs <- vs[,c("NAME_EN.x","FORMAL_EN","ISO_A3","cgregion","farming_system","ECONOMY","INCOME_GRP","area_sqkm")]
names(vs)[1] <- "NAME_EN" 
shapefile(vs, "G:/My Drive/work/ciat/eia/analysis/input/boundary/country_farming_system_cg_regions.shp",
          overwrite = TRUE)





############################################################################################################################
# all GADM country 
# cc <-ccodes()
# cb <- lapply(cc$ISO3, function(x){
#   try(getData('GADM', country = x, level = 0, path = "G:/My Drive/work/ciat/eia/analysis/input/boundary/country/"), silent = TRUE)
# })


############################################################################################################
cb <- file.path(vdir, "vector/WB_Boundaries_GeoJSON_lowres/WB_countries_Admin0_lowres.geojson")
cb <- st_read(cb)
cb <- as_Spatial(cb)
cb <- cb[,c("FORMAL_EN","ISO_A2","ISO_A3","UN_A3","CONTINENT","ECONOMY","INCOME_GRP")]

# focus only on the countries that are within any of the CG-regions
cbs <- crop(cb, rg)
cbs <- cbs[cbs$ISO_A3 != "-99",]

############################################################################################################

combineFSregion <- function(i, cbs, fs){
  cs <- cbs[i,]
  cat("processing ", cs$ISO_A3, "\n")
  fsv <- crop(fs, cs)
  if(!is.null(fsv)){  
    fsv <- aggregate(fsv, by = "frm_sys")
    names(fsv) <- "farming_system"
    a <- round(area(fsv)/(1000*1000))
    fsv@data <- data.frame(cs@data, fsv@data, area_sqkm = a)
    return(fsv)
  }
}

vv <- lapply(1:nrow(cbs), combineFSregion, cbs, fs)
vv[sapply(vv, is.null)] <- NULL
vs <- do.call(rbind,vv)

vss <- vs[vs$ISO_A3 %in% cgc,]

shapefile(vs, "G:/My Drive/work/ciat/eia/analysis/input/boundary/country_farming_system_cg_regions.shp",
          overwrite = TRUE)
write.csv(vs@data, "G:/My Drive/work/ciat/eia/analysis/input/boundary/country_farming_system_cg_regions.csv",
          row.names = FALSE)

vs <- shapefile("G:/My Drive/work/ciat/eia/analysis/input/boundary/country_farming_system_cg_regions.shp")
rvs <- vs[,c("cgregin", "NAME_EN", "frmng_s")]
rvs <- rvs@data
names(rvs) <- c("CG_region", "Country", "Farming_System" )
rvs$Farming_System <- gsub("[[:digit:]]+","", rvs$Farming_System)
rvs$Farming_System <- gsub("\\.","", rvs$Farming_System)
rvs$Farming_System <- trimws(rvs$Farming_System)
write.csv(rvs, "G:/My Drive/work/ciat/eia/analysis/input/boundary/country_farming_system_cg_regions_simple.csv",
          row.names = FALSE)
