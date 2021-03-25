# resource use efficiency trend
library(raster)
library(terra)
library(dplyr)
library(sf)

# working directory
datadir <- "G:\\My Drive\\work\\ciat\\eia\\analysis"

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
cgregions <- rbind(lac, wca, esa, cwana)

# vector boundaries
vdir <- "G:\\My Drive\\work\\ciat\\cg-prioritization"
cb <- file.path(vdir, "vector/WB_Boundaries_GeoJSON_lowres/WB_countries_Admin0_lowres.geojson")
cb <- st_read(cb)
cb <- as_Spatial(cb)

# merge country names
cgregions <- merge(cgregions, cb[,c("NAME_EN", "ISO_A3")], by.x = "ISO3", by.y = "ISO_A3", all.x = TRUE)
cgregions$ISO3[is.na(cgregions$NAME_EN)]
cgregions$NAME_EN[cgregions$ISO3 == "ESH"] <- "Western Sahara"

#########################################################################################################################################
nue <- readxl::read_excel(file.path(datadir, "input/rue/41586_2015_BFnature15743_MOESM47_ESM.xlsx"), 
                          skip = 1, sheet = 1)
names(nue)[1] <- "country"
nue$country <- gsub("\\'", "", nue$country)
cc <- ccodes()
nue <- merge(cc[,c("NAME", "ISO3", "NAME_FAO")], nue, by.x = "NAME_FAO", by.y = "country", all.y = TRUE)

# fixing some of the names
nue$NAME_FAO[is.na(nue$ISO3)]
nue$NAME[grep("Bolivia",nue$NAME_FAO)] <- "Bolivia"
nue$NAME[grep("dIvoire",nue$NAME_FAO)] <- "Côte d'Ivoire"
nue$NAME[grep("^Congo$",nue$NAME_FAO)] <- "Republic of Congo"
nue$NAME[grep("Democratic Republic of the Congo", nue$NAME_FAO)] <- "Democratic Republic of the Congo"
nue$NAME[grep("Fiji",nue$NAME_FAO)] <- "Fiji"
nue$NAME[grep("Iran",nue$NAME_FAO)] <- "Iran"
nue$NAME[grep("Lao",nue$NAME_FAO)] <- "Laos"
nue$NAME[grep("Korea",nue$NAME_FAO)] <- "South Korea"
nue$NAME[grep("Sudan",nue$NAME_FAO)] <- "Sudan"
nue$NAME[grep("Tanzania",nue$NAME_FAO)] <- "Tanzania"
nue$NAME[grep("Venezuela",nue$NAME_FAO)] <- "Venezuela"

# merge again
nue <- merge(cc[,c("NAME", "ISO3")], nue, by.x = "NAME", by.y = "NAME", all.y = TRUE)
nue$NAME_FAO <- NULL
nue$ISO3.y <- NULL
names(nue)[2] <- "ISO3"

# save the results
write.csv(nue, file.path(datadir, "input/rue/41586_2015_BFnature15743_MOESM47_ESM_ISO3.xlsx"),
          row.names = FALSE)

# merge nue with CGIAR regions
nuec <- merge(cgregions, nue, by = "ISO3", all.x = TRUE)
nuec$NAME <- NULL

# last 5 and 10 years average
c5 <- nuec[,colnames(nuec) %in% tail(colnames(nuec), 5)]
c5 <- rowMeans(c5, na.rm = TRUE)
c5[is.nan(c5)] <- NA

# 10 years
c10 <- nuec[,colnames(nuec) %in% tail(colnames(nuec), 10)]
c10 <- rowMeans(c10, na.rm = TRUE)
c10[is.nan(c10)] <- NA

# final dataset
nuec <- data.frame(nuec[,c("ISO3","cgregion","NAME_EN")], nue_avg5yr = c5, nue_avg10yr = c10)
# replace missing values by group mean
impute.mean <- function(x){x <- as.numeric(x); replace(x, is.na(x), mean(x, na.rm = TRUE))}
nuec <- nuec %>% group_by(cgregion) %>% mutate(nue_avg5yr_imputed = impute.mean(nue_avg5yr),
                                        nue_avg10yr_imputed = impute.mean(nue_avg10yr))
write.csv(nuec, file.path(datadir,"outdir/rue/nue_cleaned_5_10years_mean.csv"), row.names = FALSE)


# cgeregion spatial files
cg <- cb[cb$ISO_A3 %in% cgregions$ISO3,]
nuesp <- merge(cg[,c("ISO_A3")], nuec, by.x = "ISO_A3", by.y = "ISO3", all.x = TRUE)

# rasterize
# reference raster
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
nuev <- vect(nuesp)
nuer <- lapply(c("nue_avg5yr_imputed","nue_avg10yr_imputed"), function(x) {rasterize(nuev, ref, x)})
nuer <- do.call(c, nuer)


##########################################################################################################################
# poverty
pv <- read.csv(file.path(datadir, "input/poverty/wb_pov.csv"), stringsAsFactors = FALSE)
pv <- pv[,c("iso_a3","avg5yr","avg10yr")]
names(pv)[2:3] <- paste0("poverty_", names(pv)[2:3])
pv[pv[] == "."] <- NA

# merge with cgregions dataframe
pv <- merge(cgregions, pv, by.x = "ISO3", by.y = "iso_a3", all.x = TRUE)
# impute missing values
impute.mean <- function(x){x <- as.numeric(x); replace(x, is.na(x), mean(x, na.rm = TRUE))}
pvc <- pv %>% group_by(cgregion) %>% mutate(poverty_avg5yr_imputed = impute.mean(poverty_avg5yr),
                                             poverty_avg10yr_imputed = impute.mean(poverty_avg10yr))

# cgeregion spatial files
cg <- cb[cb$ISO_A3 %in% cgregions$ISO3,]
povsp <- merge(cg[,c("ISO_A3")], pvc, by.x = "ISO_A3", by.y = "ISO3", all.x = TRUE)

# rasterize
# reference raster
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
povv <- vect(povsp)
povr <- lapply(c("ISO_A3","cgregion","NAME_EN","poverty_avg5yr_imputed","poverty_avg10yr_imputed"), function(x) {rasterize(povv, ref, x)})
povr <- do.call(c, povr)

# all raster
rr <- c(povr, nuer)
writeRaster(rr, file.path(datadir, "outdir/all_raster/cgregion_poverty_nue.tif"), 
            gdal=c("COMPRESS=LZW", "TFW=YES","of=COG"), overwrite = TRUE)

############################################################################################################
# trend function
getSlope <- function(d){
  x <- 1:ncol(d) - 1
  d[is.na(d)] <- 0
  fit <- summary(lm(t(d) ~ x))
  n <- names(fit)
  ss <- sapply(n, function(z)coefficients(fit[z])[[1]][2,1])
  names(ss) <- NULL
  ss <- round(ss, 3)
  return(ss)
}
dnue <- nue[, 2:ncol(nue)]
ns <- getSlope(dnue)

# clean country names
clist <- nue[,1, drop = TRUE]
clist <- gsub("\\'", "", clist)
ns <- data.frame(country = clist, nue_trend = ns, stringsAsFactors = FALSE)
write.csv(ns, file.path(datadir,"outdir/rue/nue_cleaned_trend.csv"), row.names = FALSE)
