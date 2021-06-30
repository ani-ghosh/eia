# resource use efficiency trend
library(raster)
library(terra)
library(dplyr)
library(sf)

# working directory
datadir <- "G:\\My Drive\\work\\ciat\\eia\\analysis"

# vector boundaries
vdir <- "G:\\My Drive\\work\\ciat\\cg-prioritization"
cb <- st_read(file.path(vdir, "vector/WB_Boundaries_GeoJSON_lowres/WB_countries_Admin0_lowres.geojson"))
cb <- as_Spatial(cb)

# CGIAR region countries
cgregions <- shapefile(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))

# merge country names
cgregions <- merge(cgregions, cb[,c("NAME_EN", "ISO_A3")], by.x = "ISO3", by.y = "ISO_A3", all.x = TRUE)
cgregions$ISO3[is.na(cgregions$NAME_EN)]
cgregions$NAME_EN[cgregions$ISO3 == "ESH"] <- "Western Sahara"

#########################################################################################################################################
nue <- readxl::read_excel(file.path(datadir, "input/rue/41586_2015_BFnature15743_MOESM47_ESM.xlsx"), 
                          skip = 1, sheet = 1)
names(nue)[1] <- "country"
nue$country <- gsub("\\'", "", nue$country)
cc <- raster::ccodes()
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
nuec <- merge(cgregions, nue, by.x = "ISO_A3", by.y = "ISO3", all.x = TRUE)
nuec$NAME <- NULL

# last 5 and 10 years average
c5 <- nuec[, names(nuec) %in% tail(names(nuec), 5)]
c5 <- rowMeans(c5, na.rm = TRUE)
c5[is.nan(c5)] <- NA

# 10 years
c10 <- nuec[,names(nuec) %in% tail(names(nuec), 10)]
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
povr <- lapply(c("poverty_avg5yr_imputed","poverty_avg10yr_imputed"), function(x) {rasterize(povv, ref, x)})
povr <- do.call(c, povr)

# all raster
rr <- c(povr, nuer)
writeRaster(rr, file.path(datadir, "outdir/all_raster/poverty_nue.tif"), 
            gdal=c("COMPRESS=LZW", "TFW=YES"), overwrite = TRUE, datatype="FLT4S")

############################################################################################################
pv <- read.csv(file.path(datadir, "input/poverty/wb_pov.csv"), stringsAsFactors = FALSE)
eba <- readxl::read_excel(file.path(datadir, "input/worldbank/ease_of_doing_business/eba_country_aggregate_scores_2019.xlsx"),
                          sheet = 2, skip = 1)

eba$Economy[grep("Congo, Dem. Rep.", eba$Economy)] <- "Democratic Republic of the Congo"
eba$Economy[grep("Egypt, Arab Rep.", eba$Economy)] <- "Egypt"
eba$Economy[grep("Korea, Rep.", eba$Economy)] <- "South Korea"
eba$Economy[grep("Kyrgyz Republic", eba$Economy)] <- "Kyrgyzstan"
eba$Economy[grep("Lao PDR", eba$Economy)] <- "Laos"
eba$Economy[grep("Russian Federation", eba$Economy)] <- "Russia"
eba$Economy[grep("Slovak Republic", eba$Economy)] <- "Slovakia"

# get ISO3 code from other wb dataset
cc <- raster::ccodes()
eba <- merge(cc[,c("NAME", "ISO3")], eba, by.x = "NAME", by.y = "Economy")
eba <- eba[,c("NAME","ISO3", "EBA Topic Score")]

# merge with cgregions
cgr <- cgregions@data[,c("ISO_A3", "cgregin")]
cgr <- cgr[!duplicated(cgr),]
ebac <- merge(cgr, eba, by.x = "ISO_A3", by.y = "ISO3", all.x = TRUE)
names(ebac) <- c("ISO_A3","cgregion","NAME","eba_score")

# replace missing values by group mean
impute.mean <- function(x){x <- as.numeric(x); replace(x, is.na(x), mean(x, na.rm = TRUE))}
ebac <- ebac %>% group_by(cgregion) %>% mutate(eba_score_imputed = impute.mean(eba_score))
write.csv(ebac, file.path(datadir,"outdir/worldbank/eba_cleaned.csv"), row.names = FALSE)

# cgeregion spatial files
ebasp <- merge(cgregions, ebac, all.x = TRUE)
shapefile(ebasp, file.path(datadir,"outdir/worldbank/eba_cleaned_country_farming_system_cg_regions.shp"))

# rasterize
# reference raster
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
ebav <- vect(ebasp)
ebar <- rasterize(ebav, ref, "eba_score_imputed")



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


