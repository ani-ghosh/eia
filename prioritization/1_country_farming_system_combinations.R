# create country-farming system combination
library(raster)
library(readxl)

# country boundaries
dir <- "G:\\My Drive\\work\\ciat\\eia\\analysis"
setwd(dir)

# LMIC country and codes
cc <- read_excel("input/general/CLASS_worldbank.xls", sheet = "Groups")
lmic <- cc[cc$GroupName == "Low & middle income",]

# Kosovo ISO3 in GADM and Worldbank are different
lmic$CountryCode[lmic$CountryName=="Kosovo"] <- "XKO"
# remove russia
lmic <- lmic[lmic$CountryCode!="RUS",]

# download all these country boundary GADM 0 level
lapply(lmic$CountryCode,
       function(iso){getData("GADM", country=iso, level=0, path="input/boundary/country");return(iso)})

# farming system boundary
fs <- shapefile("input/general/dixon_farming_system/FarmingSystem_Global.shp")

# area/overlap of each country with farming systems; fractional area of each systems 
getFSISOcom <- function(iso, fs){
  cat("Processing ", iso, "\n")
  v <- getData("GADM", country=iso, level=0, path="input/boundary/country")
  fsv <- crop(fs, v)
  if (!is.null(fsv)){
    a <- area(fsv)/(1000*1000)
    d <- data.frame(country=v$NAME_0, farming_system=fsv$DESCRIPTIO, gis_code = fsv$GIS_CODE1,
                    area_sqkm = a, area_pct = round(a/sum(a), 2)*100)
    return(d)
  }
}

dd <- lapply(lmic$CountryCode, getFSISOcom, fs)
ddf <- do.call(rbind, dd)

# merge country code back
ddf <- merge(lmic[,c("CountryCode", "CountryName")], ddf, 
             by.x = "CountryName", by.y = "country", all.y=TRUE)

# remove small systems
ddf <- ddf[ddf$area_pct >= 5, ]
write.csv(ddf, "outdir/country_farming_system_KPI_template.csv", row.names = FALSE)
