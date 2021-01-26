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

# for each farming system, remove small polygons (merge remaining to create a single polygon?)
removePolygon <- function(gc, viso){
  vs <- viso[viso$GIS_CODE1 == gc, ]
  a <- area(vs)*100/sum(area(vs))
  k <- which(a>1) # at least 1 %
  return(vs[k,])
}

# also save the files in a geojson/shapefile for making EarthEngine and other extraction easier
getFSISOboundary <- function(iso, fs){
  cat("Processing ", iso, "\n")
  ofile <- paste0("input/boundary/country/", iso, "_farming_system.rds")
  v <- getData("GADM", country=iso, level=0, path="input/boundary/country")
  
  if(!file.exists(ofile)){
    fsv <- crop(fs, v)
    saveRDS(fsv, ofile)
  } else {
    fsv <- readRDS(ofile)
  }
  
  if (!is.null(fsv)){
    fsc <- unique(fsv$GIS_CODE1)
    if(length(fsc) > 0){
      fsvi <- lapply(fsc, removePolygon, fsv)
      fsvi <- do.call(bind, fsvi)
      a <- round(area(fsvi)/(1000^2), 2)
      fsvi@data <- data.frame(country=v$NAME_0, iso3 = iso, uid = paste0(iso,"_", 1:nrow(fsvi)),
                              area_sqkm = a, area_pct = round(a/sum(a), 2)*100, 
                              fsvi@data[, c("CODE1","FARMSYS_VE","DESCRIPTIO","NEWCODE","DIXON_C","GIS_CODE1")])
      return(fsvi)
    }
  }
}

# remove china for now, takes too much time to process?
# isol <- lmic$CountryCode[lmic$CountryCode!="CHN"]
vv <- lapply(lmic$CountryCode, 
             function(iso, fs)tryCatch(getFSISOboundary(iso, fs), error=function(e) NULL), fs) 

# remove NULL returns
vv[sapply(vv, is.null)] <- NULL
vvf <- do.call(bind, vv)

# investments in <1000sqkm probably doesn't matter
vvf <- vvf[vvf$area_sqkm >= 1000, ]

shapefile(vvf, "input/boundary/country/country_farming_system.shp", overwrite = TRUE)




# earth engine country list from LSIB
# var dataset = ee.FeatureCollection('USDOS/LSIB_SIMPLE/2017');
# print(dataset.aggregate_array("country_na").sort())

vee <- c("Abyei Area","Afghanistan","Akrotiri","Aksai Chin","Albania","Algeria","American Samoa","Andorra",
         "Angola","Anguilla","Antarctica","Antarctica","Antarctica","Antarctica","Antigua & Barbuda",
         "Argentina","Armenia","Aruba","Ashmore & Cartier Is","Australia","Austria","Azerbaijan",
         "Bahamas, The","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bermuda",
         "Bhutan","Bir Tawil","Bolivia","Bosnia & Herzegovina","Botswana","Bouvet Island","Brazil",
         "British Indian Ocean Terr","British Virgin Is","Brunei","Bulgaria","Burkina Faso","Burma",
         "Burundi","Cabo Verde","Cambodia","Cameroon","Canada","Canada","Canada","Canada","Canada",
         "Canada","Canada","Canada","Cayman Is","Central African Rep","Chad","Chile","China","China",
         "China","China","Christmas I","Clipperton Island","Cocos (Keeling) Is","Colombia","Comoros",
         "Cook Is","Coral Sea Is","Costa Rica","Cote d'Ivoire","Croatia","Cuba","Cuba","Curacao",
         "Cyprus","Czechia","Dem Rep of the Congo","Demchok Area","Denmark","Dhekelia","Djibouti",
         "Dominica","Dominican Republic","Dragonja River Mouth","Dramana-Shakatoe Area","Ecuador",
         "Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Falkland Islands",
         "Faroe Is","Fed States of Micronesia","Fiji","Finland","France","French Guiana","French Polynesia",
         "French S & Antarctic Lands","Gabon","Gambia, The","Gaza Strip","Georgia","Germany","Ghana",
         "Gibraltar","Greece","Greenland","Grenada","Guadeloupe","Guam","Guatemala","Guernsey","Guinea",
         "Guinea-Bissau","Guyana","Haiti","Halaib Triangle","Heard I & McDonald Is","Honduras",
         "Hong Kong","Hungary","IN-CH Small Disputed Areas","Iceland","India","Indonesia",
         "Invernada Area","Iran","Iraq","Ireland","Isla Brasilera","Isle of Man","Israel","Israel",
         "Italy","Jamaica","Jan Mayen","Japan","Jersey","Jordan","Kalapani Area","Kazakhstan",
         "Kenya","Kiribati","Korea, North","Korea, South","Korean Is. (UN Jurisdiction)","Kosovo",
         "Koualou Area","Kuwait","Kyrgyzstan","Laos","Latvia","Lebanon","Lesotho","Liancourt Rocks",
         "Liberia","Libya","Liechtenstein","Lithuania","Luxembourg","Macau","Macedonia","Madagascar",
         "Malawi","Malaysia","Maldives","Mali","Malta","Marshall Is","Martinique","Mauritania","Mauritius",
         "Mayotte","Mexico","Moldova","Monaco","Mongolia","Montenegro","Montserrat","Morocco","Mozambique",
         "Namibia","Nauru","Navassa I","Nepal","Netherlands","Netherlands (Caribbean)","New Caledonia",
         "New Zealand","Nicaragua","Niger","Nigeria","Niue","No Man's Land","Norfolk I",
         "Northern Mariana Is","Norway","Oman","Pakistan","Palau","Panama","Papua New Guinea",
         "Paracel Is","Paraguay","Peru","Philippines","Pitcairn Is","Poland","Portugal",
         "Portugal (Azores)","Portugal (Madeira Is)","Puerto Rico","Qatar","Rep of the Congo",
         "Reunion","Romania","Russia","Russia","Russia","Russia","Russia","Russia","Rwanda",
         "S Georgia & S Sandwich Is","Saint Lucia","Samoa","San Marino","Sao Tome & Principe",
         "Saudi Arabia","Senegal","Senkakus","Serbia","Seychelles","Siachen-Saltoro Area",
         "Sierra Leone","Sinafir & Tiran Is.","Singapore","Sint Maarten","Slovakia","Slovenia",
         "Solomon Is","Somalia","South Africa","South Sudan","Spain","Spain (Africa)",
         "Spain (Canary Is)","Spratly Is","Sri Lanka","St Barthelemy","St Helena","St Kitts & Nevis",
         "St Martin","St Pierre & Miquelon","St Vincent & the Grenadines","Sudan","Suriname","Svalbard",
         "Swaziland","Sweden","Switzerland","Syria","Taiwan","Tajikistan","Tanzania","Thailand",
         "Timor-Leste","Togo","Tokelau","Tonga","Trinidad & Tobago",
         "Tunisia","Turkey","Turkmenistan","Turks & Caicos Is","Tuvalu","US Minor Pacific Is. Refuges",
         "US Virgin Is","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States",
         "United States","United States","United States","United States (Alaska)","United States (Hawaii)",
         "Uruguay","Uzbekistan","Vanuatu","Vatican City","Venezuela","Vietnam",
         "Wake I","Wallis & Futuna","West Bank","West Bank","Western Sahara","Yemen","Zambia","Zimbabwe")

# overlap with lmic names
lmic$CountryName[lmic$CountryName %in% vee]

vee <- data.frame(CountryName=vee, lsib_countryname = vee)
dd <- merge(lmic, vee, by = "CountryName", all.x = TRUE)
# fix the mismatch in name manually
write.csv(dd, "input/boundary/lsib_lmic_common_names_gee.csv", row.names = FALSE)

