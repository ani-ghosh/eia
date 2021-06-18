# https://gaez.fao.org/pages/data-viewer
# Theme 1: Land and Water Resources
# Theme 2: Agro-climatic Resources
# Theme 3: Agro-climatic Potential Yield
# Theme 4: Suitability and Attainable Yield
# Theme 5: Actual Yields and Production
# Theme 6: Yield and Production Gaps

library(httr)
library(jsonlite)

getMetaData <- function(i, params, datadir){
  param <- params[i,]
  gapi <- "https://gaez-services.fao.org/server/rest/services"
  # dummy request body
  # qb <- "/ImageServer/identify?f=json&geometry=%7B%22x%22%3A1321728.3185412139%2C%22y%22%3A2811987.9457971323%2C%22spatialReference%22%3A%7B%22wkid%22%3A102100%7D%7D&returnGeometry=false&returnCatalogItems=true&mosaicRule=%7B%22mosaicMethod%22%3A%22esriMosaicNorthwest%22%2C%22where%22%3A%22((1%3D1))%22%2C%22sortField%22%3A%22%22%2C%22ascending%22%3Atrue%2C%22mosaicOperation%22%3A%22MT_FIRST%22%7D&geometryType=esriGeometryPoint&pixelSize=%7B%22x%22%3A39135.75848200009%2C%22y%22%3A39135.75848200009%2C%22spatialReference%22%3A%7B%22wkid%22%3A102100%7D%7D&renderingRules=%5B%7B%22rasterFunction%22%3A%22AEZ%20Classification%20(33%20Classes)%22%7D%5D&returnPixelValues=false&maxItemCount="
  # pagination doesn't work, so making a template and create for crops
  qb <- "ImageServer/query?f=json&where=((1%3D1))&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=objectid%20ASC&"
  cc <- 1000
  ofs <- seq(0, param$maxlimits, cc)
  # ofs <- seq(0, 2000, cc)
  ddf <- list()
  for(k in 1:length(ofs)){
    of <- ofs[k]
    qp <- paste0("resultOffset=",of,"&","resultRecordCount=",cc)
    print(qp)
    u <- file.path(gapi, param$imageserver, paste0(qb, qp))
    q <- GET(u)
    
    dd <- jsonlite::fromJSON(content(q, "text"))
    ddf[[k]] <- data.frame(dd$features$attributes)
  }
  ddf <- data.table::rbindlist(ddf, fill = T)
  ddf <- ddf[!duplicated(ddf),]
  
  write.csv(ddf, file.path(datadir, paste0("metadata_", param$imageserver, ".csv")), row.names = FALSE)
}



filedownload <- function(i, d, datadir){
  k <- d[i,]
  toremove <- "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/"
  u <- k$download_url
  u <- trimws(u)
  fp <- gsub(toremove, "", u)
  dfile <- file.path(datadir, "data", fp)
  dir.create(dirname(dfile), F, T)
  if(!file.exists(dfile)){
    tryCatch(download.file(u, dfile, mode = "wb"), error = function(e) NULL)
  }
  # tryCatch(download.file(u, dfile, mode = "wb"), error = function(e) NULL)
}

datadir <- "work/gaez4"
dir.create(datadir, F, T)

params <- data.frame(imageserver = c("LR","res01", "res02", "res05", "res06", "res07"), 
                     maxlimits = c(1000, 112000, 112000, 123000, 1000, 1000))

lapply(1:nrow(params), getMetaData, params, datadir)

ff <- list.files(datadir, pattern = ".csv$", full.names = T)

for (f in ff){
  d <- read.csv(f)
  lapply(1:nrow(d), filedownload, d, datadir)
}

# gsutil -m cp -r /home/anighosh/work/gaez4 gs://gaez4_global

for (f in ff){
  d <- read.csv(f)
  print(nrow(d))
}


# function to download filtered dataset
datadir <- "G:/My Drive/work/ciat/adaptation_atlas/GAEZ4/metadata"
ff <- list.files(datadir, pattern = ".csv$", full.names = TRUE)

#343871

# tar -cvf gaez4.tar gaez
# gzip -c9 gaez4.tar > gaez4.gz

getGAEZdata <- functtion(theme, sub-theme, var, year, model, rcp, ...){}

library(terra)
d <- dd[[1]]
r <- rast(trimws(d$download_url)) 


library(readxl)
datadir <- "G:/My Drive/work/ciat/adaptation_atlas/GAEZ4"

varops <- read_excel(file.path(datadir, "GAEZ4_DB_Variables_Symbology_Crops.xlsx"), 1)
crops <- read_excel(file.path(datadir, "GAEZ4_DB_Variables_Symbology_Crops.xlsx"), 2)


# Theme 1: Land and Water Resources
baseurl <- "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org"
imgserver <- "LR"
vars <- varops$`Variable acronym`[varops$`Theme acronym`== "LR"]
suffix <- "_CRUTS32_Hist_8110.tif"

# Theme 3: Agro-climatic Potential Yield
imgs <- "res02"
# yield -> yld 
# constraining factors: temp, moisture, "agro-climatic", "total climate" 
vars <- c("yld", "fc1", "fc2", "fc0", "cbd", "Tsc") 
cds <- c("CRUTS32","GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC-ESM-CHEM", "NorESM1-M")
periodhist <- c("6190H","7100H","8110H")
periodfut <-  c("2020sH","2050sH","2080sH")
rcp <- c("Hist",paste0("RCP", c("2.6", "4.5", "6.0", "8.5")))
water <- c("a", "b") # irrigation/no_irrigation


# 
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/IPSL-CM5A-LR/rcp6p0/2020sH/teas200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/GFDL-ESM2M/rcp4p5/2050sH/bana200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/GFDL-ESM2M/rcp4p5/2050sH/bana200b_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/GFDL-ESM2M/rcp2p6/2020sH/alfa200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/GFDL-ESM2M/rcp2p6/2080sH/alfa200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/6190H/alfa200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/7100H/alfa200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_yld.tif
# 
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_fc1.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_fc2.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_fc0.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_cbd.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_Tsc.tif