# Create conflict gridcells
datadir <- "G:/My Drive/work/ciat/eia/analysis"

library(terra)
library(raster)
library(sf)
library(readxl)
library(tidyverse)


# reference raster
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
names(ref) <- "elevation"

# list of conflict dataset
ff <- list.files(file.path(datadir, "input/conflict/ACLED"), pattern = ".xlsx", full.names = TRUE)
ff <- grep("Africa|CCA|East-Asia|LatinAmerica|MiddleEast|Asia", ff, value = TRUE)

summaryFun <- function(f){
  cat("Processing", f, "\n")
  d <- read_excel(f) %>% 
    filter(YEAR > 2015) %>%
    select(LATITUDE,LONGITUDE,FATALITIES) %>%
    group_by (LATITUDE,LONGITUDE) %>%
    mutate(total_conflict = n(), total_death = sum(FATALITIES, na.rm = TRUE)) %>%
    select(-FATALITIES) %>%
    mutate(severity = round(total_death/total_conflict, 2)) %>%
    ungroup() %>%
    distinct()
  return(d)
}

dd <- lapply(ff, summaryFun)
ddf <- bind_rows(dd) %>% distinct()
names(ddf)[1] <- "lat"
names(ddf)[2] <- "lon"

pp <- vect(ddf, crs="+proj=longlat +datum=WGS84")

rr <- lapply(c("total_conflict","total_death","severity"), function(x){rasterize(pp, ref, x, fun = mean)})
rrs <- do.call(c, rr)
rrs <- round(rrs)
names(rrs) <- c("total_conflict","total_death","severity")
writeRaster(rrs, file.path(datadir, "outdir/all_raster/lmic_conflicts_2016-21.tif"), 
            overwrite=TRUE, gdal=c("COMPRESS=LZW"))

