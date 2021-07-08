library(raster)
library(terra)
library(jsonlite)
library(sf)
library(tidyverse)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"


####################################################################################################################################
# undernourishment
cleanScores <- function(f, cb){
  
  cat("processing", basename(f), "\n")
  
  u <- st_read(f, quiet = TRUE)
  
  if(sum(is.na(u$centroid)) == nrow(u)) return(NULL)
  
  fcs <- lapply(u$fcs, function(x){
    if(is.na(x)){
      y <- c(NA, NA)
    }else{
      y <- data.frame(fromJSON(x))
      if(nrow(y)==0){y <- c(NA, NA)} else {y <- y[,c("score", "people")]}
      } 
    names(y) <- paste0("fcs_", c("score", "people"))
    y})
  
  fcs <- dplyr::bind_rows(fcs)
  
  rcsi <- lapply(u$rcsi, function(x){
    if(is.na(x)){
      y <- c(NA, NA)
    }else{
      y <- data.frame(fromJSON(x))
      if(nrow(y)==0) {y <- c(NA, NA)} else {y <- y[,c("ratio", "people")]}
    } 
    names(y) <- paste0("rcsi_", c("ratio", "people"))
    y})
  
  rcsi <- dplyr::bind_rows(rcsi)
  
  cc <- lapply(u$centroid, function(x){
    if(is.na(x)){
      y<- c(NA, NA)
      } else {
        y <- data.frame(fromJSON(x))
      }
    names(y) <- c("latitude", "longitude")
    return(y)
    })
  cc <- dplyr::bind_rows(cc)
  
  vv <- u[,c("id", "Code", "Name")] 
  vv <- cbind(vv, fcs, rcsi, cc)
  vv$fcs_score <- round(vv$fcs_score*100,2)
  vv$rcsi_ratio <- round(vv$rcsi_ratio*100,2)
  
  xy <- st_as_sf(cc[complete.cases(cc),], coords = c('longitude', 'latitude'), crs = 4326)
  adm0 <- cb[as_Spatial(xy),]
  vv$iso3 <- adm0$ISO_A3
  return(vv)
}

uu <- list.files(file.path(datadir, "input/wfp/hungermap/adm1data"), pattern = ".json", full.names = TRUE)
# 276 is strange; a straight line
uu <- grep(276, uu, val = T, invert = T)

# simplified country vector boundaries
vdir <- "G:/My Drive/work/ciat/cg-prioritization"
cb <- file.path(vdir, "vector/WB_Boundaries_GeoJSON_lowres/WB_countries_Admin0_lowres.geojson")
cb <- st_read(cb)
# library(s2)
# cb_s2 <- as_s2_geography(cb, check = FALSE)
# cbv <- cb_s2 %>% 
#   s2_rebuild(s2_options(split_crossing_edges = TRUE, edge_type = "undirected")) %>%
#   s2_union()
# 
cb <- as_Spatial(cb)

# f <- grep("_101", uu, val = T)
ss <- lapply(uu, cleanScores, cb)
ss[sapply(ss, is.null)] <- NULL
ssu <- do.call(rbind, ss)
st_write(ssu, file.path(datadir, "outdir/wfp/fcs_rcsi.geojson"))

ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
names(ref) <- "elevation"

ssuv <- vect(ssu)
ssr <- lapply(c("fcs_score","fcs_people","rcsi_ratio","rcsi_people"), 
              function(x) {rasterize(ssuv, ref, x, fun = mean, na.rm = TRUE)})
ssr <- do.call(c, ssr)
writeRaster(ssr, file.path(datadir, "outdir/wfp/fcs_rcsi.tif"), 
            gdal=c("COMPRESS=LZW"), overwrite = TRUE)


