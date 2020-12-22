# standardize data
library(terra)

# align to standard raster function
alignRaster <- function(r, ref, v){
  r <- crop(r, ref)
  r <- resample(r, ref)
  r <- mask(r, ref)
  r <- mask(r, vect(v))
}

u <- "https://malariaatlas.org/geoserver/ows?service=CSW&version=2.0.1&request=DirectDownload&ResourceId=Explorer:2015_accessibility_to_cities_v1.0"
download.file(u, "2015_accessibility_to_cities_v1.0.zip")
unzip("2015_accessibility_to_cities_v1.0", exdir=".")
acr <- rast(".../2015_accessibility_to_cities_v1.0.tif")