# compute summary stat
library(raster)

getAreaStat <- function(i, vs, r){
  vi <- vs[i,]
  r <- crop(r, vi)
  r <- mask(r, vi)
  r <- r[[1]]
  ta <- tapply(area(r), r[], sum)
  ta <- data.frame(country = vi$ADM0_NAME, t(round(ta*100/sum(ta))))
  return(ta)
}

# country boundaries
dir0 <- "G:\\My Drive\\work\\ciat\\eia"
v <- shapefile(file.path(dir0, "general\\africa_boundary\\African_continet.shp"))
tcountires <- c("Kenya","Nigeria", "Senegal", "South Africa")
vsub <- v[v$ADM0_NAME %in% tcountires, ]

# list layers for which we want to calculate statistics
dir1 <- "G:\\.shortcut-targets-by-id\\1Oqw1G8VnbarTTzdZb_aGFcqv_vfxxx6O\\Synthesis report"
ff <- list.files(dir1, pattern = ".tif", full.names = TRUE, recursive = TRUE)

tvar <- c("Precipitation Variability1", "hazard_heat", "hazard_drou")
ff <- grep(paste0(tvar, collapse = "|"), ff, value = TRUE)


# additional layers will come from climate layers
for (i in 1:length(ff)){
  r <- stack(ff[i])
  crs(r) <- crs(vsub)
  d <- lapply(1:nrow(vsub), getAreaStat, vsub, r)
  d <- data.table::rbindlist(d, fill=TRUE)
  ofile <- file.path(dir0, "outdir", gsub(".tif",".csv",basename(ff[i])))
  write.csv(d, ofile, row.names = FALSE)
}

