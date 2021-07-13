library(readxl)
library(sf)
library(raster)
datadir <- "G:/My Drive/work/ciat/eia/analysis"

xl <- "G:/My Drive/work/ciat/eia/analysis/output/Level123_cgregion_country_farming_system_2021-07-11 15-08 - BV.xlsx"
d <- read_excel(xl, sheet = 1)

d$id <- paste0(d$farming_system, "_", d$ISO_A3)
# ds <- d[,c(2,3)]
# ds <- ds[complete.cases(ds),]
# colnames(ds) <- ds[1,]
# ds <- ds[-1,]
# ds$id <- paste0(ds$Country, "_", ds$System)

vs <- shapefile(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))
vs$frmng_s <- gsub("[[:digit:]]+","", vs$frmng_s)
vs$frmng_s <- gsub("\\.","", vs$frmng_s)
vs$frmng_s <- trimws(vs$frmng_s)
vs$cgregin[vs$cgregin == "SAE"] <- "SEA"
vs$id <- paste0(vs$frmng_s, "_", vs$ISO_A3)

vds <- vs[vs$id %in% d$id,]
vds <- merge(vds, d, by = "id")

vdir <- "G:\\My Drive\\work\\ciat\\cg-prioritization"
cb <- file.path(vdir, "vector/WB_Boundaries_GeoJSON_lowres/WB_countries_Admin0_lowres.geojson")
cb <- st_read(cb)
cb <- as_Spatial(cb)
cbs <- cb[cb$ISO_A3 %in% vs$ISO_A3,]

cgr <- unique(vs$cgregin)

# individual extent
rgx <- lapply(cgr, function(x){
  p <- vds[vds$cgregin == x, ]
  z <- raster(p, nrow=1, ncol=1, vals=1)
  names(z) <- "zone"
  # coerce RasterLayer to SpatialPolygonsDataFrame
  z <- as(z, 'SpatialPolygonsDataFrame')
  z$zone <- x
  z
})
rgx <- do.call(rbind, rgx)


for (cg in cgr){
  ofile <- file.path(datadir, "output", paste0(cg, "_selected_geometries_v4.png"))
  
  rg <- rgx[rgx$zone == cg, ]
  
  png(ofile, height = 10, width = 20, units = "in", res = 300)
  par(mfrow=c(1,2))
  plot(rg, border = "#ffffff", main = "level 1", cex.main = 3)
  plot(vs[vs$cgregin == cg,], border = "#f0f0f0", col = "#f0f0f0", lwd = 0.5, add = T)
  plot(cbs, col = NA, lwd = 0.5, add = T)
  vds1 <- vds[vds$cgregin == cg,]
  cols <- c("#33a02c", "#fdbf6f")[vds1$rank]
  plot(vds1, col = cols, lwd = 0.1,  add = T)
  
  legend("bottomleft",
         legend = paste(cg, "region"),
         fill = NA,
         border = NA,
         bty = "n", # turn off the legend border
         cex = 3)
  
  plot(rg, border = "#ffffff", main = "level 1 & level 3", cex.main = 3)
  plot(vs[vs$cgregin == cg,], border = "#f0f0f0", col = "#f0f0f0", lwd = 0.5, add = T)
  plot(cbs, col = NA, lwd = 0.5, add = T)
  vds2 <- vds[vds$cgregin == cg & vds$rank == 1,]
  plot(vds2, col = "#33a02c", lwd = 0.1,  add = T)
  
  dev.off()
  
}

# legend("topleft",
#        legend = c("level 1 & level 3", "level 1"),
#        fill = c("#33a02c", "#fdbf6f"),
#        bty = "n", # turn off the legend border
#        cex = 2.5)


