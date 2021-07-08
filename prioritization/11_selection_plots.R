library(readxl)
library(sf)
library(raster)
datadir <- "G:/My Drive/work/ciat/eia/analysis"

xl <- "G:/My Drive/work/ciat/eia/analysis/output/level123_cgregion_country_farming_system_2021-06-30 18-15 - BV.xlsx"
d <- read_excel(xl, sheet = 2)
head(d)
# ds <- d[,c(2,3)]
# ds <- ds[complete.cases(ds),]
# colnames(ds) <- ds[1,]
# ds <- ds[-1,]
# ds$id <- paste0(ds$Country, "_", ds$System)

vs <- shapefile(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))
vs$frmng_s <- gsub("[[:digit:]]+","", vs$frmng_s)
vs$frmng_s <- gsub("\\.","", vs$frmng_s)
vs$frmng_s <- trimws(vs$frmng_s)

vs$id <- paste0(vs$frmng_s, "_", vs$ISO_A3)

vds <- vs[vs$id %in% d$id,]

vdir <- "G:\\My Drive\\work\\ciat\\cg-prioritization"
cb <- file.path(vdir, "vector/WB_Boundaries_GeoJSON_lowres/WB_countries_Admin0_lowres.geojson")
cb <- st_read(cb)
cb <- as_Spatial(cb)
cbs <- cb[vs,]

png(file.path(datadir, "output/selected_geometries_v2.png"), height = 10, width = 15, units = "in", res = 300)
plot(vs, border = "#f0f0f0", col = "#f0f0f0")
plot(cb, add = T)
plot(vds, border = "#1d91c0", col = "#1d91c0", add = T)
dev.off()

png(file.path(datadir, "output/selected_geometries_v3.png"), height = 10, width = 15, units = "in", res = 300)
plot(vs, border = "#f0f0f0", col = "#f0f0f0")
plot(cbs, add = T)
plot(vds, border = "#1d91c0", col = "#1d91c0", add = T)
dev.off()

ds$id[!ds$id %in% vs$id]
