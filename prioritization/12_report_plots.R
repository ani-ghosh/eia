# install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
library(sf)
library(tmap)
library(tmaptools)


# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

# low resolution country boundaries
vdir <- "G:/My Drive/work/ciat/cg-prioritization"


# this after combining Central American Dry Corridor with Dixon maps
cgv <- shapefile(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))
# bit of cleaning

cgv$FORMAL_ <- NULL
cgv$ECONOMY <- NULL
cgv$INCOME_ <- NULL
cgv$ar_sqkm <- NULL
names(cgv)[names(cgv) == "frmng_s"] <- "farming_system"
names(cgv)[names(cgv) == "cgregin"] <- "cgregion"

cgv$farming_system <- gsub("[[:digit:]]+","", cgv$farming_system)
cgv$farming_system <- gsub("\\.","", cgv$farming_system)
cgv$farming_system <- trimws(cgv$farming_system)

# target countries
tiso <- c('AFG','AGO','ARM','AZE','BDI','BEN','BFA','BGD','BLZ','BOL','BTN','BWA','CAF','CIV','CMR','COD','COG','COL','CRI','CUB',
          'DMA','DOM','DZA','ECU','EGY','ERI','ETH','GAB','GHA','GIN','GMB','GNB','GRD','GTM','GUY','HND','HTI','IDN','IND','IRN','IRQ',
          'JAM','KAZ','KEN','KGZ','KHM','LAO','LBN','LBR','LBY','LKA','LSO','MAR','MDG','MEX','MLI','MMR','MOZ','MRT','MWI',
          'NAM','NER','NGA','NIC','NPL','PAK','PAN','PER','PHL','PNG','PRY','PSE','RWA','SDN','SEN','SLB','SLE','SLV','SOM','SSD','SUR','SWZ','SYR',
          'TCD','TGO','THA','TJK','TKM','TLS','TUN','TZA','UGA','UZB','VCT','VEN','VNM','YEM','ZMB','ZWE')

cgv <- cgv[cgv$ISO_A3 %in% tiso,]
cgv$UID <- paste0(cgv$farming_system, "_", cgv$ISO_A3)

# exclude farming systems
rfc <- readxl::read_excel("data/irrelevant_country_farming_systems.xlsx", sheet = 1)
rfc <- rfc[!rfc$farming_system %in% c("Agro-pastoral", "Pastoral"),]
cgv <- cgv[!cgv$UID %in% rfc$UID,]
cgv <- cgv[!is.na(cgv$farming_system),]
cgv$UID <- NULL
cgv$cgregion[cgv$cgregion == "SAE"] <- "SEA"

# low resolution country boundaries
# cb <- file.path(vdir, "vector/WB_Boundaries_GeoJSON_lowres/WB_countries_Admin0_lowres.geojson")
# cb <- st_read(cb)
# cb <- st_make_valid(cb)
# cb <- as_Spatial(cb)
data("World")
cb <- World[World$iso_a3 != "ATA", ]
cbs <- cb[cb$iso_a3 %in% tiso,]


ofile <- file.path(datadir, "output", "figure1_initial_geometries.png")

png(ofile, height = 10, width = 20, units = "in", res = 300)

cgv$cols <- as.factor(cgv$cgregion)
legend.cols <- levels(as.factor(cgv$cgregion))

plot(cb, border = "#bdbdbd", col = "#f0f0f0", lwd = 0.5)
plot(cbs, col = NA, lwd = 0.5, add = T)

# colsx <- c("#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f")
# cols <- colsx[as.factor(cgv$cgregion)]
plot(cgv, col = cgv$cols, lwd = 0.1,  add = T)

legend("bottomleft",
       legend = levels(cgv$cols),
       fill = legend.cols,
       border = NA,
       bty = "n", # turn off the legend border
       cex = 3,
       horiz = F)
dev.off()

tmap_options(check.and.fix = TRUE)
p1 <- tm_shape(cb) +
  tm_polygons(col = "#f0f0f0") +
  tm_borders("#bdbdbd", lwd = .5) + 
  tm_layout(frame = FALSE)

cgv <- st_as_sf(cgv)
cgvs <- s2::s2_geog_from_wkb(st_as_binary(st_geometry(cgv)), check = FALSE)
x <- s2::s2_rebuild(cgvs)
x <- st_as_sf(x)
st_geometry(cgv) <- st_geometry(x)

p2 <- tm_shape(cgv) +
  tm_polygons(col="cgregion", title="", style="cat", 
              palette=get_brewer_pal(palette="Set1", n=6, plot=FALSE))+
  tm_layout(main.title = "CGIAR regions", title.size = 1.5, 
            frame = FALSE,
            title.position = c("right", "top"), 
            legend.outside=FALSE, legend.position= c("left", "bottom"))
p2

library(rgeos)
dc1 <- gBuffer(cgv, byid=TRUE, width=0)
dc1 <- aggregate(dc1, by = "ORIG_FID")