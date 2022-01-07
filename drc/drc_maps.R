library(tmap)
library(tmaptools)
library(sf)
library(raster)
library(terra)
library(stars)
library(geodata)
library(rvest)

# https://keen-swartz-3146c4.netlify.app/

tmap_options(max.categories = 200)

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

iso <- "COD"
fs <- cgv[cgv$ISO_A3 %in% iso,]

# country boundary
v1 <- raster::getData("GADM", country = iso, level = 1, 
                     path = file.path(datadir, "input/boundary/country"))

v <- raster::getData("GADM", country = iso, level = 2, 
                     path = file.path(datadir, "input/boundary/country"))

vt <- v[v$TYPE_2 == "Territoire",]
vt$ID <- as.numeric(as.factor((vt$NAME_2)))
vt$legend <- paste(vt$ID, ":", vt$NAME_2)

# # read the list of 145 territories table from wikipedia
# url <- "https://en.wikipedia.org/wiki/List_of_territories_of_the_Democratic_Republic_of_the_Congo"
# 
# lt <- read_html(url) %>% 
#   html_node(xpath = "//*[@id='mw-content-text']/div[1]/table[2]") %>% 
#   html_table(fill = T)
# lt <- lt[1:145,]

cpt <- shapefile("G:/My Drive/work/ciat/eia/DRC/data/drc/capitals/capitals.shp")

set.seed(1)
om <- tm_shape(v1) +
  tm_borders(lwd = 2, col = "#feb24c", lty = "solid") +
  tm_shape(vt) +
  tm_borders(lwd = 0.5, col = "#bd0026") +
  # tm_text("ID", size = 0.5) +
  tm_shape(v1) +
  tm_text("NAME_1", size = 0.5, auto.placement = TRUE, bg.color = "white", bg.alpha = 1) +
  tm_shape(cpt) +
  tm_dots(col="Capital", palette = "red", size = 0.1, shape = 16, legend.show = FALSE) +
  tm_add_legend("line", col = c("#feb24c", "#bd0026"), lwd = c(2, 0.5),
                lty = c("solid", "solid"), labels = c("Province", "Terriotry"),
                title = "Administrative")  + 
  tm_scale_bar(width = 0.4, lwd = 0.3, size = 0.5) +
  tm_compass(type = "arrow", size = 2, position = c("left", "top"))

p1 <- tm_shape(fs) + 
  tm_polygons(col = "farming_system", palette = c("#1f78b4", "#fdbf6f", "#33a02c", "#8dd3c7", "#ffff33", "#e5d8bd"),
              title = "Farming System", border.alpha = 0) 
p1m <- p1 + om +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 0.8,
            inner.margins=c(0.1,0.15,0.05,0.03))

tmap_save(p1m, filename =  file.path(datadir, "DRC_farming_system_territory.png"),
          width = 5, height = 5, dpi = 300)


# soil
soil <- st_read("G:/My Drive/work/ciat/eia/DRC/data/drc/fao_soils/fao_soils.shp")
p2 <- tm_shape(soil) + 
  tm_polygons(col = "MAJOR__SOI", palette = c(get_brewer_pal("Pastel1", n = 11), "darkblue"),
              title = "Soil Type", border.alpha = 0) +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 0.8,
            inner.margins=c(0.1,0.2,0.05,0.03))
p2m <- p2 + om
tmap_save(p2m, filename =  file.path(datadir, "DRC_soil_type_territory.png"),
          width = 5, height = 5, dpi = 300)


# aez
dd <- jsonlite::fromJSON("https://gaez-services.fao.org/server/rest/services/LR/ImageServer/legend?bandIds=&renderingRule=&f=pjson")
ds <- as.data.frame(dd$layers$legend)
ds <- data.frame(pixelvalue = c(1:(nrow(ds)-1)), classname = ds$label[-1])

# v0 <- raster::getData("GADM", country = iso, level = 0, 
#                             path = file.path(datadir, "input/boundary/country"), type = "sf")
# 
# aez <- read_stars("G:/My Drive/work/ciat/eia/DRC/data/AEZ16 r2.0 - TIF/AEZ16_CLAS--SSA.tif")
# saez <- aez[v0]
# droplevels(saez)
r <- raster("G:/My Drive/work/ciat/eia/DRC/data/gaez4_33_classes_v9v2red_5m_CRUTS32_Hist_8110_100_avg.tif")
r <- crop(r, v1)
r <- mask(r, v1)
x <- unique(values(r))
dsx <- ds[ds$pixelvalue %in% x,]

# fr <- ratify(r)
# rat <- levels(fr)[[1]]
# rat$aez <- dsx$classname
# levels(r) <- rat

p3 <- tm_shape(r) +
  tm_raster(style = "cat", 
            palette = c(get_brewer_pal("Pastel1", n = 10), "#253494"), labels = dsx$classname,
            title = "AEZ") +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 0.5,
          inner.margins=c(0.15,0.3,0.05,0.03)) +
  tm_credits("Source: Global Agro-Ecological Zoning version 4 (GAEZ v4) (https://gaez.fao.org)",
               size = 0.5, position = c("right", "bottom"))

p3m <- p3 + om 
tmap_save(p3m, filename =  file.path(datadir, "DRC_aez_territory.png"),
          width = 5, height = 5, dpi = 300)



# other summary variables
