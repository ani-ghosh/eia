library(tmap)
library(tmaptools)
library(sf)
library(raster)
library(terra)
library(geodata)
library(rvest)
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

# read the list of 145 territories table from wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_territories_of_the_Democratic_Republic_of_the_Congo"


lt <- read_html(url) %>% 
  html_node(xpath = "//*[@id='mw-content-text']/div[1]/table[2]") %>% 
  html_table(fill = T)
lt <- lt[1:145,]

cpt <- shapefile("G:/My Drive/work/ciat/eia/DRC/data/drc/capitals/capitals.shp")

p1 <- tm_shape(fs) + 
  tm_polygons(col = "farming_system", palette = c("#1f78b4", "#fdbf6f", "#33a02c", "#8dd3c7", "#ffff33", "#e5d8bd"),
              title = "Farming System", lwd = 0,) +
  tm_shape(v1) +
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
                title = "Administrative") +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 0.8,
            inner.margins=c(0.1,0.15,0.05,0.03)) + 
  tm_scale_bar(width = 0.4, lwd = 0.3, size = 0.5) +
  tm_compass(type = "arrow", size = 2, position = c("left", "top"))

tmap_save(p1, filename =  file.path(datadir, "DRC_farming_system_territory.png"),
          width = 5, height = 5, dpi = 300)

leg <- tm_shape(vt) +
  tm_polygons(col = "legend", title = "Territory", legend.is.portrait = FALSE) +
  tm_layout(legend.only = TRUE,
            legend.title.size = 1.5,
            legend.text.size = 1) +
  tm_legend(stack = "horizontal")

leg
