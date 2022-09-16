# USAID request for RWA, UGA, BDI, DRC

# install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
library(sf)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

dd <- read.csv(file.path(datadir, "outdir/level_stat/level123_cgregion_country_farming_system.csv"))
iso <- c("BDI", "RWA", "UGA", "COD")
d1 <- dd[dd$ISO_A3 %in% iso, ]
# d1 <- d1[grep("Highland", d1$farming_system),]

write.csv(d1[,1:11], file.path(datadir, "outdir/level_stat/great_lakes_highlands_usaid.csv"), 
          row.names = FALSE)


# this after combining Central American Dry Corridor with Dixon maps
cgv <- vect(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))
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

cgv1 <- cgv[cgv$ISO_A3 %in% iso, ]
cgv2 <- cgv1[grep("Highland", cgv1$farming_system),]

# plot with elevation
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
names(ref) <- "elevation"

r <- crop(ref, cgv1)
plot(r, type="interval", breaks=c(0,500,1000,1500,4000), 
     plg=list(legend=c("<500", "500-1000","1000-1500",">1500"), 
              title = "elevation range"))
plot(cgv1, add = T, border = "red", lwd = 1)
plot(cgv2, add = T, border = "blue", lwd = 1.3)
