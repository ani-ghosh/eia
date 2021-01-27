# combine both earthstat, gee, nue summary and convert back to spatial layer
library(raster)
library(sf)

# working directory
dir <- "G:\\My Drive\\work\\ciat\\eia\\analysis"
setwd(dir)

# country-farming-system boundary
cfs <- shapefile("input/boundary/country/country_farming_system.shp")

# summary
estat <- read.csv("outdir/earthstat/yield_summary_earthstat.csv", stringsAsFactors = FALSE)
gstat <- read.csv("outdir/gee/eia_kpi_gee_clean.csv", stringsAsFactors = FALSE)
# combine
dd <- merge(gstat, estat, by = "uid")

# nitrogen use efficiency
nue <- read.csv("outdir/rue/nue_cleaned_trend.csv", stringsAsFactors = FALSE)

# country names will not match
# clist <- unique(dd$country)
# clist[!clist %in% nue$country]

# manual inspection and change
# missing in nue 
# "Afghanistan","Argentina","Armenia","Azerbaijan","Bosnia and Herzegovina","Belarus"               
# "Belize", "Cambodia", "Equatorial Guinea", "Eritrea", "Georgia", "Iraq",
# "Kazakhstan", "Kyrgyzstan"

# changed name "Bolivia,"Cï¿½te d'Ivoire","Laos","Syria",
# "Tanzania","Venezuela","Vietnam"

# can't match "Congo", "Sudan"

# merge earthstat and gee summary with nue
ddn <- merge(dd, nue, by = "country", all.x = TRUE)

# merge all summary with spatial layer
dcfs <- merge(cfs, ddn, by = "uid", all.x = TRUE)
shapefile(dcfs, "outdir/all_kpi_summary.shp", overwrite = TRUE)

# save as geojson
dsf <- st_as_sf(dcfs)
st_write(dsf, "outdir/all_kpi_summary.geojson", delete_layer = TRUE)
