# worldpop

iso <- "KEN"

burl <- "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj"
year <- 2020


u <- file.path(burl, year, iso, paste0(tolower(iso), "_ppp_", year, "_1km_Aggregated_UNadj.tif"))
download.file(u, destfile = basename(u), mode = "wb")
