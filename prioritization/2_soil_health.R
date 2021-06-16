library(terra)

# data directory
datadir <- "G:/My Drive/work/ciat/eia/analysis"

# reference raster
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
names(ref) <- "elevation"

# soil directory
soildir <- file.path(datadir, "/input/soil")

# human impact
hh <- list.files(file.path(datadir, "input/human_impact/Global areas of low human"), 
                 pattern = ".tif$", full.names = TRUE)

hr <- rast(hh)

# reclassify to make non-land to NA, high impact = 0, low impac

# relative soil health
libs <- c("raster","rgeos","RSQLite","RODBC","sp","sf","terra")
lapply(libs, library, character.only = TRUE)

# download from https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1247

# data directory
soildir <- "G:/My Drive/work/ciat/eia/analysis/input/soil/HWSD"
setwd(soildir)

# load data
hwsd <- rast("./HWSD_RASTER/hwsd.bil")

# We can also use just the first three digits of the map unit codes, which
# presumably are also a meaningful grouping; to remove the ‘hundreds’
# places we use the %/% “integer divide” operator or it's equivalent.
hwsd3 <- round(hwsd/100)

# connecting to attribute table
ch <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=HWSD.mdb")


# reference raster
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
# https://github.com/dlebauer/rhwsd