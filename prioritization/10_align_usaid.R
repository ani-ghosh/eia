library(terra)

# input
datadir <- "G:/My Drive/work/ciat/eia/analysis"

# eia selection summary is the final layer made by Jordan after removing the FS we are not interested in
# but is looks like we need to keep all the LS but remove the countries
rv <- readRDS(file.path(datadir, "output/eia_selection_summary.rds"))
tiso <- unique(rv$ISO_A3)
cgv <- vect(file.path(datadir, "input/boundary/country_farming_system_cg_regions.shp"))
cgv <- cgv[cgv$ISO_A3 %in% tiso, ]

s <- rast(file.path(datadir, 
                    "input/child_growth_failure/IHME_LMIC_CGF_2000_2017_STUNTING_PREV_MEAN_2017_Y2020M01D08.TIF"))


wstat <- extract(s, cgv, fun = mean, na.rm = TRUE)
wstat <- wstat[,2:ncol(wstat)]
cgv$stunting_2017 <- round(wstat*100, 0)


rr <- rast(file.path(datadir, "outdir/all_raster/KPI_global_raster_10km.tif"))
r <- subset(rr, c("poverty_avg5yr_imputed","povmap_global_subnational_infant_mortality_rates_v2_01"))

wstat1 <- extract(r, cgv, fun = mean, na.rm = TRUE)
wstat1 <- wstat1[,2:ncol(wstat1)]
cgv$poverty_5years <- round(wstat1$poverty_avg5yr_imputed, 0)
cgv$infant_mortality_rates <- round(wstat1$povmap_global_subnational_infant_mortality_rates_v2_01, 0)

cgv$FORMAL_ <- NULL
cgv$ECONOMY <- NULL
cgv$INCOME_ <- NULL
cgv$ar_sqkm <- NULL
names(cgv)[names(cgv) == "frmng_s"] <- "farming_system"
names(cgv)[names(cgv) == "cgregin"] <- "cgregion"

# two excel sheets: one with the simple CG/frmng_s
library(writexl)
library(dplyr)
dd <- as.data.frame(cgv)
dd$farming_system <- gsub("[[:digit:]]+","", dd$farming_system)
dd$farming_system <- gsub("\\.","", dd$farming_system)
dd$farming_system <- trimws(dd$farming_system)

d1 <- dd %>% 
  select(-c(NAME_EN, ISO_A3)) %>%
  group_by(cgregion, farming_system) %>%
  summarize_all(mean, na.rm = T) %>%
  mutate(across(c(stunting_2017, poverty_5years, infant_mortality_rates), round)) %>%
  ungroup()


d2 <- dd %>% 
  group_by(cgregion, farming_system, NAME_EN, ISO_A3) %>%
  summarize_all(mean, na.rm = T) %>%
  mutate(across(c(stunting_2017, poverty_5years, infant_mortality_rates), round)) %>%
  ungroup()

tmp <- write_xlsx(list(cg_fs = d1, country_fs = d2), path = file.path(datadir, "output/summary_table_level1.xlsx"))



library(sf)
x <- st_as_sf(cgv)
plot(x[,"stunting_2017"])

# 
# xx <- list.files("C:/Users/anibi/Documents/test/beans", pattern = "_in_weath.RDS", full.names = T)
# xx <- lapply(xx, readRDS)
# xx$fun <- mean
# xx$na.rm <- TRUE
# yy <- do.call(mosaic, xx)
