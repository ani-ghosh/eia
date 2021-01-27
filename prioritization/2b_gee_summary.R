# clean GEE summary stat and calculate npp trend

######################################################################################
# working directory
dir <- "G:\\My Drive\\work\\ciat\\eia\\analysis"
setwd(dir)

gee <- read.csv("outdir/gee/eia_kpi_gee.csv", stringsAsFactors = FALSE)

# columns to keep except npp
c1 <- c("uid","country","iso3","area_sqkm","area_pct","croparea_sum","orc_mean_mean","ph_mean_mean","population_sum")
d1 <- subset(gee, select = c1)
# calculate population_sum/croparea_sum

# npp trend
npp_sum <- gee[, grep("Npp_sum", names(gee), value = TRUE)]
npp_mean <- gee[, grep("Npp_mean", names(gee), value = TRUE)]

# npp trend function
getSlope <- function(d){
  x <- 1:ncol(d) - 1
  d[is.na(d)] <- 0
  fit <- summary(lm(t(d) ~ x))
  n <- names(fit)
  ss <- sapply(n, function(z)coefficients(fit[z])[[1]][2,1])
  names(ss) <- NULL
  ss <- round(ss, 3)
  return(ss)
}

npp_sum_trend <- getSlope(npp_sum)
npp_mean_trend <- getSlope(npp_mean)

dd <- data.frame(d1, npp_sum_trend = npp_sum_trend, npp_mean_trend = npp_mean_trend)
write.csv(dd, "outdir/gee/eia_kpi_gee_clean.csv", row.names = FALSE)
