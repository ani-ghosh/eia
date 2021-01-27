# resource use efficiency trend

# working directory
dir <- "G:\\My Drive\\work\\ciat\\eia\\analysis"
setwd(dir)

# trend function
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

nue <- readxl::read_excel("input/rue/41586_2015_BFnature15743_MOESM47_ESM.xlsx", 
                          skip = 1, sheet = 1)
dnue <- nue[, 2:ncol(nue)]
ns <- getSlope(dnue)

# clean country names
clist <- nue[,1, drop = TRUE]
clist <- gsub("\\'", "", clist)
ns <- data.frame(country = clist, nue_trend = ns, stringsAsFactors = FALSE)
write.csv(ns, "outdir/rue/nue_cleaned_trend.csv", row.names = FALSE)
