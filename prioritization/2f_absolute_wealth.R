# remotes::install_github('vincentarelbundock/WDI')
library(WDI)
library(tidyverse)
library(countrycode)

# https://stackoverflow.com/questions/23223548/how-can-i-generate-a-sample-from-a-log-normal-distribution-with-pareto-tail-in-r
# https://github.com/Jihoon/DLE-inequality
# https://tjmurphy.github.io/jabstb/lognormal.html
yr.target <- 2030
yr.base <- 2016

historical <- WDI(country = c("IN", "NE", "RW", "ZA"), indicator = c("NY.GDP.PCAP.KD.ZG", "SI.POV.GINI")) %>% 
  mutate(iso3c = countrycode(iso2c, 'iso2c', 'iso3c'))  %>% rename(gr = NY.GDP.PCAP.KD.ZG, gini = SI.POV.GINI) %>% 
  filter(year <= yr.base) %>%
  group_by(iso3c) 

historical <- historical %>%
  filter(!is.na(gini)) %>%
  rbind(historical %>% filter(iso3c=="IND", year==yr.base) %>% mutate(gini=37.8)) %>%
  mutate(recent = ifelse(year == max(year), "Latest", "Past")) %>%
  arrange(country)

# GDP per cap, PPP (constant 2017 international $)
raw.gdp.pcap <- WDI(country = "all", indicator = "NY.GDP.PCAP.PP.KD", 
                    start = 1990, end = 2019, extra = TRUE, cache = NULL) %>%
  # filter(region!="Aggregates") %>% 
  select(-iso2c, -(capital:lending)) 
names(raw.gdp.pcap)[2] <- 'GDP.PCAP'

gdp.pcap.base <- raw.gdp.pcap %>% filter(iso3c %in% c('RWA', 'NER', 'IND', 'ZAF', 'LIC'), year == yr.base) %>%
  rename(avg.base=GDP.PCAP) %>% select(-region, -country)

gdp.thres <- gdp.pcap.base %>% filter(iso3c=="LIC") %>% select(avg.base) %>% as.numeric()

scaler_infl <- WDI(country = c("IN", "RW"), indicator = c("NY.GDP.DEFL.KD.ZG"), start=2012, end = yr.base) %>% 
  rename(r=NY.GDP.DEFL.KD.ZG) %>% mutate(r=r/100+1) %>% group_by(country) %>% summarise(r.tot = prod(r)) %>% 
  ungroup() %>% summarise(r = mean(r.tot))

# Household & NPISH share of GDP
sh.hh.NPISH <- WDI(country = c("IN", "NE", "RW", "ZA"), indicator = "NE.CON.PRVT.ZS", start=yr.base, end = yr.base) %>% 
  mutate(iso3c = countrycode(iso2c, 'iso2c', 'iso3c')) %>% mutate(sh = NE.CON.PRVT.ZS/100)

# Threshold for equivalant of 1.9$/day for yr.base (2016)
thres1.val <- as.numeric(1.9 * scaler_infl * 365)
thres2.val <- as.numeric(3.2 * scaler_infl * 365)

# $1.9 is for per-capita income, need to translate it to per-cap GDP
thres1 <- as.list(thres1.val / sh.hh.NPISH$sh)
thres2 <- as.list(thres2.val / sh.hh.NPISH$sh)
names(thres1) <- sh.hh.NPISH$iso3c
names(thres2) <- sh.hh.NPISH$iso3c

master.sub.wb <- gdp.pcap.base %>% filter(iso3c != "LIC") %>% 
  left_join(historical %>% filter(recent=="Latest") %>% select(iso3c, gini), by="iso3c") %>%
  # mutate(min.base=0, dle.thres = as.numeric(thres2)) %>%   # This is for plotting IND illustrative lognorm curves.
  mutate(min.base=0, dle.thres = as.numeric(thres1)) %>%  # This is for plotting indifference curve sets.
  rename(gini.base = gini) %>% select(iso3c, year, everything())

country.list <- split(master.sub.wb, seq(nrow(master.sub.wb)))
names(country.list) <- master.sub.wb$iso3c

sc.list <- lapply(country.list, GetScaler)

GetRefLognorm <- function(cty.data) {
  gini.base  = cty.data$gini.base/100  # WDI has pct values.
  avg.base   = cty.data$avg.base  
  min.base   = cty.data$min.base 
  dle.thres   = cty.data$dle.thres 
  
  # X domain dependent on country distribution
  del = dle.thres*1e3/12000 # x interval (based on dle.thres to make the shifted distribution start right from )
  x0 = seq(0, avg.base*1e3, del)
  
  x0 <- sort(gg$GDP.per.capita)
  y0 <- RefLognorm(x0, g$Gini, g$GDP.per.capita, min.base) 

  
  cty.data$y0=y0
  
  return(cty.data)
}

library(terra)



DrawRefLognorm <- function(n, gini, avg, min=0){
  gini_baselognormal = gini *avg / (avg-min) # Assume scaler=1 for this base transformation
  
  # Parameters of the base log normal pdf
  sdlog_hat = qnorm((gini_baselognormal+1)/2)*sqrt(2) 
  mlog_hat = log(avg-min) - (sdlog_hat^2)/2 
  
  x = rlnorm(n, mlog_hat, sdlog_hat)
  # x = qlnorm((1:n)/(n+1), mlog_hat, sdlog_hat)
  return(x)
}

RefLognorm <- function(xval, gini, avg, min=0){
  gini_baselognormal = gini *avg / (avg-min) # Assume scaler=1 for this base transformation
  
  # Parameters of the base log normal pdf
  sdlog_hat = qnorm((gini_baselognormal+1)/2)*sqrt(2) 
  mlog_hat = log(avg-min) - (sdlog_hat^2)/2 
  
  x0 = dlnorm(xval, mlog_hat, sdlog_hat)
  # F0 = plnorm(xval, mlog_hat, sdlog_hat)
  
  return(x0)
  # return(list(x0, F0))
}


datadir <- "G:/My Drive/work/ciat/eia/analysis"

ff <- list.files(file.path(datadir, "input/facebook/relative-wealth-index-april-2021"), 
                 pattern = ".csv", full.names = T) 
cc <- raster::ccodes()
iso <- 'KEN'
country <- cc$NAME[cc$ISO3 == iso]

f <- grep(iso, ff, value = T)
d <- read.csv(f)
d$rank <- rank(d$rwi, ties.method = "random")


# xdata$icdf_log <- qlnorm(xdata$rank, meanlog=mu, sdlog=sigma)
# xdata$awe_log <- xdata$icdf_log * ggdp / sum(xdata$icdf_log)
# ggdp <- gdp_per_cap * #gridcells
  

gg <- read.csv(file.path(datadir, "input/facebook/gdp_gini.csv"))
g <- gg[grep(country, gg$Country),]


w <- DrawRefLognorm(n=nrow(gg), gini = g$Gini, avg = g$GDP.per.capita)
ws <- sum(1/w)
# ws <- sum(w)
d$aws <- d$rank*g$GDP.per.capita/(nrow(d)*ws)
v <- vect(d, geom=c("longitude", "latitude"))
ref <- rast(file.path(datadir, "outdir/all_raster/wc2.1_5m_elev.tif"))
rref <- crop(ref, v)
rr <- lapply(c("rwi", "aws"), function(x)
  {rx <- rasterize(v, rref, x); names(rx) <- x; return(rx)})
rr <- c(rr[[1]], rr[[2]])
plot(rr)

gc <- g$Gini
gdp <- g$GDP.per.capita

# dummy data
set.seed(123)
x <- runif(10000)

# actual data looks like; 
# x <- runif(10000, -2, max = 2)

# Pareto
a <- (1+gc)/2*gc 
th <- (1-1/a)*gdp
pr <- Pareto::qPareto(x, th, a) # x should be probability, what's the implication for real data? probability of getting a rank?
pr <- pr[is.finite(pr)]
w1 <- pr*gdp/sum(pr)

# Log-normal

z <- (gc+1)/2
sigma <- sqrt(2)*(1/(1+exp(-z))) # is it the correct function form for probit?
mean <- log(gdp) - sigma^2/2
lr <- qlnorm(0.5, meanlog = mean, sdlog = sigma)
w2 <- lr*gdp/sum(lr)


#N = number of samples
#N = number of sample
rpar <- function(N,g,k){
  
  if (k < 0 | g <0){
    stop("both k and g >0")
  }
  
  k*(1-runif(N))^(-1/g)
}


rand_pareto <- rpar(1e5, 5, 16)
hist(rand_pareto, 100, freq = FALSE)