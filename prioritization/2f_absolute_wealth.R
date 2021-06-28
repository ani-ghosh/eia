datadir <- "G:/My Drive/work/ciat/eia/analysis"

ff <- list.files(file.path(datadir, "input/facebook/relative-wealth-index-april-2021"), 
                 pattern = ".csv", full.names = T) 

f <- grep("BGD", ff, value = T)
d <- read.csv(f)
d$rank <- rank(d$rwi, ties.method = "random")

gc <- 32.4
gdp <- 1698.26

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
lr <- qlnorm(x, meanlog = mean, sdlog = sigma)
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