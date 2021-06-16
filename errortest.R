install.packages("logr")
install.packages("tryCatchLog")
logdir <- "C:/Users/anibi/Documents/test/logs"

library(logr)

myfunction <- function(x,...){
  y <- x*10
  print(y)
  z <- log(y)
  print(z)
}

myLogFunction <- function(i, x, logdir){
  dir.create(logdir, F, T)
  # went to fancy with file name, but you get the idea
  logr::log_open(file.path(logdir,paste0("log_", x[[i]], ".log")))
  out <- tryCatch(myfunction(x[[i]]))
  logr::log_print(out)
  logr::log_close()
}  

plan(multiprocess)
x <- list(1, "a", "B", 4, -10, 15) 
future.apply::future_lapply(1:length(x), myLogFunction, x, logdir)
future:::ClusterRegistry("stop")

######################################################################






library(here)
library(tryCatchLog)
library(futile.logger)



options(keep.source = TRUE)        
options("tryCatchLog.write.error.dump.file" = TRUE) 
# Script loads all necessary functions
flog.appender(appender.file(
  here("errors","error.log")))
tryCatchLog(source(here("errortest.R"))) 
