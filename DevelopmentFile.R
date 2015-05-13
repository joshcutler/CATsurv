
## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("~/Github/catSurv") #This will need to be changed to match your directory

## This can be run many times as the code is updates
current.code <- as.package("catSurv")
load_all(current.code)
document(current.code)

## Install the package
#install(pkg=current.code, local=TRUE)

#see what things may be wrong...
#check(current.code)

## Build a version of the package to share manually
build(current.code, path=getwd())


##
x<-c(1,2,3,4)

vmean <- Vectorize(mean)
mean(x)
vmean(x)

x <- 1:100
filter(x, rep(1, 3))
