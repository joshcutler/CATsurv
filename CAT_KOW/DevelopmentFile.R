##########################################################################################
## CATpack - Development File - Myunghoon Kang, Elif Özdemir, Dalston Ward - March 2014 ##
##########################################################################################

## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("Myunghoon's WD HERE")
setwd("Elif's WD Here")
setwd("/Users/clockbob1/Documents/WashU 2nd Year/Applied Stats Programming/CATsurv/CAT_KOW") #This will need to be changed to match your directory

## This is run once when the package strcuture is first created
create(path="./CATPack", check=FALSE)

## This can be run many times as the code is updates
current.code <- as.package("CATPack")
load_all(current.code)
document(current.code)

## Install the package
install(pkg=current.code, local=TRUE)

#see what things may be wrong...
check(current.code)