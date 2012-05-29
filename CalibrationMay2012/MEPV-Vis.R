setwd("/Users/jmontgomery/GitHub/CATSurv/CalibrationMay2012/")
source("../CATSurv.R")
library("multicore")
library("foreach")
library("doMC")
library("plyr")
library("compiler")



mepv.plotter <- function(answers){
  itemParams <- read.csv(file="ItemParams.csv", header=TRUE, as.is=TRUE, row.names=1)
  nItems <- nrow(itemParams)
  guessing <- rep(0, nItems)
  questions <- data.frame(difficulty=itemParams$difficulty, discrimination=itemParams$discrimination,
                          guessing=guessing,
                          answers=answers)
  cat <- new("CATsurv", questions=questions, priorParams=c(0, 1.75))
  evalItems <- nextItem(cat)$all.estimates
  new.item <- nextItem(cat)$next.item
  evalItems <- evalItems[order(evalItems$difficulty),]
  with(evalItems, plot(difficulty, epv, type="b", cex=.4))
  with(evalItems[as.character(new.item),], points(difficulty, epv, pch="X"))
  print(rownames(itemParams[as.numeric(new.item),]))
  return(new.item)
}




answers <- rep(NA, 65)
mepv.plotter(answers)
answers[5] <- 1
mepv.plotter(answers)
answers[26] <- 1
mepv.plotter(answers)
answers[18] <- 1
mepv.plotter(answers)
answers[32] <- 1
mepv.plotter(answers)
answers[15] <- 1
mepv.plotter(answers)
answers[28] <- 1
mepv.plotter(answers)


answers <- rep(NA, 65)
mepv.plotter(answers)
answers[5] <- 0
mepv.plotter(answers)
answers[17] <- 0
mepv.plotter(answers)
answers[7] <- 0
mepv.plotter(answers)
answers[41] <- 0
mepv.plotter(answers)
answers[9] <- 0
mepv.plotter(answers)
answers[39] <- 0
mepv.plotter(answers)


