
library(ltm)
library(plyr)
rm(list=ls())

setwd("~/github/CATsurv/TestSampleJune2012/")

## Read in raw data
testData <- read.csv("results.csv", header=T, row.names="response_id")
testData <- testData[,-1] # Remove firs useless column


## Read in order
order <- read.csv("ordered.csv", header=T, row.names="response_id")



## Read in mapping
mapping <- read.csv("Mapping.csv", header=T)
colnames(mapping)


use.these <- c(1804:1835, 1837:1868, 1708:1710, 1712:1744, 1746:1753, 1755:1772, 1774:1775)
theseAreStatic <- c(1708:1710, 1712:1744, 1746:1753, 1755:1772, 1774:1775)
theseAreDyn <- c(1804:1835, 1837:1868)

## Recode NAs to 0s for knowledge questions
for (i in use.these){
  testData[is.na(testData[,paste("question_", i, sep="")]),paste("question_", i, sep="")] <- 0
  testData[(testData[,paste("question_", i, sep="")]==-1), paste("question_", i, sep="")] <- 0
}


## Extract conditions
dynCond <- testData[,"question_1712"]==-99


## Make sub-setted data
dynResultsRaw <- testData[dynCond,]
staticResultsRaw <- testData[!dynCond,]

## remove irrelevant columns for each subset
dynResultsRaw <- dynResultsRaw[,!colnames(testData) %in% paste("question_", theseAreStatic, sep="")]
staticResultsRaw <- staticResultsRaw[,!colnames(testData) %in% paste("question_", theseAreDyn, sep="")]




## Rename columns in static file
colnames(dynResultsRaw)[colnames(dynResultsRaw)%in%paste("question_", theseAreDyn, sep="")] <- paste("q", 1:64, sep="")




## Rename associated entries in the order and mapping files
counter <- 1
for (i in theseAreDyn){
  order[order==i] <- c(1:64)[counter]
  mapping$Dynamic[mapping$Dynamic==i] <- c(1:65)[counter]
  counter <- counter+1
}


#Rename columns in the dynamic, and order files
for (i in 1:64){
  colnames(staticResultsRaw)[colnames(staticResultsRaw)==paste("question_",mapping$Static[mapping$Dynamic==i], sep="")] <- paste("q", i, sep="")
  j <- mapping$Static[mapping$Dynamic==i]
  order[order==j] <- i
}


## Read in the calibration datasset
CalibrationData <- read.csv("../CalibrationMay2012/RawResults.csv", header=T, row.names="response_id")
CalibrationData <- CalibrationData[,-1] # Remove firs useless column
dim(CalibrationData)


# Columns for Calibration data
theseAreCal <- c(1583:1588,1590:1629, 1631:1651)

# Recode NAs to 0s for knowledge questions
for (i in theseAreCal){
  CalibrationData[is.na(CalibrationData[,paste("question_", i, sep="")]),paste("question_", i, sep="")] <- 0
  CalibrationData[(CalibrationData[,paste("question_", i, sep="")]==-1), paste("question_", i, sep="")] <- 0
}


## Rename columns in the calibration using mapping
for (i in 1:64){
  colnames(CalibrationData)[colnames(CalibrationData)==paste("question_",mapping$Calibration[mapping$Dynamic==i], sep="")] <- paste("q", i, sep="")
}




##  Now merge it all together
temp1 <- CalibrationData[,paste("q", 1:64, sep="")]
temp1$sample <- 1
temp2 <- staticResultsRaw[,paste("q", 1:64, sep="")]
temp2$sample <- 2
temp3 <- dynResultsRaw[,paste("q", 1:64, sep="")]
temp3$sample <- 3

allData <- rbind(temp1, temp2, temp3)
colnames(allData)
table(allData$sample)
table(allData$q3)


## fit the ltm model using all data.  The -65 gets rid fo the "sample" indicator.  In this case, I am *only* estimating with the new (second) dataset.
ltm.res <- ltm(allData[allData$sample>1,-65]~z1, IRT.param=TRUE)



#
source("../CATSurv.R")

# Set up questions object
questions <- data.frame(difficulty=coefficients(ltm.res)[,"Dffclt"], discrimination=coefficients(ltm.res)[,"Dscrmn"], guessing=c(0), answers=c(NA))
cat=new("CATsurv", questions=questions, priorParams=c(0,1))


testResults <- function(rows, .n){
  
  res <- ldply(rows, 
               .fun <- function(i, n=.n){
                 i=as.character(i)
                 cat=storeAnswer(cat, 1:64, NA)
                 these.qs <- unlist(order[i,1:n])
                 cat=storeAnswer(cat, these.qs, unlist(allData[i,these.qs]))
                 theta.est <- estimateTheta(cat)
                 theta.se <- estimateSE(cat, theta.est)
                 cat=storeAnswer(cat, 1:64, unlist(allData[i,-65]))
                 theta.true <- estimateTheta(cat)
                 out <- data.frame(theta.true=theta.true, theta.est=theta.est, theta.se=theta.se)
                 return(out)
               }
               
               )
  return(res)
}


dynRes3 <- testResults(rows=rownames(dynResultsRaw), .n=3)
dynRes5 <- testResults(rows=rownames(dynResultsRaw), .n=5)
dynRes7 <- testResults(rows=rownames(dynResultsRaw), .n=7)
dynRes10 <- testResults(rows=rownames(dynResultsRaw), .n=10)
statRes3 <- testResults(rows=rownames(staticResultsRaw), .n=3)
statRes5 <- testResults(rows=rownames(staticResultsRaw), .n=5)
statRes7 <- testResults(rows=rownames(staticResultsRaw), .n=7)
statRes10 <- testResults(rows=rownames(staticResultsRaw), .n=10)




results.plotter <- function(y.min=0, y.max=2.5, y.lab="MSE", xlab=expression(paste("True values of ", theta)),
                           dyn.obj = dynRes3,static.obj = statRes3, this.title="N=3"){
  dyn.obj=dyn.obj[order(dyn.obj$theta.true),]
  static.obj=static.obj[order(static.obj$theta.true),]
  plot(NULL, xlim=c(-3.1,3.1), ylim=c(y.min, y.max), ylab=y.lab, xlab=expression(paste("True values of ", theta))  )
  mse.dyn <- (dyn.obj$theta.est-dyn.obj$theta.true)^2 + dyn.obj$theta.se^2
  points(dyn.obj$theta.true, mse.dyn,  col="skyblue", pch=24, cex=.5)
  mse.stat <- (static.obj$theta.est-static.obj$theta.true)^2 + static.obj$theta.se^2
  points(static.obj$theta.true, mse.stat, col="pink", pch=23, cex=.5)
  lines(dyn.obj$theta.true, predict(loess(mse.dyn ~ dyn.obj$theta.true, span=.5, degree=1)), type="l", lwd=2, col="blue", lty=2)
  lines(static.obj$theta.true, predict(loess(mse.stat ~ static.obj$theta.true, span=.5, degree=1)), type="l", lwd=2, col="red")
    rect(-3.35, y.max-.075, 3.35, y.max+100, col="gray80", lwd=1)
  text(0, y.max+.02, this.title, cex=1.25)
}
pdf(file="~/Dropbox/Adaptive Surveys/TexFiles/EmpResults.pdf", width=6, height=6)
par(bg="white", mgp=c(1,0,0), tcl=0, mar=c(0,2,2.1,0), xaxt="n", yaxt="s", bty="o")
layout(matrix(1:6, nrow=3, ncol=2), height=c(3,3,1.3))
results.plotter(dyn.obj=dynRes3, static.obj=statRes3)
par(mar=c(2.1,2,0,0), xaxt="s")
results.plotter(dyn.obj=dynRes7, static.obj=statRes7, this.title="N=7")
par(bty="n", xaxt="n", yaxt="n", mar=c(0,0,0,0), mgp=c(0,0,0))
plot(NULL, xlim=c(0,1), ylim=c(0,1),  ylab="", xlab="")
legend(.5,.9, c("Static scale est.", "CAT scale est.", "Loess for static", "Loess for CAT"), pch=c(23,24, 26, 26), col=c("pink", "skyblue", "red", "blue"),  lty=c(0,0,1,2), lwd=c(1,1,2,2), cex=1.3)
par(mar=c(0,0,2.1,2), xaxt="n", yaxt="n", bty="o")
results.plotter(dyn.obj=dynRes5, static.obj=statRes5, this.title="N=5")
par(mar=c(2.1,0,0,2), xaxt="s", mgp=c(1,0,0))
results.plotter(dyn.obj=dynRes10, static.obj=statRes10, this.title="N=10")
dev.off()



results.plotter.bias <- function(y.min=-2.5, y.max=2.5, y.lab="Bias", xlab=expression(paste("True values of ", theta)),
                           dyn.obj = dynRes3,static.obj = statRes3, this.title="N=3"){
  dyn.obj=dyn.obj[order(dyn.obj$theta.true),]
  static.obj=static.obj[order(static.obj$theta.true),]
  plot(NULL, xlim=c(-3.1,3.1), ylim=c(y.min, y.max), ylab=y.lab, xlab=expression(paste("True values of ", theta))  )
  mse.dyn <- (dyn.obj$theta.est-dyn.obj$theta.true)
  points(dyn.obj$theta.true, mse.dyn,  col="skyblue", pch=24, cex=.5)
  mse.stat <- (static.obj$theta.est-static.obj$theta.true)
  points(static.obj$theta.true, mse.stat, col="pink", pch=23, cex=.5)
  lines(dyn.obj$theta.true, predict(loess(mse.dyn ~ dyn.obj$theta.true, span=.5, degree=1)), type="l", lwd=2, col="blue", lty=2)
  lines(static.obj$theta.true, predict(loess(mse.stat ~ static.obj$theta.true, span=.5, degree=1)), type="l", lwd=2, col="red")
    rect(-3.35, y.max-.075, 3.35, y.max+100, col="gray80", lwd=1)
  text(0, y.max, this.title, cex=1.25)
}
par(bg="white", mgp=c(1,0,0), tcl=0, mar=c(0,2,2.1,0), xaxt="n", yaxt="s", bty="o")
layout(matrix(1:6, nrow=3, ncol=2), height=c(3,3,1.3))
results.plotter.bias(dyn.obj=dynRes3, static.obj=statRes3)
par(mar=c(2.1,2,0,0), xaxt="s")
results.plotter.bias(dyn.obj=dynRes7, static.obj=statRes7, this.title="N=7")
par(bty="n", xaxt="n", yaxt="n", mar=c(0,0,0,0), mgp=c(0,0,0))
plot(NULL, xlim=c(0,1), ylim=c(0,1),  ylab="", xlab="")
legend(.5,.9, c("Static scale est.", "CAT scale est.", "Loess for static", "Loess for CAT"), pch=c(23,24, 26, 26), col=c("pink", "skyblue", "red", "blue"),  lty=c(0,0,1,2), lwd=c(1,1,2,2), cex=1.3)
par(mar=c(0,0,2.1,2), xaxt="n", yaxt="n", bty="o")
results.plotter.bias(dyn.obj=dynRes5, static.obj=statRes5, this.title="N=5")
par(mar=c(2.1,0,0,2), xaxt="s", mgp=c(1,0,0))
results.plotter.bias(dyn.obj=dynRes10, static.obj=statRes10, this.title="N=10")






my.wilcox.test <- function(dyn.obj=dynRes3, stat.obj=statRes3){
  mse.dyn <- (dyn.obj$theta.est-dyn.obj$theta.true)^2 + dyn.obj$theta.se^2
  mse.stat <- (static.obj$theta.est-static.obj$theta.true)^2 + static.obj$theta.se^2
  return(wilcox.test(mse.dyn, mse.stat))
}
my.wilcox.test(dynRes3, statRes3)
my.wilcox.test(dynRes5, statRes5)
my.wilcox.test(dynRes7, statRes7)
my.wilcox.test(dynRes10, statRes10)






### So what?

dynRes3 <- testResults(rows=rownames(dynResultsRaw), .n=3)
dynRes4 <- testResults(rows=rownames(dynResultsRaw), .n=4)
dynRes5 <- testResults(rows=rownames(dynResultsRaw), .n=5)
dynRes6 <- testResults(rows=rownames(dynResultsRaw), .n=6)
dynRes7 <- testResults(rows=rownames(dynResultsRaw), .n=7)
dynRes8 <- testResults(rows=rownames(dynResultsRaw), .n=8)
dynRes9 <- testResults(rows=rownames(dynResultsRaw), .n=9)
dynRes10 <- testResults(rows=rownames(dynResultsRaw), .n=10)
statRes3 <- testResults(rows=rownames(staticResultsRaw), .n=3)
statRes4 <- testResults(rows=rownames(staticResultsRaw), .n=4)
statRes5 <- testResults(rows=rownames(staticResultsRaw), .n=5)
statRes6 <- testResults(rows=rownames(staticResultsRaw), .n=6)
statRes7 <- testResults(rows=rownames(staticResultsRaw), .n=7)
statRes8 <- testResults(rows=rownames(staticResultsRaw), .n=8)
statRes9 <- testResults(rows=rownames(staticResultsRaw), .n=9)
statRes10 <- testResults(rows=rownames(staticResultsRaw), .n=10)


dyn <- NULL
dyn <- (c(dyn,median((dynRes3$theta.est-dynRes3$theta.true)^2+dynRes3$theta.se^2)))
dyn <- (c(dyn,median((dynRes4$theta.est-dynRes4$theta.true)^2+dynRes4$theta.se^2)))
dyn <- (c(dyn,median((dynRes5$theta.est-dynRes3$theta.true)^2+dynRes5$theta.se^2)))
dyn <- (c(dyn,median((dynRes6$theta.est-dynRes3$theta.true)^2+dynRes6$theta.se^2)))
dyn <- (c(dyn,median((dynRes7$theta.est-dynRes3$theta.true)^2+dynRes7$theta.se^2)))
dyn <- (c(dyn,median((dynRes8$theta.est-dynRes3$theta.true)^2+dynRes8$theta.se^2)))
dyn <- (c(dyn,median((dynRes9$theta.est-dynRes3$theta.true)^2+dynRes9$theta.se^2)))
dyn <- (c(dyn,median((dynRes10$theta.est-dynRes3$theta.true)^2+dynRes10$theta.se^2)))

stat <- NULL
stat <- (c(stat,median((statRes3$theta.est-statRes3$theta.true)^2+statRes3$theta.se^2)))
stat <- (c(stat,median((statRes4$theta.est-statRes4$theta.true)^2+statRes4$theta.se^2)))
stat <- (c(stat,median((statRes5$theta.est-statRes3$theta.true)^2+statRes5$theta.se^2)))
stat <- (c(stat,median((statRes6$theta.est-statRes3$theta.true)^2+statRes6$theta.se^2)))
stat <- (c(stat,median((statRes7$theta.est-statRes3$theta.true)^2+statRes7$theta.se^2)))
stat <- (c(stat,median((statRes8$theta.est-statRes3$theta.true)^2+statRes8$theta.se^2)))
stat <- (c(stat,median((statRes9$theta.est-statRes3$theta.true)^2+statRes9$theta.se^2)))
stat <- (c(stat,median((statRes10$theta.est-statRes3$theta.true)^2+statRes10$theta.se^2)))


dynB <- NULL
dynB <- (c(dynB,median(abs(dynRes3$theta.est-dynRes3$theta.true))))
dynB <- (c(dynB,median(abs(dynRes4$theta.est-dynRes4$theta.true))))
dynB <- (c(dynB,median(abs(dynRes5$theta.est-dynRes3$theta.true))))
dynB <- (c(dynB,median(abs(dynRes6$theta.est-dynRes3$theta.true))))
dynB <- (c(dynB,median(abs(dynRes7$theta.est-dynRes3$theta.true))))
dynB <- (c(dynB,median(abs(dynRes8$theta.est-dynRes3$theta.true))))
dynB <- (c(dynB,median(abs(dynRes9$theta.est-dynRes3$theta.true))))
dynB <- (c(dynB,median(abs(dynRes10$theta.est-dynRes3$theta.true))))

statB <- NULL
statB <- (c(statB,median(abs(statRes3$theta.est-statRes3$theta.true))))
statB <- (c(statB,median(abs(statRes4$theta.est-statRes4$theta.true))))
statB <- (c(statB,median(abs(statRes5$theta.est-statRes3$theta.true))))
statB <- (c(statB,median(abs(statRes6$theta.est-statRes3$theta.true))))
statB <- (c(statB,median(abs(statRes7$theta.est-statRes3$theta.true))))
statB <- (c(statB,median(abs(statRes8$theta.est-statRes3$theta.true))))
statB <- (c(statB,median(abs(statRes9$theta.est-statRes3$theta.true))))
statB <- (c(statB,median(abs(statRes10$theta.est-statRes3$theta.true))))


pdf(file="~/Dropbox/Adaptive Surveys/TexFiles/Comparison.pdf", width=6.5, height=4)
par(bg="white", mgp=c(1,0,0), tcl=0, mar=c(2.1,2,2.1,1), xaxt="s", yaxt="s", bty="o", mfrow=c(1,2))
plot(3:10, dyn, pch=24, col="blue", type="b", xlab="Number of questions", ylab="Median MSE", ylim=c(0.2, .62), main="MSE by Scale Length", lty=2)
points(3:10, stat, pch=23, col="red", type="b")
#abline(h=stat, col="pink")
legend(7,.62, c("Static scale", "CAT scale"), pch=c(23,24), col=c("red", "blue"),  lty=c(1,2), cex=.75)
plot(3:10, dynB, pch=24, col="blue", type="b", xlab="Number of questions", ylab="Median Deviation", ylim=c(0.21, .4), main="Deviation by Scale Length", lty=2) #
points(3:10, statB, pch=23, col="red", type="b")
#abline(h=statB, col="pink")
legend(7,.4, c("Static scale", "CAT scale"), pch=c(23,24), col=c("red", "blue"),  lty=c(1,2), cex=.75)
dev.off()




