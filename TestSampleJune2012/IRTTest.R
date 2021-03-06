
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

theseAreOutcomes <- c(1685:1699)



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

# Make outcome data for regressions at the bottom
statOut <- staticResultsRaw[,colnames(staticResultsRaw) %in% paste("question_", theseAreOutcomes, sep="")]
dynOut <- dynResultsRaw[,colnames(dynResultsRaw) %in% paste("question_", theseAreOutcomes, sep="")]

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




results.plotter <- function(y.min=0, y.max=2.5, y.lab="Squared error", xlab=expression(paste("True values of ", theta)),
                           dyn.obj = dynRes3,static.obj = statRes3, this.title="N=3"){
  dyn.obj=dyn.obj[order(dyn.obj$theta.true),]
  static.obj=static.obj[order(static.obj$theta.true),]
  plot(NULL, xlim=c(-3.1,3.1), ylim=c(y.min, y.max), ylab=y.lab, xlab=expression(paste("True values of ", theta))  )
  mse.dyn <- (dyn.obj$theta.est-dyn.obj$theta.true)^2 + dyn.obj$theta.se^2
  points(dyn.obj$theta.true, mse.dyn,  col="gray90", pch=24, cex=.5)
  mse.stat <- (static.obj$theta.est-static.obj$theta.true)^2 + static.obj$theta.se^2
  points(static.obj$theta.true, mse.stat, col="gray65", pch=23, cex=.5)
  lines(dyn.obj$theta.true, predict(loess(mse.dyn ~ dyn.obj$theta.true, span=.5, degree=1)), type="l", lwd=2, col="black", lty=2)
  lines(static.obj$theta.true, predict(loess(mse.stat ~ static.obj$theta.true, span=.5, degree=1)), type="l", lwd=2, col="black")
    rect(-3.35, y.max-.075, 3.35, y.max+100, col="gray80", lwd=1)
  text(0, y.max+.02, this.title, cex=1.25)
}
pdf(file="~/Dropbox/Adaptive Surveys/PA Submission/EmpResults.pdf", width=6, height=6)
par(bg="white", mgp=c(1,0,0), tcl=0, mar=c(0,2,2.1,0), xaxt="n", yaxt="s", bty="o")
layout(matrix(1:6, nrow=3, ncol=2), height=c(3,3,1.3))
results.plotter(dyn.obj=dynRes3, static.obj=statRes3)
par(mar=c(2.1,2,0,0), xaxt="s")
results.plotter(dyn.obj=dynRes7, static.obj=statRes7, this.title="N=7")
par(bty="n", xaxt="n", yaxt="n", mar=c(0,0,0,0), mgp=c(0,0,0))
plot(NULL, xlim=c(0,1), ylim=c(0,1),  ylab="", xlab="")
legend(.5,.9, c("Static scale est.", "CAT scale est.", "Loess for static", "Loess for CAT"), pch=c(23,24, 26, 26), col=c("gray65", "gray90", "black", "black"),  lty=c(0,0,1,2), lwd=c(1,1,2,2), cex=1.3)
par(mar=c(0,0,2.1,2), xaxt="n", yaxt="n", bty="o")
results.plotter(dyn.obj=dynRes5, static.obj=statRes5, this.title="N=5")
par(mar=c(2.1,0,0,2), xaxt="s", mgp=c(1,0,0))
results.plotter(dyn.obj=dynRes10, static.obj=statRes10, this.title="N=10")
dev.off()





my.wilcox.test <- function(dyn.obj=dynRes3, stat.obj=statRes3){
  mse.dyn <- (dyn.obj$theta.est-dyn.obj$theta.true)^2 + dyn.obj$theta.se^2
  mse.stat <- (static.obj$theta.est-static.obj$theta.true)^2 + static.obj$theta.se^2
  return(wilcox.test(mse.dyn, mse.stat))
}
my.wilcox.test(dynRes3, statRes3)
my.wilcox.test(dynRes5, statRes5)
my.wilcox.test(dynRes7, statRes7)
my.wilcox.test(dynRes10, statRes10)







####
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


dynC <- NULL
dynC <- (c(dynC,sum(abs(dynRes3$theta.est-dynRes3$theta.true))))
dynC <- (c(dynC,sum(abs(dynRes4$theta.est-dynRes4$theta.true))))
dynC <- (c(dynC,sum(abs(dynRes5$theta.est-dynRes3$theta.true))))
dynC <- (c(dynC,sum(abs(dynRes6$theta.est-dynRes3$theta.true))))
dynC <- (c(dynC,sum(abs(dynRes7$theta.est-dynRes3$theta.true))))
dynC <- (c(dynC,sum(abs(dynRes8$theta.est-dynRes3$theta.true))))
dynC <- (c(dynC,sum(abs(dynRes9$theta.est-dynRes3$theta.true))))
dynC <- (c(dynC,sum(abs(dynRes10$theta.est-dynRes3$theta.true))))

statC <- NULL
statC <- (c(statC,sum(abs(statRes3$theta.est-statRes3$theta.true))))
statC <- (c(statC,sum(abs(statRes4$theta.est-statRes4$theta.true))))
statC <- (c(statC,sum(abs(statRes5$theta.est-statRes3$theta.true))))
statC <- (c(statC,sum(abs(statRes6$theta.est-statRes3$theta.true))))
statC <- (c(statC,sum(abs(statRes7$theta.est-statRes3$theta.true))))
statC <- (c(statC,sum(abs(statRes8$theta.est-statRes3$theta.true))))
statC <- (c(statC,sum(abs(statRes9$theta.est-statRes3$theta.true))))
statC <- (c(statC,sum(abs(statRes10$theta.est-statRes3$theta.true))))



dynD <- NULL
dynD <- (c(dynD,median(dynRes3$theta.se^2)))
dynD <- (c(dynD,median(dynRes4$theta.se^2)))
dynD <- (c(dynD,median(dynRes5$theta.se^2)))
dynD <- (c(dynD,median(dynRes6$theta.se^2)))
dynD <- (c(dynD,median(dynRes7$theta.se^2)))
dynD <- (c(dynD,median(dynRes8$theta.se^2)))
dynD <- (c(dynD,median(dynRes9$theta.se^2)))
dynD <- (c(dynD,median(dynRes10$theta.se^2)))

statD<- NULL
statD <- (c(statD,median(statRes3$theta.se^2)))
statD <- (c(statD,median(statRes4$theta.se^2)))
statD <- (c(statD,median(statRes5$theta.se^2)))
statD <- (c(statD,median(statRes6$theta.se^2)))
statD <- (c(statD,median(statRes7$theta.se^2)))
statD <- (c(statD,median(statRes8$theta.se^2)))
statD <- (c(statD,median(statRes9$theta.se^2)))
statD <- (c(statD,median(statRes10$theta.se^2)))




pdf(file="~/Dropbox/Adaptive Surveys/PA Submission/Comparison.pdf", width=7, height=7)
par(bg="white", mgp=c(1,0,0), tcl=0, mar=c(2.1,2,2.1,1), xaxt="s", yaxt="s", bty="o", mfrow=c(2,2))
plot(3:10, dyn, pch=24, col="gray40", type="b", xlab="Scale length", ylab="Median squared error", ylim=c(0.2, .62), main="Mean squared error", lty=2)
points(3:10, stat, pch=23, col="black", type="b")
#abline(h=stat, col="pink")
legend(7,.62, c("Static scale", "CAT scale"), pch=c(23,24), col=c("black", "gray40"),  lty=c(1,2), cex=.75)
plot(3:10, dynB, pch=24, col="gray40", type="b", xlab="Scale length", ylab="Median absolute bias", ylim=c(0.20, .4), main="Median absolute bias", lty=2) #
points(3:10, statB, pch=23, col="black", type="b")
#abline(h=statB, col="pink")
legend(7,.4, c("Static scale", "CAT scale"), pch=c(23,24), col=c("black", "gray40"),  lty=c(1,2), cex=.75)
plot(3:10, dynC, pch=24, col="gray40", type="b", xlab="Scale length", ylab="Total absolute bias", ylim=c(110, 200), main="Total absolute bias", lty=2) #
points(3:10, statC, pch=23, col="black", type="b")
#abline(h=statB, col="pink")
legend(7,200, c("Static scale", "CAT scale"), pch=c(23,24), col=c("black", "gray40"),  lty=c(1,2), cex=.75)
plot(3:10, dynD, pch=24, col="gray40", type="b", xlab="Scale length", ylab="Median posterior variance", ylim=c(0.1, .45), main="Median posterior variance", lty=2) #
points(3:10, statD, pch=23, col="black", type="b")
#abline(h=statB, col="pink")
legend(7,.45, c("Static scale", "CAT scale"), pch=c(23,24), col=c("black", "gray40"),  lty=c(1,2), cex=.75)
dev.off()



## Some population-level statistics
var(dynRes5$theta.est)
var(statRes5$theta.est)


##missing extremes
##upper
.table <- table(dynRes5$theta.est==max(dynRes5$theta.est))
.table
.table/sum(.table)
.table <- table(statRes5$theta.est==max(statRes5$theta.est))
.table
.table/sum(.table)

##lower
.table <- table(dynRes5$theta.est==min(dynRes5$theta.est))
.table
.table/sum(.table)
.table <- table(statRes5$theta.est==min(statRes5$theta.est))
.table
.table/sum(.table)


## Here is where we can run some regressions or something
library(car)


# Re-scale the political knowledge scale so it is between 0 and 1
dynOut <- cbind(dynOut,dynRes5$theta.est)
statOut <- cbind(statOut, statRes5$theta.est)
colnames(dynOut)[16] <- colnames(statOut)[16] <- "know"
statOut$know <- (statOut$know-min(statOut$know))/(max(statOut$know)-min(statOut$know))
dynOut$know <- (dynOut$know-min(dynOut$know))/(max(dynOut$know)-min(dynOut$know))
hist(statOut$know)

for (i in c(1685:1694, 1698, 1699)){
 this <- paste("question_", i, sep="")
 dynOut[,this] <-  recode(dynOut[,this], "2=0")
 statOut[,this] <-  recode(statOut[,this], "2=0")
  
}


# Make an index of 
bothOut <- rbind(dynOut, statOut)

these.ones <- c(1:10)
ltm.part <- ltm(bothOut[,these.ones]~z1, IRT.param=TRUE)

scores <- factor.scores(ltm.part, dynOut[,these.ones])
dynOut$partScore <- scores$score.dat$z1

scores <- factor.scores(ltm.part, statOut[,these.ones])
statOut$partScore <- scores$score.dat$z1

#sum
dynOut$sumPart <- rowSums(dynOut[,1:10])
statOut$sumPart <- rowSums(statOut[,1:10])


## ehh..not super strong
summary(lm(sumPart~know, data=dynOut))
summary(lm(sumPart~know, data=statOut))

summary(lm(partScore~know, data=dynOut))
summary(lm(partScore~know, data=statOut))



############## These ones in the paper

summary(lm(question_1695~know, data=dynOut))
summary(lm(question_1695~know, data=statOut))

summary(lm(question_1696~know, data=dynOut))
summary(lm(question_1696~know, data=statOut))

summary(lm(question_1697~know, data=dynOut))
summary(lm(question_1697~know, data=statOut))





