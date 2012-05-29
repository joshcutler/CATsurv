setwd("/Users/jacobmontgomery/GitHub/CATSurv/CalibrationMay2012/")
source("../CATSurv.R")
library("multicore")
library("foreach")
library("doMC")
library("plyr")
library("compiler")


## Read in data/set up item bank
  itemParams <- read.csv(file="ItemParams.csv", header=TRUE, as.is=TRUE, row.names=1)
  nItems <- nrow(itemParams)
  guessing <- rep(0, nItems)
  questions <- data.frame(difficulty=itemParams$difficulty, discrimination=itemParams$discrimination,
                          guessing=guessing,
                          answers=c(NA))
  cat <- new("CATsurv", questions=questions, priorParams=c(0, 1.75))
  evalItems <- nextItem(cat)$all.estimates
  evalItems <- evalItems[order(evalItems$difficulty),]
  with(evalItems, plot(difficulty, epv, type="b"))
  

drawEPV = function (theta.hat=1, 
                    prior.dist="norm",
                    priorPar=c(0, 1),
                    parInt=c(-4, 4, 33),
                    difficulty=seq(-3, 3, .1),
                    discrimination=c(1),
                    guessing=c(0),
                    inattention=c(1)) {
  
  questions = data.frame(a=discrimination, b=difficulty, c=guessing, d=inattention)
  items = createItemBank(items=questions)
  
  EPVs_correct = rep(NA, length(difficulty))
  EPVs_incorrect = rep(NA, length(difficulty))
  for (i in 1:length(difficulty)) {
    EPVs_correct[i] = EPV(items, i, 1, theta.hat, questions, priorDist=prior.dist)
    EPVs_incorrect[i] = EPV(items, i, 0, theta.hat, questions, priorDist=prior.dist)
  }
  EPVS_joint = EPVs_correct + EPVs_incorrect
  
  return(list(correct=EPVs_correct, incorrect=EPVs_incorrect, difficulty=difficulty))
}

#Graphs from paper with new column
library(ltm)
library(plyr)
library(combinat)
#library(catR)
library(snow)


estimate.theta.one <- function(item.nums, respond.num, priorVar=1){
  this.answers <- answers[respond.num,item.nums]
  this.params <- matrix(item.params[item.nums,], ncol=4)
  this.EAP <- thetaEst(this.params, this.answers, method="EAP", D=1.7, priorPar=c(0,priorVar), parInt=this.parInt)
  this.PV <- eapSem(this.EAP, this.params, this.answers, priorPar=c(0,priorVar), D=1.7, lower=this.parInt[1], upper=this.parInt[2], nqp=this.parInt[3])^2
  return(list(est=this.EAP, var=this.PV, true=theta.true[respond.num]))
}

sim.dynamic <- function(respond.num, num.red){
  asked.items <- NULL
  for(i in 1:num.red){
    theta.hat <- 0
    if(!is.null(asked.items)){
      theta.hat <- thetaEst(matrix(item.params[asked.items,1:4],ncol=4), this.answers, method="EAP", D=1.7,  priorPar=c(0,priorVar), parInt=this.parInt)
    }
    new.item.temp <- nextItem(item.bank, theta.hat, criterion="MEPV", out=asked.items, priorDist="norm", priorPar=c(0,priorVar), D=1.7, parInt=this.parInt)
    new.item <- new.item.temp$item
    asked.items<-c(asked.items, new.item)
    this.answers <- answers[respond.num, asked.items]
  }
  print(asked.items)
  this.est <- estimate.theta.one(item.nums=asked.items, respond.num=respond.num)
  this.est$items <- asked.items
  return(this.est)
}

num.items=60
num.sample=100
num.red=5
disc.hyper.param1=50
disc.hyper.param2=25
guess.hyper.param=.1
preset.fixed.scale=c(10,20,30,40,50)


num.sample<<-num.sample
theta.true <<- seq(-3,3,length.out=num.sample)
disc <- rgamma(num.items, disc.hyper.param1, disc.hyper.param2)
diff <- seq(-3, 3, length.out=num.items)
guess <- runif(num.items, 0, guess.hyper.param)
d <- rep(1, num.items)

item.params <<- cbind(disc, diff, guess, d)
item.bank <<- createItemBank(item.params, model="3PL")
answer.one <- function(x){
  (item.params[,"guess"]  + (1 - item.params[,"guess"])  / (1 + exp(1.7*-item.params[,"disc"]*(x-item.params[,"diff"])))>.5)*1#runif(num.items))*1
}
answers <<- data.frame(t(unlist(sapply(theta.true, answer.one))))
rownames(answers) <-paste("Respond", 1:num.sample, sep="")
colnames(answers) <- paste("Q", 1:num.items, sep="")

priorVar <- 1.5
this.parInt <- c(-13,13,120)
resp.num=34
estimate.theta.one(item.nums=preset.fixed.scale, respond.num=resp.num)
sim.dynamic(resp.num,5)

my.curve <- function(x,it.num=33){
  item.params[it.num,"guess"]+(1 - item.params[it.num,"guess"])/(1 + exp(1.7*-item.params[it.num,"disc"]*(x-item.params[it.num,"diff"])))
}

x <- seq(-4,4, length.out=500)

this.items <- c(28,22, 19, 41, 27)

post.plotter <- function(x, item.nums){
  lik <- function(theta, item.nums){
    output <- 1
    for (i in 1:length(item.nums)){
      a <- item.params[item.nums[i],"disc"]
      b <- item.params[item.nums[i],"diff"]
      c <- item.params[item.nums[i],"guess"]
      p.i <- c + (1-c)*exp(1.7*a*(theta-b))/(1+exp(1.7*a*(theta-b)))
      q.i <- 1-p.i
      this.ans <- answers[resp.num,item.nums[i]]
      output <- output*p.i^this.ans*q.i^(1-this.ans)
      #  print(output)
    }
    output*dnorm(theta, 0, sqrt(priorVar))
  }
  thetas <- x
  height <- lik(thetas, item.nums=item.nums)
  constant <- integrate.xy(thetas, height)
  height <- height/constant
  height
  #lines(thetas, height, type="l")
}
plot(x, post.plotter(x=x, item.nums=c(28,2, 24, 23, 19, 20)))

my.plotter3 <- function(this.items, resp.num){
  par(bg="white", mgp=c(1,0,0), tcl=0, mar=c(0,0,0,0), xaxt="n", yaxt="n", bty="o")
  layout(matrix(1:15, nrow=5, ncol=3), width=c(3,1.5))
  my.line.plotter <- function(it.num=33){
    plot(NULL, xlim=c(-3,3), ylim=c(-0.1,1.1), xlab="", ylab="")
    abline(h=c(0,1), col="gray", lwd=2)
    lines(x,my.curve(x=x, it.num=it.num), type="l", lwd=2)
    segments(theta.true[resp.num],0,theta.true[resp.num],1, lty=2)
    text(theta.true[resp.num],1.06, expression(theta), cex=1)
    abline(h=.5, lty=3, col="gray")
    text(2.7,1.065, expression(paste("Pr(", y[ij], "=1", "|", theta, ")=1")))
    text(2.7,.07, expression(paste("Pr(", y[ij], "=1", "|", theta, ")=0")))
    text(seq(-3,3,by=1), -.06, seq(-3,3,by=1), cex=.9, col="gray20")
    rect(-4, -5, -2.8, 5, col="bisque1")
  }
  
  #Draw left graphs
  for (i in 1:5){
    my.line.plotter(this.items[i])
    par(srt=90)
    text(-3,.5, paste("Item", i), cex=1.4)
    par(srt=0)
  }
  
  #Draw theta hat/theta graphs
  for (i in 1:5){
    this.est <- estimate.theta.one(this.items[1:i], resp.num, priorVar=priorVar)
    y <- post.plotter(x=x, item.nums=this.items[1:i])
    y.top.adj <- 1.25#max(c(1.1, max(y)+.1))
    plot(NULL,, xlim=c(-4.6, 3), ylim=c(-0.1,y.top.adj))
    lines(x,y, type="l", lwd=2)
    abline(h=c(0,y.top.adj-.1), col="gray", lwd=2)
    segments(theta.true[resp.num],0,theta.true[resp.num],y.top.adj-.1, lty=2)
    text(theta.true[resp.num], y.top.adj-.045, expression(theta), cex=1)
    text(this.est$est, y.top.adj-.025, expression(widehat(theta)), cex=1)
    text(seq(-3,3,by=1), -.06, seq(-3,3,by=1), cex=.9, col="gray20")
    rect(-6, -5, -4, 5, col="bisque1")
    par(srt=90)
    text(-4.5, .5, expression(paste("p(", theta, "|", bold(y), ")")), cex=1.4)
    par(srt=0)
  }
  
  #Draw third graph
  for (i in 1:5) {
    this.est <- estimate.theta.one(this.items[1:i], resp.num, priorVar=priorVar)
    
    epv = drawEPV(theta.hat=this.est$est)
    y = epv$correct + epv$incorrect
    x = epv$difficulty
    ytop = max(y)
    
    plot(x, y, xlim=c(-3, 3), type="n")
    lines(x, y, type="l", lwd=2)
    abline(h=c(0, .015), col="gray", lwd=2)
    segments(theta.true[resp.num], 0, theta.true[resp.num], ytop - .0005, lty=2)
    text(theta.true[resp.num], ytop - .0002, expression(theta), cex=1)
    text(this.est$est, ytop - .0002, expression(widehat(theta)), cex=1)
    text(seq(-3, 3, by=1), -.95, seq(-3, 3, by=1), cex=.9, col="gray20")
    rect(-6, -5, -4, 5, col="bisque1")
    par(srt=90)
    text(-4.5, .5, expression(paste("EPV")), cex=1.4)
    par(srt=0)
  }
}
my.plotter3(this.items, resp.num)


#my.plotter3(preset.fixed.scale, resp.num)
