setwd("/users/jmontgomery/Github/CATsurv/")

##Use the new library
source("./CATSurv.R")
library(plyr)



## Setup values question bank
  theta.true = -1
  discrimination = rgamma(60, 50, 25) # gamma distribution
  difficulty = seq(-3, 3, length.out=60) # difficult parameters are distributed evenly in the space
  guessing = runif(60, 0, 0) # Setting guessing parameters to 0
  questions = data.frame(difficulty=difficulty, discrimination=discrimination, guessing=guessing, answers=c(NA))


##
sim.fixed.run <- function(questions,
                          theta.true=c(),
                          questions.to.ask=c(),
                          CAT.prior=c(0, 1.75),
                          D=1,
                          correctness.threshold=0.5)
{
  
  results = ldply(theta.true, # this could be made much faster by vectorizing.  No need to be a loop or even an ply.  
    .fun <- function(theta.true.ind) {
      cat = new("CATsurv", questions=questions, priorParams=CAT.prior)
      question = questions[questions.to.ask,]
      correct.answer.prob = three.pl(cat, theta.true.ind, question$difficulty, question$discrimination, question$guessing, D)
      correct.answer = (correct.answer.prob >= correctness.threshold) * 1
      cat = storeAnswer(cat, questions.to.ask, correct.answer)
      theta.est = estimateTheta(cat)
      theta.se = estimateSE(cat, theta.est)
      return(data.frame(theta.true=theta.true.ind, theta.est=theta.est, theta.se=theta.se, answer=correct.answer[length(correct.answer)]))
    }
    ) # end ldply
  
  return(results)
}
  
## Run the fixed battery

fsRes <- function(x, fixed.scale=c(10,20,30,40,50), CAT.prior=c(0,1.75)){
  fixed.results = sim.fixed.run(questions, theta.true=theta.true,
    questions.to.ask=fixed.scale[1:x],
    CAT.prior=CAT.prior)
  out <- NULL
  out <- c(out, fixed.results$theta.est, fixed.results$answer)
  out <- c(out, unlist(questions[fixed.scale[x],1:2]))
  names(out) <- c("thetaHat", "answer", "difficulty", "discrimination")
  out
}
forFixed <- matrix(NA, nrow=5, ncol=4)
for (i in 1:5){
  forFixed[i,] <- fsRes(i)
}
forFixed
colnames(forFixed) <- c("thetaHat", "answer", "difficulty", "discrimination")



#########
sim.dynamic.run <- function(questions,
                            theta.true=c(),
                            CAT.prior=c(0, 1.75),
                            D=1,
                            correctness.threshold=.5,
                            n.questions=NA)
{
  if (is.na(n.questions)) {
    n.questions = nrow(questions)
  }
  results = ldply(theta.true,
    .fun <- function(theta.true.ind) { # perhaps not all of these commands need be run within the ply
      cat = new("CATsurv", questions=questions, priorParams=CAT.prior)
        for(i in 1:n.questions) {
          question.id = nextItem(cat)$next.item
           question = questions[question.id,]
          correct.answer.prob = three.pl(cat, theta.true.ind, question$difficulty, question$discrimination, question$guessing, D)
          correct.answer = (correct.answer.prob >= correctness.threshold)*1
          cat = storeAnswer(cat, question.id, correct.answer)
        }
      theta.est = estimateTheta(cat)
      theta.se = estimateSE(cat, theta.est)
      return(data.frame(theta.est=theta.est, answer=correct.answer, difficulty=questions[question.id,"difficulty"],discrimination=questions[question.id,"discrimination"]))
    },
    .parallel=FALSE) # end ldply
  return(results)
}
## Run the dynamic battery

forDyn <- matrix(NA, nrow=5, ncol=4)
for (i in 1:5){
  forDyn[i,] <- unlist(sim.dynamic.run(questions, theta.true, CAT.prior, n.questions=i))
}
forDyn
colnames(forDyn) <- c("thetaHat", "answer", "difficulty", "discrimination")


### Plotting utility functions ###

my.curve <- function(x,it.num=33, it.obj){
  1/(1 + exp(1*-it.obj[it.num,"discrimination"]*(x-it.obj[it.num,"difficulty"]))) #
}


## posterior plotter







post.plotter <- function(x, item.nums, it.obj){
  lik <- function(theta, item.nums, it.obj, ans){
    output <- 1
    for (i in 1:length(item.nums)){
      a <- it.obj[item.nums[i],"discrimination"]
      b <- it.obj[item.nums[i],"difficulty"]
      p.i <- exp(a*(theta-b))/(1+exp(a*(theta-b)))
      q.i <- 1-p.i
      this.ans <- ans[i]
      output <- output*p.i^this.ans*q.i^(1-this.ans)
    }
    output*dnorm(theta, 0, sqrt(1.75))
  }
  thetas <- x
  height <- lik(thetas, item.nums=item.nums, it.obj=it.obj, ans=it.obj[,"answer"])
  constant <- integrate.xy(thetas, height)
  height <- height/constant
  height
  #lines(thetas, height, type="l")
}


my.line.plotter <- function(item.num, it.obj){
  plot(NULL, xlim=c(-5,3.7), ylim=c(-0.1,1.1), xlab="", ylab="")
  abline(h=c(0,1), col="gray", lwd=2)
  lines(x,my.curve(x=x, it.num=item.num, it.obj=it.obj), type="l", lwd=2)
  segments(theta.true,0,theta.true,1, lty=2)
  text(theta.true,1.06, expression(theta), cex=1)
  abline(h=.5, lty=3, col="gray")
  text(2.7,1.065, expression(paste("Pr(", y[ij], "=1", "|", theta, ")=1")))
  text(2.7,.07, expression(paste("Pr(", y[ij], "=1", "|", theta, ")=0")))
  text(seq(-4,3,by=1), -.06, seq(-4,3,by=1), cex=.9, col="gray20")
  rect(-6, -0.15, -4.5, 1.15, col="gray80", lty=1, lwd=2)
}

pdf(file="/Users/jmontgomery/dropbox/Adaptive Surveys/TexFiles/ExDyn.pdf", width=5, height=6)
## Make dynamic battery plot
par(bg="white", mgp=c(1,0,0), tcl=0, mar=c(0,0,0,0), xaxt="n", yaxt="n", bty="o")
layout(matrix(1:10, nrow=5, ncol=2), width=c(2,2))
x <- seq(-5,4, length.out=500)
for(i in 1:5){
  my.line.plotter(i, it.obj=forDyn)
    par(srt=90)
    text(-4.95,.5, paste("Item", i), cex=1.4)
    par(srt=0)
}
for (i in 1:5){
  plot(x, post.plotter(x=x, item.nums=1:i, it.obj=forDyn), type="l", xlim=c(-5, 3.7), ylim=c(-.1, 1.1))
  abline(h=c(0,1), col="gray", lwd=2)
  segments(theta.true,0,theta.true,1, lty=2)
  text(theta.true,1.06, expression(theta), cex=1)
  text(seq(-4,3,by=1), -.06, seq(-4,3,by=1), cex=.9, col="gray20")
    rect(-6, -0.15, -4.5, 1.15, col="gray80", lty=1, lwd=2)
  par(srt=90)
    text(-4.95, .5, expression(paste("p(", theta, "|", bold(y), ")")), cex=1.4)
  par(srt=0)
}
box(which="outer", lwd=2)
dev.off()


# Fixed battery plot
pdf(file="/Users/jmontgomery/dropbox/Adaptive Surveys/TexFiles/ExFixed.pdf", width=5, height=6)
par(bg="white", mgp=c(1,0,0), tcl=0, mar=c(0,0,0,0), xaxt="n", yaxt="n", bty="o")
layout(matrix(1:10, nrow=5, ncol=2), width=c(2,2))
x <- seq(-5,4, length.out=500)
for(i in 1:5){
  my.line.plotter(i, it.obj=forFixed)
    par(srt=90)
    text(-4.95,.5, paste("Item", i), cex=1.4)
    par(srt=0)
}
for (i in 1:5){
  plot(x, post.plotter(x=x, item.nums=1:i, it.obj=forFixed), type="l",xlim=c(-5, 3.7), ylim=c(-.1, 1.1))
  abline(h=c(0,1), col="gray", lwd=2)
  segments(theta.true,0,theta.true,1, lty=2)
  text(theta.true,1.06, expression(theta), cex=1)
  text(seq(-4,3,by=1), -.06, seq(-4,3,by=1), cex=.9, col="gray20")
    rect(-6, -0.15, -4.5, 1.15, col="gray80", lty=1, lwd=2)
  par(srt=90)
    text(-4.95, .5, expression(paste("p(", theta, "|", bold(y), ")")), cex=1.4)
  par(srt=0)
}
dev.off()



## MEPV plot



mepv.plotter <- function(answers, make.plot=TRUE){
  nItems <- 60
  guessing <- rep(0, nItems)
  questions <- data.frame(difficulty=difficulty, discrimination=discrimination,
                          guessing=guessing, answers=answers)
  cat <- new("CATsurv", questions=questions, priorParams=c(0, 1.75))
  evalItems <- nextItem(cat)$all.estimates
  new.item <- nextItem(cat)$next.item
  evalItems <- evalItems[order(evalItems$difficulty),]
  if(make.plot){
  with(evalItems, plot(difficulty, epv, type="p", cex=.4, xlab="Difficulty", ylab="EPV"))
  with(evalItems, points(difficulty, epv, type="l", cex=.4))
  with(evalItems[as.character(new.item),], points(difficulty, epv, pch="X"))
#  abline(v=theta.true, lty=2)
  with(evalItems[as.character(new.item),], text(theta.true, epv, expression(theta)))
}
  return(item=new.item)
}


pdf(file="/Users/jmontgomery/dropbox/Adaptive Surveys/TexFiles/EPVExample.pdf")
par(mfrow=c(2,2),  mgp=c(1,0,0), tcl=0, mar=c(3,2,2,2))
answers <- rep(NA, 60)
for (i in 1:4){
  if(i==1){this <- mepv.plotter(answers, make.plot=TRUE)}
  if(i>1){this <- mepv.plotter(answers, make.plot=TRUE)}
  title(paste("Item", i, "selection"))
  answers[as.numeric(this)] <- forDyn[i,"answer"]
#  abline(v=forDyn[i,"thetaHat"], lty=2)
}
dev.off()

