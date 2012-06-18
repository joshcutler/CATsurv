#Use the new library
setwd("~/GitHub/CatSurv/")
source("CATSurv.R")
library("multicore")
library("foreach")
library("doMC")
library("plyr")
library("compiler")

#results10 <- sim.run(items=60, recipients=1000, fixed.scale=seq(3, 57, by=6), n.cores=24)

seq(3, 57, by=6)

###
#' Simulation Run
#'
#' The high end function for running a single simulation
sim.run <- function(items=20,
                    recipients=10, 
                    guessing.param=0,
                    discrimination.param1=100,
                    discrimination.param2=50,
                    fixed.scale=c(NA),
                    CAT.prior=c(0, 1.75), 
                    multicore=T,
                    n.cores=4) {
  
  ##Set this up to run in parallel
  if (multicore) {
    registerDoMC(cores=n.cores)
  }
  
  ##Setup values question bank
  theta.true = seq(-3, 3, length.out=recipients) #Ability parameter ar spread evenly through the plausible space
  discrimination = rgamma(items, discrimination.param1, discrimination.param2) # gamma distribution
  difficulty = seq(-3, 3, length.out=items) # difficult parameters are distributed evenly in the space
  guessing = runif(items, 0, guessing.param) # Setting guessing parameters to 0
  
  questions = data.frame(difficulty=difficulty, discrimination=discrimination, guessing=guessing, answers=c(NA))
  
  #Run the fixed battery
  fixed.results = sim.fixed.run(questions, theta.true=theta.true, questions.to.ask=fixed.scale, CAT.prior=CAT.prior)
  fixed.bias = fixed.results$theta.est - fixed.results$theta.true
  fixed.mse = fixed.bias^2 + fixed.results$theta.se^2
  
  #Run the dynamic battery
  dynamic.results = sim.dynamic.run(questions, theta.true, CAT.prior, n.questions=length(fixed.scale))
  dynamic.bias = dynamic.results$theta.est - dynamic.results$theta.true
  dynamic.mse = dynamic.bias^2 + dynamic.results$theta.se^2
  
  #Store results and return
  results = list()
  results$fixed.results = fixed.results
  results$dynamic.results = dynamic.results
  results$fixed.bias = fixed.bias
  results$dynamic.bias = dynamic.bias
  results$fixed.mse = fixed.mse
  results$dynamic.mse = dynamic.mse
  
  return(results)
}

###
#' Simulation for a fixed battery
#'
#' Run all respondents through a fixed battery
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
      return(data.frame(theta.true=theta.true.ind, theta.est=theta.est, theta.se=theta.se))
    }
    ) # end ldply
  
  return(results)
}

###
#' Simulation for a dynamic battery
#'
#' Run all respondents through a dynamic battey of length 'n.questions'
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
      
      return(data.frame(theta.true=theta.true.ind, theta.est=theta.est, theta.se=theta.se))
    },
    .parallel=TRUE) # end ldply
  
  return(results)
}









########## Load all stored data and make plot
rm(list=ls())
setwd("/users/jmontgomery/github/CATsurv/")
load("results5")
load("results3")
load("results7")
load("results9")
load("results10")



###
#' Plotter function for simulations
sim.plot <- function(sim.results,
                     y.name="bias",
                     y.min=0,
                     y.max=10,
                     y.lab="Squared error",
                     this.title="N=3")
{
  fixed = sim.results$fixed.results
  dynamic = sim.results$dynamic.results
  plot(NULL, xlim=c(-3.1,3.1), ylim=c(y.min, y.max), ylab=y.lab, xlab=expression(paste("True values of ", theta))  )
  points(fixed$theta.true, sim.results[[paste('fixed.', y.name, sep='')]], col="black", pch=18, cex=.6)
  points(dynamic$theta.true, sim.results[[paste('dynamic.', y.name, sep='')]], col="gray60", pch=18, cex=.6)
  col.coder = abs(sim.results[[paste('fixed.', y.name, sep='')]]) > abs(sim.results[[paste('dynamic.', y.name, sep='')]])
  colors = rep("pink", length(fixed$theta.true))
  colors[col.coder] = "skyblue"
#  points(fixed$theta.true, predict(loess(sim.results[[paste('fixed.', y.name, sep='')]] ~ fixed$theta.true, span=.5, degree=1)), type="l", lwd=2, col="red")
#  points(dynamic$theta.true, predict(loess(sim.results[[paste('dynamic.', y.name, sep='')]] ~ dynamic$theta.true, span=.5, degree=1)), type="l", lwd=2, col="blue", lty=2)
  rect(-3.35, y.max-.075, 3.35, y.max+100, col="gray80", lwd=1)
  text(0, y.max, this.title, cex=1.25)
}
#system.time(results5 <- sim.run(items=60, recipients=1000, fixed.scale=seq(from=10,to=50, length.out=5), n.cores=32))
#sim.plot(results5, "bias", y.min=-0.5, y.max=0.5)
#sim.plot(results5, "mse", y.min=0, y.max=1)
#save(results5, file="results5")
pdf(file="/Users/jmontgomery/dropbox/Adaptive Surveys/TexFiles/MSE4.pdf", width=6, height=6)
par(bg="white", mgp=c(1,0,0), tcl=0, mar=c(0,2,2.1,0), xaxt="n", yaxt="s", bty="o")
layout(matrix(1:6, nrow=3, ncol=2), height=c(3,3,1.3))
sim.plot(results3, "mse", y.min=0, y.max=1.6)
par(mar=c(2.1,2,0,0), xaxt="s")
sim.plot(results7, "mse", y.min=0, y.max=1.6, this.title="N=7")
par(bty="n", xaxt="n", yaxt="n", mar=c(0,0,0,0), mgp=c(0,0,0))
plot(NULL, xlim=c(0,1), ylim=c(0,1),  ylab="", xlab="")
legend(.5,.9, c("Static scale est.", "CAT scale est."), col=c("black", "gray60"),  lty=c(1,1), lwd=c(1.9, 1.9), cex=1.1)
par(mar=c(0,0,2.1,2), xaxt="n", yaxt="n", bty="o")
sim.plot(results5, "mse", y.min=0, y.max=1.6, this.title="N=5")
par(mar=c(2.1,0,0,2), xaxt="s", mgp=c(1,0,0))
sim.plot(results10, "mse", y.min=0, y.max=1.6, this.title="N=10")
dev.off()
