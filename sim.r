#Use the new library
source("./CATSurv.R")
library("multicore")
library("foreach")
library("doMC")
library("plyr")

sim.run = function(items=20, recipients=10, 
                   guessing.param=0,
                   discrimination.param1=10,
                   discrimination.param2=5,
                   fixed.scale=c(NA),
                   CAT.prior=c(0, 1.75), 
                   multicore=T) {
  
  #Set this up to run in parallel
  if (multicore) {
    registerDoMC()
  }
  
  #Setup values question bank
  theta.true = seq(-3, 3, length.out=recipients)
  discrimination = rgamma(items, discrimination.param1, discrimination.param2)
  difficulty = seq(-3, 3, length.out=items)
  guessing = runif(items, 0, guessing.param)
  
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

sim.fixed.run = function(questions, theta.true=c(), questions.to.ask=c(), CAT.prior=c(0, 1.75), D=1, correctness.threshold=0.5) {
  
  results = ldply(theta.true, .fun=function(theta.true.ind) {
    cat = new("CATsurv", questions=questions, priorParams=CAT.prior)
    
    for(i in 1:length(questions.to.ask)) {
      question.id = questions.to.ask[i]
      
      question = questions[question.id,]
      correct.answer.prob = three.pl(cat, theta.true.ind, question$difficulty, question$discrimination, question$guessing, D)
      correct.answer = if (correct.answer.prob >= correctness.threshold) 1 else 0
      
      cat = storeAnswer(cat, question.id, correct.answer)
    }
    theta.est = estimateTheta(cat)
    theta.se = estimateSE(cat, theta.est)
    
    return(data.frame(theta.true=theta.true.ind, theta.est=theta.est, theta.se=theta.se))
  })
  
  return(results)
}

sim.dynamic.run = function(questions, theta.true=c(), CAT.prior=c(0, 1.75), D=1, correctness.threshold=.5, n.questions=NA) {
  
  if (is.na(n.questions)) {
    n.questions = nrow(questions)
  }
  
  results = ldply(theta.true, .fun=function(theta.true.ind) {
    cat = new("CATsurv", questions=questions, priorParams=CAT.prior)
    for(i in 1:n.questions) {
      question.id = nextItem(cat)$next.item
      
      question = questions[question.id,]
      correct.answer.prob = three.pl(cat, theta.true.ind, question$difficulty, question$discrimination, question$guessing, D)
      correct.answer = if (correct.answer.prob >= correctness.threshold) 1 else 0
      
      cat = storeAnswer(cat, question.id, correct.answer)
    }
    theta.est = estimateTheta(cat)
    theta.se = estimateSE(cat, theta.est)
    
    return(data.frame(theta.true=theta.true.ind, theta.est=theta.est, theta.se=theta.se))
  }, .parallel=TRUE)
  
  return(results)
}

sim.plot = function(sim.results, y.name="bias", y.min=0, y.max=10) {
  fixed = sim.results$fixed.results
  dynamic = sim.results$dynamic.results
  plot(NULL, xlim=c(-3.1,3.1), ylim=c(y.min, y.max), ylab=y.name, xlab=expression(paste("True values of ", theta))  )
  
  points(fixed$theta.true, sim.results[[paste('fixed.', y.name, sep='')]], col="pink", pch=2, cex=.5)
  points(dynamic$theta.true, sim.results[[paste('dynamic.', y.name, sep='')]], col="skyblue", pch=3, cex=.5)
  
  col.coder = abs(sim.results[[paste('fixed.', y.name, sep='')]]) > abs(sim.results[[paste('dynamic.', y.name, sep='')]])
  colors = rep("pink", length(fixed$theta.true))
  colors[col.coder] = "skyblue"
  #segments(fixed$theta.true, sim.results[[paste('fixed.', y.name, sep='')]], fixed$theta.true, sim.results[[paste('dynamic.', y.name, sep='')]], col=colors, cex=1)
  points(fixed$theta.true, predict(loess(sim.results[[paste('fixed.', y.name, sep='')]] ~ fixed$theta.true, span=.5, degree=1)), type="l", lwd=2, col="red")
  points(dynamic$theta.true, predict(loess(sim.results[[paste('dynamic.', y.name, sep='')]] ~ dynamic$theta.true, span=.5, degree=1)), type="l", lwd=2, col="blue", lty=2)
}

results = sim.run(items=60, recipients=1000, fixed.scale=seq(10, 50, by=5))
sim.plot(results, "bias", y.min=-0.5, y.max=0.5)
sim.plot(results, "mse", y.min=0, y.max=1)

