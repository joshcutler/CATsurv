rm(list=ls())

class.name = "CATsurv"
setClassUnion("numericORlist", c("numeric","list"))
setClassUnion("logicalORnumeric", c("numeric","logical"))

setClass("CATsurv",
         slots=list(
           guessing="numeric",
           discrimination="numeric",
           answers="logicalORnumeric",
           priorName="character",
           priorParams="numeric",
           lowerBound="numeric",
           upperBound="numeric",
           quadPoints="numeric",
           D="numeric",
           X="numeric",
           Theta.est="numeric",
           difficulty="numericORlist",
           poly="logical"
         ),
         prototype=prototype(
           priorName="normal",
           priorParams=c(0,1),
           lowerBound=-4.5,
           upperBound=4.5,
           quadPoints=43,
           D=1,
           poly=FALSE
         )
)


#' @export
setMethod("initialize", class.name, function(.Object, ...) {
  value = callNextMethod()
  value@X <- seq(from=value@lowerBound,to=value@upperBound,length=value@quadPoints)
  validObject(value)
  return(value)
})


## functions are orderd by their independence. 


## ltmCAT(generate CATsurv for dicho)
setGeneric("ltmCAT", function(data, object=NULL, ...){standardGeneric("ltmCAT")})

#' @export
setMethod(f="ltmCAT", signature="data.frame", 
          definition=function(data, object,...){
            if(!is.null(object)) if(class(object)!="CATsurv") stop("object is not class CATsurv")            
            fit <- tpm(data)
            coefficient <- coef(fit)
            answer <- rep(NA,nrow(coefficient))
            discrimination <- coefficient[,"Dscrmn"]
            difficulty <- coefficient[,"Dffclt"]
            guessing <- coefficient[,"Gussng"]
            names(difficulty) <- rownames(coefficient)
            if(is.null(object)){
              return(new("CATsurv", discrimination=discrimination, difficulty=difficulty, guessing=guessing, answers=answer))
            } else {
              object@discrimination <- discrimination
              object@difficulty <- difficulty
              object@guessing <- guessing
              object@answers <- answer
              return(object)
            }
          })


## grmCAT(generate CATsurv for poly)
setGeneric("grmCAT", function(data, object=NULL, ...){standardGeneric("grmCAT")})

#' @export
setMethod(f="grmCAT", signature="data.frame", 
          definition=function(data, object,...){
            if(!is.null(object)) if(class(object)!="CATsurv") stop("object is not class CATsurv")            
            fit <- grm(data=data)
            coefficient <- coef(fit)
            answer <- rep(NA,nrow(coefficient))
            discrimination <- coefficient[,"Dscrmn"]
            difficulty <- lapply(1:nrow(coefficient), function(i) coefficient[i,-ncol(coefficient)])
            names(difficulty) <- rownames(coefficient)
            if(is.null(object)){
              return(new("CATsurv", discrimination=discrimination, difficulty=difficulty, poly=TRUE, guessing=0, answers=answer))
            } else {
              object@discrimination <- discrimination
              object@difficulty <- difficulty
              object@poly <- TRUE
              object@guessing <- 0
              object@answers <- answer
              return(object)
            }
          })


# storeAnswer(totally independent)
setGeneric("storeAnswer", function(cat, item, answer){standardGeneric("storeAnswer")})

#' @export
setMethod(f="storeAnswer", signature=class.name, definition=function(cat, item, answer) {
  eval( 
    eval( 
      substitute( 
        expression(cat@answers[item] <<- answer) 
        ,env=parent.frame(1) ) 
    ) 
  ) 
})


## three.pl(used in likelihood)
setGeneric("three.pl", function(cat, theta, difficulty, discrimination, guessing){standardGeneric("three.pl")})

#' @export 
setMethod(f="three.pl", signature="CATsurv", definition=function(cat, theta, difficulty, discrimination, guessing) {
  if(cat@poly){ prob=1/(1+exp(-discrimination*theta+difficulty)) 
  } else {
    exp.portion = exp(cat@D*discrimination*(theta - difficulty))
    prob = guessing + (1 - guessing)*(exp.portion / (1 + exp.portion))}
})

## prior(used in estimateTheta, estimateSE)
setGeneric("prior", function(cat, values, name, params){standardGeneric("prior")})

#' @export
setMethod(f="prior", signature=class.name, definition=function(cat, values, name, params) {
  prior.value = switch(name,
                       normal = dnorm(values, params[1], params[2]),
                       cauchy = dcauchy(values, params[1], params[2]),
                       t = 1 / params[2] * dt((values - params[1]) / params[2], df=params[3])
  )
  return(prior.value)
})

## likelihood(used in estimateTheta, estimateSE)(using three.pl)
setGeneric("likelihood", function(cat, theta, items){standardGeneric("likelihood")})

#' @export
setMethod(f="likelihood", signature=class.name, definition=function(cat, theta, items) {
  if (cat@poly) {
    probabilities = c()
    L = 1
    for (question in items) {
      this.question.cdf = three.pl(cat, theta, cat@difficulty[[question]], cat@discrimination[question], cat@guessing[question])
      this.question.cdf = c(1, this.question.cdf, 0)
      this.question.pdf = c()
      for (i in 2:(length(this.question.cdf))) {
        this.question.pdf[i-1] = this.question.cdf[i-1] - this.question.cdf[i]
      }
      
      L = L * this.question.pdf[cat@answers[question]]
    }
  } else {
    probabilities = three.pl(cat, theta, cat@difficulty[items], cat@discrimination[items], cat@guessing[items])
    L = prod(probabilities^cat@answers[items] * (1 - probabilities)^(1 - cat@answers[items]))
  }
  return(L)
})


##estimateSE (used in expectedPV)(using prior, likelihood)
setGeneric("estimateSE", function(cat, theta.hat,...){standardGeneric("estimateSE")})

#' @export
setMethod(f="estimateSE", signature=class.name, definition=function(cat, theta.hat,...) {
  applicable_rows =   applicable_rows = which(!is.na(cat@answers))
  
  prior.values = prior(cat, cat@X, cat@priorName, cat@priorParams)
  likelihood.values = rep(NA, times=length(cat@X))
  for (i in 1:length(likelihood.values)) {
    likelihood.values[i] = likelihood(cat, cat@X[i], applicable_rows)
  }
  
  results = sqrt(integrate.xy(cat@X, (cat@X - theta.hat)^2*likelihood.values*prior.values) / integrate.xy(cat@X, likelihood.values*prior.values))
  return(results)
})

##estimateTheta (used in expectedPV)(using prior, likelihood)
setGeneric("estimateTheta", function(cat, ...){standardGeneric("estimateTheta")})

#' @export
setMethod(f="estimateTheta", signature=class.name, definition=function(cat,...) {
  applicable_rows = which(!is.na(cat@answers))
  
  prior.values = prior(cat, cat@X, cat@priorName, cat@priorParams)
  likelihood.values = rep(NA, times=length(cat@X))
  
  for (i in 1:length(likelihood.values)) {
    likelihood.values[i] = likelihood(cat, cat@X[i], applicable_rows)
  }
  
  results = integrate.xy(cat@X, cat@X*likelihood.values*prior.values) / integrate.xy(cat@X, likelihood.values*prior.values)
  return(results)
})


##expectedPV(used in nextItem)(using all the functions above)
setGeneric("expectedPV", function(cat, item){standardGeneric("expectedPV")})

#' @export
setMethod(f="expectedPV", signature=class.name, definition=function(cat, item) {
  if (cat@poly) {
    row.name = item
    thetas = rep(NA, length(cat@difficulty[row.name]) + 1)
    variances = rep(NA, length(cat@difficulty[row.name]) + 1)
    
    for (i in 1:(length(cat@difficulty[[row.name]])+1)) {
      cat@answers[row.name] = i
      thetas[i] = estimateTheta(cat)
      variances[i] = estimateSE(cat, thetas[i])^2
    }
    cat@answers[row.name] = NA
    
    this.question.cdf = three.pl(cat, cat@Theta.est, cat@difficulty[[row.name]], cat@discrimination[row.name], cat@guessing[row.name])
    this.question.cdf = c(1, this.question.cdf, 0)
    this.question.pdf = c()
    for (i in 2:(length(this.question.cdf))) {
      this.question.pdf[i-1] = this.question.cdf[i-1] - this.question.cdf[i]
    }
    
    return (sum(variances * this.question.pdf))
  } else {
    prob.correct = three.pl(cat, cat@Theta.est, cat@difficulty[item], cat@discrimination[item], cat@guessing[item])
    prob.incorrect = 1 - prob.correct
    
    old_val = cat@answers[item]
    
    cat@answers[item] = 1
    theta.correct = estimateTheta(cat)
    variance.correct = estimateSE(cat, theta.correct)^2
    
    cat@answers[item] = 0
    theta.incorrect = estimateTheta(cat)
    variance.incorrect = estimateSE(cat, theta.incorrect)^2
    
    cat@answers[item] = if (is.null(old_val) || is.na(old_val)) NA else old_val
    
    return(prob.correct*variance.correct + prob.incorrect*variance.incorrect)
  }
})


## nextItem(using all the functions above)
setGeneric("nextItem", function(cat){standardGeneric("nextItem")})

#' @export
setMethod(f="nextItem", signature=class.name, definition=function(cat) {
  available_questions = data.frame(questions=which(is.na(cat@answers)),epv=NA)
  
  cat@Theta.est = estimateTheta(cat)
  
  
  #available_questions$epv = NA
  for (i in 1:nrow(available_questions)) {
    available_questions[i,]$epv = expectedPV(cat, available_questions[i,]$questions)
  }
  
  next.item = available_questions[available_questions$epv == min(available_questions$epv, na.rm=TRUE), ]
  to.return = list(all.estimates=available_questions, next.item=row.names(next.item)[1])
  
  return(to.return)
})


library(sfsmisc)
library(ltm)
# dichotomous 
data(Abortion)
cat <- ltmCAT(Abortion)
nextItem(cat)

storeAnswer(cat,1,1)
cat@answers
nextItem(cat)

storeAnswer(cat,2,0)
cat@answers
nextItem(cat)


# polytomous
data(Environment)
cat <- grmCAT(Environment)
nextItem(cat)

storeAnswer(cat,4,2)
cat@answers
nextItem(cat)

storeAnswer(cat,3,1)
cat@answers
nextItem(cat)
  
