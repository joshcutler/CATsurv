#' Title
#'
#' Definition
#'
#' @param cat
#' @param item
#' @param theta.hat 
#' @param D
#' @param lowerBound
#' @param upperBound
#' @param quadPoints
#'
#' @return An object of class CATsurv containing
#'  \item{ }{ }
#'  
#' @author Josh W. Cutler and Jacob M. Montgomery

#' @export
setGeneric("expectedPV", function(cat, item, theta.est, D=1, lowerBound=-4, upperBound=4, quadPoints=33){standardGeneric("expectedPV")})

#' @export
setMethod(f="expectedPV", signature=class.name, definition=function(cat, item, theta.est, D=1, lowerBound=-4, upperBound=4, quadPoints=33) {
  if (cat@poly) {
    row.name = item
    thetas = rep(NA, length(cat@difficulties[row.name]) + 1)
    variances = rep(NA, length(cat@difficulties[row.name]) + 1)
    
    for (i in 1:(length(difficulties[[row.name]]))) {
      cat@questions[row.name, 'answers'] = i
      thetas[i] = estimateTheta(cat, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
      variances[i] = estimateSE(cat, thetas[i], D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)^2
    }
    cat@questions[row.name, 'answers'] = NA
    
    this.question.cdf = three.pl(cat, theta.est, cat@difficulties[[row.name]], cat@questions[row.name, 'discrimination'], cat@questions[row.name, 'guessing'], D)
    this.question.cdf = c(1, this.question.cdf, 0)
    this.question.pdf = c()
    for (i in 2:(length(this.question.cdf))) {
      this.question.pdf[i-1] = this.question.cdf[i-1] - this.question.cdf[i]
    }
    
    return (sum(variances * this.question.pdf))
  } else {
    prob.correct = three.pl(cat, theta.est, cat@questions[item,]$difficulty, cat@questions[item,]$discrimination, cat@questions[item,]$guessing, D)
    prob.incorrect = 1 - prob.correct
    
    old_val = cat@questions[item, 'answers']
    
    cat@questions[item, 'answers'] = 1
    theta.correct = estimateTheta(cat, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
    variance.correct = estimateSE(cat, theta.correct, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)^2
    
    cat@questions[item, 'answers'] = 0
    theta.incorrect = estimateTheta(cat, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
    variance.incorrect = estimateSE(cat, theta.incorrect, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)^2
    
    cat@questions[item, 'answers'] = if (is.null(old_val) || is.na(old_val)) NA else old_val
    
    return(prob.correct*variance.correct + prob.incorrect*variance.incorrect)
  }
})