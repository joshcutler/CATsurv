#' Computerized Adaptive Testing Survey Expected Posterior Variance Estimator
#'
#' This function estimates the expected posterior variance for a respondent's estimated position on the latent trait on an item which he/she has yet to answer based on a respondent's position on the latent trait from the items he/she has already answered.  
#'
#' @param cat an object of class \code{CATsurv}
#' @param item The question for which to estimate the expected posterior variance for a respondent with a latent trait estimate of theta.hat.  This should be the name of a row in the "questions" data-frame in the "questions" slot of a \code{CATsurv} object.
#' @param theta.est A scalar value containing an estimate of a respondent's position on the latent trait.  Generally, this is the output of the \code{\link{estimateTheta}} funciton.
#' @param D A numeric value used as model parameter.  For logit models, set D=1.  For an approximation of the probit model, set D=1.702.  Defaults to D=1. 
#' @param lowerBound The lower bound of the interval of the latent trait used in estimation.  Defaults to -4.
#' @param upperBound The upper bound of the interval of the latent trait used in estimation.  Defaults to 4.
#' @param quadPoints The number of points used in approximating the integral.  Defaults to 33. 
#'
#' @return The expected posterior variance for respondent \emph{j} on item \emph{k}.  
#' @details The expected posterior variance is calculated as \deqn{P(y^*_{kj}=1|\mathbf{y}_{k-1,j})\text{Var}(\theta_j|\mathbf{y}_{k-1,j},y^*_{kj}=1)+P(y^*_{kj}=0|\mathbf{y}_{k-1,j})\text{Var}(\theta_j|\mathbf{y}_{k-1,j},y^*_{kj}=0)}, where \eqn{y_{kj}^*} is a possible response to item \emph{k} by respondent \emph{j}.
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname expectedPV 
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