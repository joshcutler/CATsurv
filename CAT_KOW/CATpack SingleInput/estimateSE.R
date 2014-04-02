#' Computerized Adaptive Testing Survey Posterior Standard Error of Estimated Latent Trait Position Estimator
#'
#' This function estimates the standard error of an estimate of a respondent's expected \emph{a posteriori} (EAP) position on the latent scale.
#'
#' @param cat An object of class \code{CATsurv}
#' @param theta.hat A scalar value containing an estimate of a respondent's position on the latent trait.  Generally, this is the output of the \code{\link{estimateTheta}} funciton.
#' @param D A numeric value used as model parameter.  For logit models, set D=1.  For an approximation of the probit model, set D=1.702.  Defaults to D=1.  
#' @param priorName The type of prior distribution for respondent's position on the latent scale of interest.  
#' @param priorParams The parameters for the prior distribution specified by \code{priorName}.  The first parameter is the mean/location parameter, the second is the standard deviation/scale parameter, and the third (only used by the "t" distribution) is a degrees of freedom parameter.  
#' @param lowerBound The lower bound of the interval of the latent trait used in estimation.  Defaults to -4.
#' @param upperBound The upper bound of the interval of the latent trait used in estimation.  Defaults to 4.
#' @param quadPoints The number of points used in approximating the integral.  Defaults to 33.   
#'
#' @return The estimate of the standard error of the user supplied \code{theta.hat} 
#' @details The standard error of the expected \emph{a posteriori} (EAP) estimate of respondent \eqn{j}'s position on the latent scale is calculated as the square root of \deqn{E((\theta_j-\hat{\theta_j}^{(\text{EAP})})^2|\mathbf{y}_{k-1,j})=\frac{\int(\theta_j-\hat{\theta_j}^{(\text{EAP})})^2\pi(\theta_j)L(\theta_j|\mathbf{y}_{k-1,j}d\theta_j}{\int\pi(\theta_j)L(\theta_j|\mathbf{y}_{k-1,j})d\theta_j}}.
#'  
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname estimateSE
#' @export
setGeneric("estimateSE", function(cat, theta.hat,...){standardGeneric("estimateSE")})

#' @export
setMethod(f="estimateSE", signature=class.name, definition=function(cat, theta.hat...) {
  applicable_rows =   applicable_rows = which(!is.na(cat@answers))
  
  prior.values = prior(cat, cat@X, cat@priorName, cat@priorParams)
  likelihood.values = rep(NA, times=length(cat@X))
  for (i in 1:length(likelihood.values)) {
    likelihood.values[i] = likelihood(cat, cat@X[i], applicable_rows)
  }
  
  results = sqrt(integrate.xy(cat@X, (cat@X - theta.hat)^2*likelihood.values*prior.values) / integrate.xy(X, likelihood.values*prior.values))
  return(results)
})