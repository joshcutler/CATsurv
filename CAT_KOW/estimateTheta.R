#' Computerized Adaptive Testing Survey Estimating Each Respondent's Position on the Latent Scale of Interest
#'
#' This function returns the expected \emph{a posteriori} (EAP) estimate of each respondent's popsition on the latent scale. 
#'
#' @param cat an object of class \code{CATsurv}.
#' @param D model parameter. \code{1} is for a logistic model and \code{1.702} for an approximation of the probit model. The default value is 1.
#' @param priorName the type of prior distribution for respondent's position on the latent scale of interest.
#' @param priorParams the parameters for prior distribution. The first element for mean value, the second for standard deviation, and the third for degree of freedom.
#' @param lowerBound the lower bound of the interval of the latent scale used in estimation. The default value is \code{-4}
#' @param upperBound the upper bound of the interval of the latent scale used in estimation. The default value is \code{4}
#' @param quadPoints desired number of points to be used in approximating integral. The default value is \code{33}.
#'
#' @return A vector consisting of the expected \emph{a posteriori} estimate for each respondent's position on the latent scale of interest. 
#'  
#' @details The expected \emph{a posteriori} (EAP) estimate of respondent \eqn{j}'s position on the latent scale is calculated as \deqn{\hat{\theta}_j^{(\text{EAP})}\equiv E(\theta_j|\mathbf{y}_{k-1j})=\frac{\int\theta_j\pi(\theta_j)L(\theta_j|\mathbf{y}_{k-1j}d\theta_j}{\int\pi(\theta_j)L(\theta_j|\mathbf{y}_{k-1j}d\theta_j}}.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname estimateTheta
#' @export
setGeneric("estimateTheta", function(cat, D=1, priorName=NULL, priorParams=NULL, lowerBound=-4, upperBound=4, quadPoints=33, ...){standardGeneric("estimateTheta")})

#' @export
setMethod(f="estimateTheta", signature=class.name, definition=function(cat, D=1, priorName=NULL, priorParams=NULL, lowerBound=-4, upperBound=4, quadPoints=33, ...) {
  X = seq(from=lowerBound, to=upperBound, length=quadPoints)
  applicable_rows = cat@questions[!is.na(cat@questions$answers), ]
  
  priorName = if (!is.null(priorName)) priorName else cat@priorName
  priorParams = if (!is.null(priorParams)) priorParams else cat@priorParams
  prior.values = prior(cat, X, priorName, priorParams)
  likelihood.values = rep(NA, times=length(X))
  
  for (i in 1:length(likelihood.values)) {
    likelihood.values[i] = likelihood(cat, X[i], applicable_rows, D)
  }
  
  results = integrate.xy(X, X*likelihood.values*prior.values) / integrate.xy(X, likelihood.values*prior.values)
  return(results)
})
