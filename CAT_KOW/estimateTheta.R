#' Title
#'
#' Definition
#'
#' @param cat
#' @param D 
#' @param priorName
#' @param priorParams
#' @param lowerBound
#' @param upperBound
#' @param quadPoints
#'
#' @return An object of class CATsurv containing
#'  \item{results}{ }
#'  
#' @author Josh W. Cutler and Jacob M. Montgomery

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