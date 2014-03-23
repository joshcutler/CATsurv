#' Title
#'
#' Definition
#'
#' @param cat
#' @param theta.est
#' @param D
#' @param lowerBound
#' @param upperBound
#' @param quadPoints 
#' @param answer
#'
#' @return An object of class CATsurv containing
#'  \item{next.item}{ }
#'  
#' @author Josh W. Cutler and Jacob M. Montgomery

#' @export
setGeneric("debugNextItem", function(cat, theta.est=NA, D=1, lowerBound=-4, upperBound=4, quadPoints=33){standardGeneric("debugNextItem")})

#' @export
setMethod(f="debugNextItem", signature=class.name, definition=function(cat, theta.est=NA, D=1, lowerBound=-4, upperBound=4, quadPoints=33) {
  if (is.na(theta.est)) {
    theta.est = estimateTheta(cat, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
  }
  
  next.item = nextItem(cat, theta.est, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
  
  plot(next.item$all.estimates$difficulty, next.item$all.estimates$epv, type="n", xlab="Difficulty", ylab="EPV")
  lines(next.item$all.estimates$difficulty, next.item$all.estimates$epv)
  segments(theta.est, 0, theta.est, 10, lty=2)
  points(next.item$all.estimates[next.item$next.item,]$difficulty, next.item$all.estimates[next.item$next.item,]$epv, col="red")
  
  return(next.item)
})