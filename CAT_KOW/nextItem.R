#' Title
#'
#' Definition
#'
#' @param cat
#' @param theta.est 
#' @param lowerBound
#' @param upperBound
#' @param quadPoints
#'
#' @return An object of class CATsurv containing
#'  \item{to.return}{ }
#'  
#' @author Josh W. Cutler and Jacob M. Montgomery

#' @export
setGeneric("nextItem", function(cat, theta.est=NA, D=1, lowerBound=-4, upperBound=4, quadPoints=33){standardGeneric("nextItem")})

#' @export
setMethod(f="nextItem", signature=class.name, definition=function(cat, theta.est=NA, D=1, lowerBound=-4, upperBound=4, quadPoints=33) {
  available_questions = cat@questions[is.na(cat@questions$answers), ]
  
  if (is.na(theta.est)) {
    theta.est = estimateTheta(cat, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
  }
  
  available_questions$epv = NA
  for (i in 1:nrow(available_questions)) {
    available_questions[i,]$epv = expectedPV(cat, row.names(available_questions[i,]), theta.est)
  }
  
  next.item = available_questions[available_questions$epv == min(available_questions$epv, na.rm=TRUE), ]
  to.return = list(all.estimates=available_questions, next.item=row.names(next.item)[1])
  
  return(to.return)
})