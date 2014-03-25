#' Computerized Adaptive Testing Survey Next Item Function
#'
#' This function takes a respondent's previous answers to determine the next item from the list of available questions, by using the expected \emph{a posteriori} (EAP) position on the latent scale. 
#'
#' @param cat An object of class \code{CATsurv}
#' @param theta.est A scalar value to contain an estimate of a respondent's position on the latent trait, using the \code{\link{estimateTheta}} funciton. Defaults to NA.
#' @param D A numeric value used as model parameter.  For logit models, set D=1.  For an approximation of the probit model, set D=1.702.  Defaults to D=1.   
#' @param lowerBound The lower bound of the interval of the latent trait used in estimation.  Defaults to -4.
#' @param upperBound The upper bound of the interval of the latent trait used in estimation.  Defaults to 4.
#' @param quadPoints The number of points used in approximating the integral.  Defaults to 33.
#'
#' @return A data frame of available questions based on the min expected posterior variance for respondent and a row name for the next item to be asked 
#'  
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}},  \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname nextItem
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