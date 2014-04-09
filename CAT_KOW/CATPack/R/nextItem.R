#' Computerized Adaptive Testing Survey Next Item Function
#'
#' This function takes a respondent's previous answers to determine the next item from the list of available questions determined according to a user specified methods for latent trait estimation and item selection. 
#'
#' @param cat An object of class \code{CATsurv}
#' @param ability.estimator The estimation procedure used to estimate the respondent's position on the latent scale.  The three options are "EAP" for expected a posterior (the default),  "ML" for maximum likelihood, and "MAP" for maximum a posterior.
#' 
#' @return A data frame of available questions based on the use selected item selection criterion for the respondent and a row name for the next item to be asked 
#'  
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}},  \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname nextItem
#' @export
setGeneric("nextItem", function(cat,ability.estimator="EAP"){standardGeneric("nextItem")})

#' @export
setMethod(f="nextItem", signature=class.name, definition=function(cat,ability.estimator="EAP") {
  available_questions = data.frame(questions=which(is.na(cat@answers)),epv=NA)
  
  cat@Theta.est <- switch(ability.estimator,EAP=estimateTheta(cat),ML=estimateThetaML(cat),MAP=estimateThetaMAP(cat))
  
  for (i in 1:nrow(available_questions)) {
    available_questions[i,]$epv = expectedPV(cat, available_questions[i,]$questions)
  }
  
  next.item = available_questions[available_questions$epv == min(available_questions$epv, na.rm=TRUE),1]
  to.return = list(all.estimates=available_questions, next.item=next.item)
  
  return(to.return)
})

