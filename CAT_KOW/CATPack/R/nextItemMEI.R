#' Computerized Adaptive Testing Survey Maximum Expected Information Criterion Function
#'
#' This function determines the next item by comparing maximum expected information and choosing the largest and presents it to the respondent.
#'
#' @param cat an object of \code{CATsurv} class.
#'
#' @return The next item to present to the respondent with the maximum expected information.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{nextItem}},\code{\link{nextItemEPV}}, \code{\link{nextItemKL}}, \code{\link{nextItemMFI}}, \code{\link{nextItemMPWI}}, \code{\link{nextItemMWFI}}
#' @rdname nextItemMEI

#' @export
setGeneric("nextItemMEI", function(cat,...){standardGeneric("nextItemMEI")})

#' @export
setMethod(f="nextItemMEI", signature=class.name, definition=function(cat, available_questions) {
  available_questions = data.frame(questions=which(is.na(cat@answers)),MEI=NA)
  prob.correct<- three.pl(cat, theta, cat@difficulty, cat@discrimination, cat@guessing)
  prob.incorrect<- 1-prob.correct
  
  old_val = cat@answers[items]
  
  cat@answers[items] = 1
  for (i in 1:nrow(available_questions)) {
  obsInfo.correct = observedInfo(cat, theta, available_questions[i,1])
  }
  cat@answers[items] = 0
  for (i in 1:nrow(available_questions)) {
    obsInfo.incorrect = observedInfo(cat, theta, available_questions[i,1])
  }
  cat@answers[items] = if (is.null(old_val) || is.na(old_val)) NA else old_val
  
  for (i in 1:nrow(available_questions)) {
    available_questions[i,]$MEI <- (prob.correct[i]*obsInfo.correct[i])+(prob.incorrect[i]*obsInfo.incorrect[i])
  }
     
  next.item = available_questions[available_questions$MEI == max(available_questions$MEI, na.rm=TRUE), 1 ]
  to.return = list(all.estimates=available_questions, next.item=next.item)
  return(to.return)
}
)
