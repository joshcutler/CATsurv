#' A CATsurv value object 
#' 
#' Object of class \code{CATsurv} are created
#'
#' 
#' An object of the class `CATsurv' has the following slots:
#' \itemize{
#' \item \code{questions}
#' \item \code{difficulties} 
#' \item \code{priorName}
#' \item \code{priorParams}
#' \item \code{poly}
#' \item
#' }
#'
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @aliases CATsurv-class initialize,CATsurv-method
#' @rdname CATsurv
#' @export
library(sfsmisc)

class.name = "CATsurv"
setClass(class.name,
         representation=representation(
           questions="data.frame",
           difficulties="vector",
           priorName="character",
           priorParams="numeric",
           poly='logical'
         ),
         prototype=prototype(
           priorName="normal",
           priorParams=c(1,1),
           poly=FALSE
         )
)

#' @export
setValidity(class.name, function(object) {
  cols = names(object@questions)
  
  if (!object@poly &&!("difficulty" %in% cols))
    return("No difficulty column detected in @questions")
  if (!("discrimination" %in% cols))
    return("No discrimination column detected in @questions")
  if (!("guessing" %in% cols))
    return("No guessing column detected in @questions")
  if (!("answers" %in% cols))
    return("No answers column detected in @questions")
  if (object@poly) 
    if (!("response_count" %in% cols))
      return("No response_count column detected in @questions for polytmous model")
  if (is.null(object@difficulties))
    return("No difficulties present for polytmous model")
  
  return(TRUE)
})

#' @export
setMethod("initialize", class.name, function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})
