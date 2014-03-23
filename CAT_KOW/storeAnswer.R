#' Title
#'
#' Definition
#'
#' @param cat
#' @param item 
#' @param answer
#'
#' @return An object of class CATsurv containing
#'  \item{cat}{ }
#'  
#' @author Josh W. Cutler and Jacob M. Montgomery

#' @export
setGeneric("storeAnswer", function(cat, item, answer){standardGeneric("storeAnswer")})

#' @export
setMethod(f="storeAnswer", signature=class.name, definition=function(cat, item, answer) {
  cat@questions[item, 'answers'] = answer
  return(cat)
})