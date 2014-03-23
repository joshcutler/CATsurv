#' Title
#'
#' Definition
#'
#' @param cat 
#' @param theta
#' @param difficulty
#' @param discrimination
#' @param guessing
#' @param D
#'
#' @return An object of class CATsurv containing
#'  \item{prob}{ }
#'  
#' @author Josh W. Cutler and Jacob M. Montgomery

#' @export
setGeneric("three.pl", function(cat, theta, difficulty, discrimination, guessing, D=1){standardGeneric("three.pl")})

#' @export 
setMethod(f="three.pl", signature=class.name, definition=function(cat, theta, difficulty, discrimination, guessing, D=1) {
  exp.portion = exp(D*discrimination*(theta - difficulty))
  prob = guessing + (1 - guessing)*(exp.portion / (1 + exp.portion))
})