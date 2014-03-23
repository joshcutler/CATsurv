#' Title
#'
#' Definition
#'
#' @param cat 
#' @param theta
#' @param items
#' @param D
#'
#' @return An object of class CATsurv containing
#'  \item{L}{ }
#'  
#' @author Josh W. Cutler and Jacob M. Montgomery

#' @export
setGeneric("likelihood", function(cat, theta, items, D=1){standardGeneric("likelihood")})

#' @export
setMethod(f="likelihood", signature=class.name, definition=function(cat, theta, items, D=1) {
  if (cat@poly) {
    probabilities = c()
    L = 1
    for (question in rownames(items)) {
      this.question.cdf = three.pl(cat, theta, cat@difficulties[[question]], items[question, 'discrimination'], items[question, 'guessing'], D)
      this.question.cdf = c(1, this.question.cdf, 0)
      this.question.pdf = c()
      for (i in 2:(length(this.question.cdf))) {
        this.question.pdf[i-1] = this.question.cdf[i-1] - this.question.cdf[i]
      }
      
      L = L * this.question.pdf[items[question, 'answers']]
    }
  } else {
    probabilities = three.pl(cat, theta, items$difficulty, items$discrimination, items$guessing, D)
    L = prod(probabilities^items$answers * (1 - probabilities)^(1 - items$answers))
  }
  return(L)
})