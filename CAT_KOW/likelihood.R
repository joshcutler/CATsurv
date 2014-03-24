#' Computerized Adaptive Testing Survey Likelihood Function
#'
#' This function returns the value of likelihood funtion asscociated with the responses to the first \eqn{k-1} items under a local independence assumption.
#'
#' @param cat an object of \code{CATsurv} class.
#' @param theta vector consisting of each respondent's position on the latent scale of interest
#' @param items data frame containing discrimination parameter, guessing parameter, difficulty parameter, and answer for each item, 
#' @param D model parameter. 1 is for a logistic model and 1.702 for an approximation of the probit model. The default value is 1.
#'
#' @return An object of class \code{CATsurv} containing
#'  \item{L}{The value of likelihood function}
#'  
#' @details Letting \eqn{q_i(\theta_j)=1-p_i(\theta_j)}, the likelihood function associated with the responses to the first \eqn{k-1} items under a local independence assumption is \deqn{L(\theta_j|\mathbf{y}_{k-1,j})=\prod^{k-1}_{i=1}p_i(\theta_j)^{Y_{ij}}q_i(\theta_j)^{(1-y_{ij}}}.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{likelihood}},\code{\link{prior}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname likelihood

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
