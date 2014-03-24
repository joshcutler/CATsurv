#' Three-Parameter IRT Model
#'
#' This function returns the probability a correct response for each individual.
#'
#' @param cat 
#' @param theta vector of each respondent's position on the latent scale of interest.
#' @param difficulty an item difficulty parameter number.
#' @param discrimination an item discrimination parameter number.
#' @param guessing an item guessability parameter number. 
#' @param D a model parameter number. 1 is for a logistic model and 1.702 for an approximation of the probit model. The defaulat value is 1. 
#'
#' @return An object of class CATsurv containing
#'  \item{prob}{The probability of a correct response for each individual}
#'
#' @details The probability of a correct response for respondent \eqn{j} is \deqn{Pr(y_j=1|\theta_j)=c+(1-c)\frac{\exp(Da(\theta_j-b))}{1+\exp(Da(\theta_j-b))}} where \theta_j is respondent \eqn{j}'s position on the latent scale of interest, \eqn{a} is an item discrimination parameter, \eqn{b} is an item difficulty parameter, and \eqn{c} is an item guessablity parameter.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{likelihood}},\code{\link{prior}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname three.pl
#' @export
setGeneric("three.pl", function(cat, theta, difficulty, discrimination, guessing, D=1){standardGeneric("three.pl")})

#' @export 
setMethod(f="three.pl", signature=class.name, definition=function(cat, theta, difficulty, discrimination, guessing, D=1) {
  exp.portion = exp(D*discrimination*(theta - difficulty))
  prob = guessing + (1 - guessing)*(exp.portion / (1 + exp.portion))
})
