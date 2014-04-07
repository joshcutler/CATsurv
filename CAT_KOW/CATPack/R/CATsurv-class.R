#' A Computerized Adaptive Testing Survey (CATsurv) Object
#' 
#' Objects of class \code{CATsurv} are used in administering Computerized Adaptive Testing (CAT) Surveys.  These objects contain several pieces of information relevent for CAT surveys, and are used as input in the main functions of the \code{CATsurv} package, \code{\link{nextItem}} and \code{\link{storeAnswer}}. They are created using the \code{initialize} function.      
#'
#' An object of the class `CATsurv' has the following slots:
#' \itemize{
#' \item \code{guessing} A named vector of guessing parameters 
#' \item \code{discrimination} A named vector of disrimination parameters including a numeric value per question/item.
#' \item \code{answers} A named vector of answers to questions as given by the survey respondent.    
#' \item \code{priorName} A character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{normal} for the normal distirbution, \code{cauchy} for the Cauchy distribution, are \code{t} for the t-distribution. Defaults to \code{normal}. 
#' \item \code{priorParams} A numeric vector of parameters for the distribution specified in the \code{priorName} slot. See the details section for more infomration.  Defaults to \code{c(1,1)}.  
#' }
#'
#'@details When priorName is set to "normal", the first priorParam is the mean, the second is the standard deviation.  When priorName is set to "Cauchy", the first priorParam is the location, and the second is the scale.  When priorName is set to "t", the first priorParam is mu, a location parameter, the second is sigma, a scale parameter, and the third is nu, the degrees of freedom parameter.   
#'
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{nextItem}}
#' @seealso \code{\link{storeAnswer}}
#' @aliases CATsurv-class initialize,CATsurv-method
#' @rdname CATsurv
#' @export

class.name = "CATsurv"
setClassUnion("numericORlist", c("numeric","list"))
setClassUnion("logicalORnumeric", c("numeric","logical"))

setClass("CATsurv",
         slots=list(
           guessing="numeric",
           discrimination="numeric",
           answers="logicalORnumeric",
           priorName="character",
           priorParams="numeric",
           lowerBound="numeric",
           upperBound="numeric",
           quadPoints="numeric",
           D="numeric",
           X="numeric",
           Theta.est="numeric",
           difficulty="numericORlist",
           poly="logical"
         ),
         prototype=prototype(
           priorName="normal",
           priorParams=c(0,1),
           lowerBound=-4.5,
           upperBound=4.5,
           quadPoints=43,
           D=1,
           poly=FALSE
         )
)


#' @export
setMethod("initialize", class.name, function(.Object, ...) {
  value = callNextMethod()
  value@X <- seq(from=value@lowerBound,to=value@upperBound,length=value@quadPoints)
  validObject(value)
  return(value)
})
