#'@include CATsurv-class.R
#'
#' A Computerized Adaptive Testing Survey (CATsurv) Object
#' 
#' Objects of class \code{DichoCATsurv} are used in administering Computerized Adaptive Testing (CAT) Surveys containing questions with dichotomous answers. These objects contain several pieces of information relevent for CAT surveys, and are used as input in the main functions of the \code{CATsurv} package, \code{\link{nextItem}} and \code{\link{storeAnswer}}. They are created using the \code{initialize} function.      
#'
#' An object of the sub-class `DichoCATsurv' of class \code{CATsurv} has the following slots:
#' \itemize{
#' \item \code{difficulty} A named vector of difficulty parameters for use with dichotomous questions/items.  Each element's name tells the question/item to which it applies.
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
#' @aliases DichoCATsurv-class initialize, DichoCATsurv-method
#' @rdname DichoCATsurv
#' @export
library(sfsmisc) ####PUT THIS AS A DEPENDS IN THE DESCRIPTION 

class.name = "DichoCATsurv"
setClass(class.name,
         contains="CATsurv",
         representation=representation(
           difficulty="vector"
         ),
         prototype=prototype(
           priorName="normal",
           priorParams=c(1,1)
         )
)

#' @export
setMethod("initialize", class.name, function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})

##
setAs(from="CATsurv", to="DichoCATsurv",
      def=function(from){
        new("DichoCATsurv",
            guessing=from@guessing,
            discrimination=from@discrimination,
            answers=from@answers,
            priorName=from@priorName,
            priorParams=from@priorParams
        )
      }
)