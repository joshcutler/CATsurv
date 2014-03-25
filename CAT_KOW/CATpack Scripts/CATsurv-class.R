#' A Computerized Adaptive Testing Survey (CATsurv) Object
#' 
#' Objects of class \code{CATsurv} are used in administering Computerized Adaptive Testing (CAT) Surveys.  These objects contain several pieces of information relevent for CAT surveys, and are used as input in the main functions of the \code{CATsurv} package, \code{\link{nextItem}} and \code{\link{storeAnswer}}. They are created using the \code{initialize} function.      
#'
#' An object of the class `CATsurv' has the following slots:
#' \itemize{
#' \item \code{questions} A data-frame where the rows represent the questions/items used in the CAT, and the columns contain information about each question/item.  See the details section for more information.  
#' \item \code{difficulties} A named vector of difficulty parameters for use with polytomous questions/items.  Each element's name tells the question/item to which it applies.  This slot \emph{must} be filled if \code{poly=TRUE}.    
#' \item \code{priorName} A character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{normal} for the normal distirbution, \code{cauchy} for the Cauchy distribution, are \code{t} for the t-distribution. Defaults to \code{normal}. 
#' \item \code{priorParams} A numeric vector of parameters for the distribution specified in the \code{priorName} slot. See the details section for more infomration.  Defaults to \code{c(1,1)}.  
#' \item \code{poly} A logical, indicating whether or not the questions/items used in this CAT have dichotomous (FALSE) or polytomous (TRUE) responses.  Defaults to FALSE. 
#' }
#'
#'@details The "questions" data frame \emph{must} contain the following columns: "difficulty", which is a vector of difficulty parameters, one per question (unless poly=TRUE, in which case this column can be ommitted but the slot difficulties must be filled); "discrimination", which is a vector of discrimination parameters, also one per question; "guessing" which is a vector of guessing parameters; "answers" which is a vector of answers to questions as given by the survey respondent.\cr
#' When priorName is set to "normal", the first priorParam is the mean, the second is the standard deviation.  When priorName is set to "Cauchy", the first priorParam is the location, and the second is the scale.  When priorName is set to "t", the first priorParam is mu, a location parameter, the second is sigma, a scale parameter, and the third is nu, the degrees of freedom parameter.   
#'
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{nextItem}}
#' @seealso \code{\link{storeAnswer}}
#' @aliases CATsurv-class initialize,CATsurv-method
#' @rdname CATsurv
#' @export
library(sfsmisc) ####PUT THIS AS A DEPENDS IN THE DESCRIPTION 

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
