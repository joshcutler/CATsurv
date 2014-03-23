#' Title
#'
#' Definition
#'
#' @param cat 
#' @param values
#' @param name
#' @param params
#'
#' @return An object of class CATsurv containing
#'  \item{prior.value}{ }
#'  
#' @author Josh W. Cutler and Jacob M. Montgomery

#' @export
setGeneric("prior", function(cat, values, name, params){standardGeneric("prior")})

#' @export
setMethod(f="prior", signature=class.name, definition=function(cat, values, name, params) {
  prior.value = switch(name,
                       normal = dnorm(values, params[1], params[2]),
                       cauchy = dcauchy(values, params[1], params[2]),
                       t = 1 / params[2] * dt((values - params[1]) / params[2], df=params[3])
  )
  return(prior.value)
})