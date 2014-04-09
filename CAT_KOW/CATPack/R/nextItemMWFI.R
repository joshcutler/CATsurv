setGeneric("nextItemMWFI", function(cat,...){standardGeneric("nextItemMWFI")})

#' @export
setMethod(f="nextItemMWFI", signature=class.name, definition=function(cat, available_questions) {
  colnames(available_questions) <- c("questions", "MWFI")
  applicable_rows = which(!is.na(cat@answers))
  
  prior.values = prior(cat, cat@X, cat@priorName, cat@priorParams)
  likelihood.values = rep(NA, times=length(cat@X))
  
  for (i in 1:length(likelihood.values)) {
    likelihood.values[i] = likelihood(cat, cat@X[i], applicable_rows)
  }
  
  
  for (i in 1:nrow(available_questions)) {
    FishInfo <- expectedInfo(cat, cat@X, available_questions[i,1])
    available_questions[i,]$MWFI = integrate.xy(cat@X, FishInfo*likelihood.values)
  }
    
  next.item = available_questions[available_questions$MWFI == max(available_questions$MWFI, na.rm=TRUE), 1 ]
  to.return = list(all.estimates=available_questions, next.item=next.item)
  return(to.return)
}
)


