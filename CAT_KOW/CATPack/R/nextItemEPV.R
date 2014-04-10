setGeneric("nextItemEPV", function(cat,...){standardGeneric("nextItemEPV")})


setMethod(f="nextItemEPV", signature=class.name, definition=function(cat, available_questions) {
  
  colnames(available_questions) <- c("questions","epv")
  
  for (i in 1:nrow(available_questions)) {
    available_questions[i,]$epv = expectedPV(cat, available_questions[i,]$questions)
  }
  
  next.item = available_questions[available_questions$epv == min(available_questions$epv, na.rm=TRUE),1]
  to.return = list(all.estimates=available_questions, next.item=next.item)
  
  return(to.return)
})