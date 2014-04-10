setGeneric("nextItemMFI", function(cat,...){standardGeneric("nextItemMFI")})
          
setMethod(f="nextItemMFI", signature=class.name, definition=function(cat, available_questions) {
     colnames(available_questions) <- c("questions","MFI")
     for (i in 1:nrow(available_questions)) {
          items <- available_questions[i,]$questions
          theta.hat <- cat@Theta.est
          exp.portion <- exp(cat@D*cat@difficulty[items]*(theta.hat-cat@discrimination[items]))
          p.prime <- (cat@D*cat@difficulty[items]*(1-cat@guessing[items])*(exp.portion/(1+exp.portion)^2))
          p <- three.pl(cat, theta.hat, cat@difficulty[items], cat@discrimination[items], cat@guessing[items])
          I <- p.prime^2/(p*(1-p))
          available_questions[i,]$MFI = I
          }
            
  next.item = available_questions[available_questions$MFI == max(available_questions$MFI, na.rm=TRUE), ]
  to.return = list(all.estimates=available_questions, next.item=row.names(next.item)[1])
            
return(to.return)
})          
