setGeneric("Fisher.i", function(cat, items){standardGeneric("Fisher.i")})

setMethod(f="Fisher.i", signature=class.name, definition=function(cat, items) {
  theta.hat <- estimateTheta(cat)
  exp.portion <- exp(cat@D*cat@difficulty[items]*(theta.hat-cat@discrimination[items]))
  p.prime <- (cat@D*cat@difficulty[items]*(1-cat@guessing[items])*(exp.portion/(1+exp.portion)^2))^2
  p <- three.pl(cat, theta.hat, cat@difficulty[items], cat@discrimination[items], cat@guessing[items])
  I <- p.prime^2/(p*(1-p))
}


setGeneric("nextItemFisher", function(cat){standardGeneric("nextItem")})
          
setMethod(f="nextItemFisher", signature=class.name, definition=function(cat) {
     available_questions = data.frame(questions=which(is.na(cat@answers)),fisher.i=NA)
     
     for (i in 1:nrow(available_questions)) {
             available_questions[i,]$epv = Fisher.i(cat, available_questions[i,]$questions)
            }
            
            next.item = available_questions[available_questions$fisher.i == max(available_questions$fisher.i, na.rm=TRUE), ]
            to.return = list(all.estimates=available_questions, next.item=row.names(next.item)[1])
            
return(to.return)
})          
          
          