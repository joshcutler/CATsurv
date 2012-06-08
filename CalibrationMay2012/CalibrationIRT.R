library(MCMCpack)
library(ltm)

setwd("~/GitHub/CATsurv/CalibrationMay2012")

CalibrationData <- read.csv("RawResults.csv", header=T, row.names="response_id")
CalibrationData <- CalibrationData[,-1] # Remove firs useless column
dim(CalibrationData)
CalibrationData <- CalibrationData[,colMeans(is.na(CalibrationData))!=1] # remove empty columns



# Recode NAs to 0s for knowledge questions
for (i in c(1583:1588,1590:1629, 1631:1651)){
  CalibrationData[is.na(CalibrationData[,paste("question_", i, sep="")]),paste("question_", i, sep="")] <- 0
  CalibrationData[(CalibrationData[,paste("question_", i, sep="")]==-1), paste("question_", i, sep="")] <- 0
  
}

use.these <- c(1583:1588,1590:1598, 1600:1620, 1622:1629, 1631:1651)

## Subset for IRT analysis
IRTData <- CalibrationData[,paste("question_", use.these, sep="")]
colMeans(IRTData)

system.time(chain1 <- MCMCirt1d(IRTData, burnin=50000, mcmc=200000, verbose=2500, store.item=TRUE, thin=100, seed=234897, theta.constraints=list("1259"="+", "1204"="-"), T0=1, AB0=0.25))
system.time(chain2 <- MCMCirt1d(IRTData, burnin=50000, mcmc=200000, verbose=2500, store.item=TRUE, thin=100, seed=835734, theta.constraints=list("1259"="+", "1204"="-"), T0=1, AB0=0.25))
system.time(chain3 <- MCMCirt1d(IRTData, burnin=50000, mcmc=200000, verbose=2500, store.item=TRUE, thin=100, seed=198375, theta.constraints=list("1259"="+", "1204"="-"), T0=1, AB0=0.25))

# Make an MCMC List of all chains: I am cutting off the first 500 iterations in each chain because of geweke
posteriors <- mcmc.list(chain1, chain2, chain3)

# Check for convergence
gelman.diag(posteriors)
geweke.diag(posteriors)

# Check effective size
effectiveSize(posteriors)

# Save output
save(posteriors, file="Posteriors")

## Reparameterize posteriors


re.param <- function(chain=chain1){
  this.out <- matrix(NA, ncol=2*length(use.these), nrow=niter(chain))
  colnames(this.out) <- c(paste("difficulty.",use.these, sep=""), paste("discrim.",use.these, sep="")  )
  for ( i in use.these){
 #   this.out[,paste("difficulty.", i, sep="")] <- chain[, paste("alpha.question_", i, sep="")]*chain[, paste("beta.question_", i, sep="")]*-1
    this.out[,paste("difficulty.", i, sep="")] <- chain[, paste("alpha.question_", i, sep="")]*-1
    this.out[,paste("discrim.", i, sep="")] <- chain[, paste("beta.question_", i, sep="")]
  }
as.mcmc(this.out)
}

chain1.rp <- re.param(chain1)
chain2.rp <- re.param(chain2)
chain3.rp <- re.param(chain3)

posteriors.rp <- mcmc.list(chain1.rp, chain2.rp, chain3.rp)
#gelman.diag(posteriors.rp)


## Make the output for putting into CAT

sum.post <- summary(posteriors.rp, quantiles=c(.5))

output <- matrix(ncol=2, nrow=length(use.these))
rownames(output) <- use.these
colnames(output) <- c("difficulty", "discrimination")

for (i in  use.these){
  output[as.character(i),1] <- sum.post$quantiles[paste("difficulty.", i, sep="")]
  output[as.character(i),2] <- sum.post$quantiles[paste("discrim.", i, sep="")]
}


# Compare with ltm
library(ltm)
ltm.res <- ltm(IRTData~z1, IRT.param=TRUE)



this <- cbind((pnorm(output[,1]+output[,2])), plogis(ltm.res$coefficients[,1]+ltm.res$coefficients[,2]))
colnames(this) <- c("MCMC", "ltm")
this
# these are basically the same.  So we are going to use the ltm package.


ltm.output <-  matrix(ncol=2, nrow=length(use.these))
rownames(ltm.output) <- paste("question_", use.these, sep="")
colnames(ltm.output) <- c("difficulty", "discrimination")
ltm.output[,2] <- ltm.res$coefficients[,2]
ltm.output[,1] <- ltm.res$coefficients[,1]/(-1*ltm.res$coefficients[,2])

# look at lowest performing items
ltm.output[ltm.output[,2]<.6,]

library(xtable)
(ltm.output[order(ltm.output[,"difficulty"]),])[32,]
# output
write.csv(ltm.output, file="ItemParams.csv", row.names=TRUE)              
              
              
