\name{nextItem}
\alias{nextItem}
\title{Computerized Adaptive Testing Survey Next Item Function}
\usage{
nextItem(cat, theta.est = NA, D = 1, lowerBound = -4, upperBound = 4,
  quadPoints = 33)
}
\arguments{
  \item{cat}{An object of class \code{CATsurv}}

  \item{theta.est}{A scalar value to contain an estimate of
  a respondent's position on the latent trait, using the
  \code{\link{estimateTheta}} funciton. Defaults to NA.}

  \item{D}{A numeric value used as model parameter.  For
  logit models, set D=1.  For an approximation of the
  probit model, set D=1.702.  Defaults to D=1.}

  \item{lowerBound}{The lower bound of the interval of the
  latent trait used in estimation.  Defaults to -4.}

  \item{upperBound}{The upper bound of the interval of the
  latent trait used in estimation.  Defaults to 4.}

  \item{quadPoints}{The number of points used in
  approximating the integral.  Defaults to 33.}
}
\value{
A data frame of available questions based on the min
expected posterior variance for respondent and a row name
for the next item to be asked
}
\description{
This function takes a respondent's previous answers to
determine the next item from the list of available
questions, by using the expected \emph{a posteriori} (EAP)
position on the latent scale.
}
\author{
Josh W. Cutler: \email{josh@zistle.com} and Jacob M.
Montgomery: \email{jacob.montgomery@wustl.edu}
}
\seealso{
\code{\link{three.pl}},\code{\link{likelihood}},
\code{\link{prior.value}}, \code{\link{estimateTheta}},
\code{\link{estimateSE}}, \code{\link{expectedPV}},
\code{\link{storeAnswer}}, \code{\link{debugNextItem}}
}

