\name{expectedPV}
\alias{expectedPV}
\title{Computerized Adaptive Testing Survey Expected Posterior Variance Estimator}
\usage{
expectedPV(cat, item, theta.est, D = 1, lowerBound = -4, upperBound = 4,
  quadPoints = 33)
}
\arguments{
  \item{cat}{an object of class \code{CATsurv}}

  \item{item}{The question for which to estimate the
  expected posterior variance for a respondent with a
  latent trait estimate of theta.hat.  This should be the
  name of a row in the "questions" data-frame in the
  "questions" slot of a \code{CATsurv} object.}

  \item{theta.est}{A scalar value containing an estimate of
  a respondent's position on the latent trait.  Generally,
  this is the output of the \code{\link{estimateTheta}}
  funciton.}

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
The expected posterior variance for respondent \emph{j} on
item \emph{k}.
}
\description{
This function estimates the expected posterior variance for
a respondent's estimated position on the latent trait on an
item which he/she has yet to answer based on a respondent's
position on the latent trait from the items he/she has
already answered.
}
\details{
The expected posterior variance is calculated as
\deqn{P(y^*_{kj}=1|\mathbf{y}_{k-1,j})\text{Var}(\theta_j|\mathbf{y}_{k-1,j},y^*_{kj}=1)+P(y^*_{kj}=0|\mathbf{y}_{k-1,j})\text{Var}(\theta_j|\mathbf{y}_{k-1,j},y^*_{kj}=0)},
where \eqn{y_{kj}^*} is a possible response to item
\emph{k} by respondent \emph{j}.
}
\author{
Josh W. Cutler: \email{josh@zistle.com} and Jacob M.
Montgomery: \email{jacob.montgomery@wustl.edu}
}
\seealso{
\code{\link{three.pl}},\code{\link{likelihood}},
\code{\link{prior.value}}, \code{\link{estimateTheta}},
\code{\link{estimateSE}}, \code{\link{nextItem}},
\code{\link{storeAnswer}}, \code{\link{debugNextItem}}
}

