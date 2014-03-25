\name{estimateSE}
\alias{estimateSE}
\title{Computerized Adaptive Testing Survey Posterior Standard Error of Estimated Latent Trait Position Estimator}
\usage{
estimateSE(cat, theta.hat, D = 1, priorName = NULL, priorParams = NULL,
  lowerBound = -4, upperBound = 4, quadPoints = 33, ...)
}
\arguments{
  \item{cat}{An object of class \code{CATsurv}}

  \item{theta.hat}{A scalar value containing an estimate of
  a respondent's position on the latent trait.  Generally,
  this is the output of the \code{\link{estimateTheta}}
  funciton.}

  \item{D}{A numeric value used as model parameter.  For
  logit models, set D=1.  For an approximation of the
  probit model, set D=1.702.  Defaults to D=1.}

  \item{priorName}{The type of prior distribution for
  respondent's position on the latent scale of interest.}

  \item{priorParams}{The parameters for the prior
  distribution specified by \code{priorName}.  The first
  parameter is the mean/location parameter, the second is
  the standard deviation/scale parameter, and the third
  (only used by the "t" distribution) is a degrees of
  freedom parameter.}

  \item{lowerBound}{The lower bound of the interval of the
  latent trait used in estimation.  Defaults to -4.}

  \item{upperBound}{The upper bound of the interval of the
  latent trait used in estimation.  Defaults to 4.}

  \item{quadPoints}{The number of points used in
  approximating the integral.  Defaults to 33.}
}
\value{
The estimate of the standard error of the user supplied
\code{theta.hat}
}
\description{
This function estimates the standard error of an estimate
of a respondent's expected \emph{a posteriori} (EAP)
position on the latent scale.
}
\details{
The standard error of the expected \emph{a posteriori}
(EAP) estimate of respondent \eqn{j}'s position on the
latent scale is calculated as the square root of
\deqn{E((\theta_j-\hat{\theta_j}^{(\text{EAP})})^2|\mathbf{y}_{k-1,j})=\frac{\int(\theta_j-\hat{\theta_j}^{(\text{EAP})})^2\pi(\theta_j)L(\theta_j|\mathbf{y}_{k-1,j}d\theta_j}{\int\pi(\theta_j)L(\theta_j|\mathbf{y}_{k-1,j})d\theta_j}}.
}
\author{
Josh W. Cutler: \email{josh@zistle.com} and Jacob M.
Montgomery: \email{jacob.montgomery@wustl.edu}
}
\seealso{
\code{\link{three.pl}},\code{\link{likelihood}},
\code{\link{prior.value}}, \code{\link{estimateTheta}},
\code{\link{expectedPV}}, \code{\link{nextItem}},
\code{\link{storeAnswer}}, \code{\link{debugNextItem}}
}

