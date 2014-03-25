\name{likelihood}
\alias{likelihood}
\title{Computerized Adaptive Testing Survey Likelihood Function}
\usage{
likelihood(cat, theta, items, D = 1)
}
\arguments{
  \item{cat}{an object of class \code{CATsurv}.}

  \item{theta}{vector consisting of each respondent's
  position on the latent scale of interest}

  \item{item-level}{parameters data frame containing
  discrimination parameter, guessing parameter, difficulty
  parameter, and answer for each item,}

  \item{D}{model parameter. 1 is for a logistic model and
  1.702 for an approximation of the probit model. The
  default value is 1.}
}
\value{
A vector of the value of the likelihood function associated
with the responses to the first \eqn{k-1} items for each
respondent.
}
\description{
This function returns the value of likelihood funtion
asscociated with the responses to the first \eqn{k-1} items
under a local independence assumption.
}
\details{
Letting \eqn{q_i(\theta_j)=1-p_i(\theta_j)}, the likelihood
function associated with the responses to the first
\eqn{k-1} items under a local independence assumption is
\deqn{L(\theta_j|\mathbf{y}_{k-1,j})=\prod^{k-1}_{i=1}p_i(\theta_j)^{Y_{ij}}q_i(\theta_j)^{(1-y_{ij}}}.
}
\author{
Josh W. Cutler and Jacob M. Montgomery
}
\seealso{
\code{\link{three.pl}},\code{\link{prior.value}},
\code{\link{estimateTheta}}, \code{\link{estimateSE}},
\code{\link{expectedPV}}, \code{\link{nextItem}},
\code{\link{storeAnswer}}, \code{\link{debugNextItem}}
}

