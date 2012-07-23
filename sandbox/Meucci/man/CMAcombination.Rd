\name{CMAcombination}
\alias{CMAcombination}
\title{CMA combination. Glues an arbitrary copula and arbitrary marginal distributions into a new joint distribution}
\usage{
  CMAcombination(x, u, U)
}
\arguments{
  \item{x}{a generic x variable. Note: Linearly spaced 'x'
  help for coverage when performing linear interpolation}

  \item{u}{The value of the cumulative density function
  associated with x (parametric or non-parametric)}

  \item{U}{an aribtrary copula. Can take any copula
  obtained with the separation step (i.e. a set of
  scenario-probabilities)}
}
\value{
  X a J x N matrix containing the new joint distribution
  based on the arbitrary copula 'U'
}
\description{
  The combination step starts from arbitrary marginal
  distributions, and grades distributed according to a
  chosen arbitrary copula which can, but does not need to,
  be obtained by seperation. Then this function combines
  the marginals and copula into a new joint distribution.
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}
\references{
  Meucci A., "New Breed of Copulas for Risk and Portfolio
  Management", Risk, September 2011 Most recent version of
  article and code available at
  \url{http://www.symmys.com/node/335}
}

