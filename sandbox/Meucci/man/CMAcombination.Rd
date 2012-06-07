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
  Combination step of Copula-Marginal Algorithm (CMA) based
  on Meucci A., "New Breed of Copulas for Risk and
  Portfolio Management", Risk, September 2011. Most recent
  version of article and code available at
  http://www.symmys.com/node/335
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}
\references{
  \url{http://www.symmys.com}
}

