\name{CMAseparation}
\alias{CMAseparation}
\title{CMA separation. Decomposes arbitrary joint distributions (scenario-probabilities) into their copula and marginals}
\usage{
  CMAseparation(X, p)
}
\arguments{
  \item{X}{A matrix where each row corresponds to a
  scenario/sample from a joint distribution.  Each column
  represents the value from a marginal distribution}

  \item{p}{A 1-column matrix of probabilities of the
  Jth-scenario joint distribution in X}
}
\value{
  xdd a JxN matrix where each column consists of each
  marginal's generic x values in ascending order

  udd a JxN matrix containing the cumulative probability
  (cdf) for each marginal by column - it is rescaled by 'l'
  to be <1 at the far right of the distribution can
  interpret 'udd' as the probability weighted grade
  scenarios (see formula 11 in Meucci)

  U a copula (J x N matrix) - the joint distribution of
  grades defined by feeding the original variables X into
  their respective marginal CDF
}
\description{
  The CMA separation step attains from the cdf "F" for the
  marginal "X", the scenario-probabilities representation
  of the copula (cdf of U: "F") and the inter/extrapolation
  representation of the marginal CDF's
}
\details{
  Separation step of Copula-Marginal Algorithm (CMA) Meucci
  A., "New Breed of Copulas for Risk and Portfolio
  Management", Risk, September 2011 Most recent version of
  article and code available at
  http://www.symmys.com/node/335
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}
\references{
  \url{http://www.symmys.com}
}

