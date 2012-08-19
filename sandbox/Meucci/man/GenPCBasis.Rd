\name{GenPCBasis}
\alias{GenPCBasis}
\title{This function computes the conditional principal portfolios}
\usage{
  GenPCBasis(S, A)
}
\arguments{
  \item{S}{Covariance Matrix}

  \item{A}{Conditioning Matrix}
}
\value{
  a list containing

  E a matrix containing conditional principal portfolios
  composition

  L a matrix containing conditional principal portfolios
  variances

  G map weights -> conditional diversification distribution
  (square root of, not normalized)

  \deqn{ e_{n} \equiv argmax_{ e'e \equiv 1 } \big\{ e'
  \Sigma e \big\} s.t.  e' \Sigma e_{j} \equiv 0 }
}
\description{
  This function computes the conditional principal
  portfolios
}
\author{
  Manan Shah \email{mkshah@cmu.edu}
}
\references{
  A. Meucci - "Managing Diversification", Risk Magazine,
  June 2009 - Formula (12)
  \url{http://ssrn.com/abstract=1358533}
}

