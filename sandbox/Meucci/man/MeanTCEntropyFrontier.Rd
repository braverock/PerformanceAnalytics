\name{MeanTCEntropyFrontier}
\alias{MeanTCEntropyFrontier}
\title{This function computes the mean diversification efficient frontier}
\usage{
  MeanTCEntropyFrontier(S, Mu, w_b, w_0, Constr)
}
\arguments{
  \item{S}{Covariance Matrix}

  \item{Mu}{a matrix containing the expectations}

  \item{w_b}{a matrix containing the benchmark weights}

  \item{w_0}{a matrix containing the initial portfolio
  weights}

  \item{Constr}{a list containing the equality and
  inequality constraints}
}
\value{
  a list containing

  Weights

  Ne_s

  R_2_s

  m_s

  s_S
}
\description{
  This function computes the mean diversification efficient
  frontier
}
\author{
  Manan Shah \email{mkshah@cmu.edu}
}
\references{
  A. Meucci - "Managing Diversification", Risk Magazine,
  June 2009 \url{http://ssrn.com/abstract=1358533}
}

