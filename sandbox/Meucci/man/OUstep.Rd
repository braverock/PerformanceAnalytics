\name{OUstep}
\alias{OUstep}
\title{Generate the next element based on Ornstein-Uhlenbeck Process}
\usage{
  OUstep(X_0, t, Mu, Th, Sig)
}
\arguments{
  \item{X_0}{a matrix containing the starting value of each
  process}

  \item{t}{a numeric containing the timestep}

  \item{Mu}{a vector containing the unconditional
  expectation of the process}

  \item{Th}{a transition matrix, i.e., a fully generic
  square matrix that steers the deterministic portion of
  the evolution of the process}

  \item{Sig}{a square matrix that drives the dispersion of
  the process}
}
\value{
  a list containing

  X_t a vector containing the value of the process after
  the given timestep

  Mu_t a vector containing the conditional expectation of
  the process

  Sig_t a matrix containing the covariance after the time
  step

  \deqn{ X_{t+ \tau } = \big(I- e^{- \theta \tau } \big)
  \mu + e^{- \theta \tau } X_{t} + \epsilon _{t, \tau } }
}
\description{
  Generate the next element based on Ornstein-Uhlenbeck
  Process
}
\author{
  Manan Shah \email{mkshah@cmu.edu}
}
\references{
  A. Meucci - "Review of Statistical Arbitrage,
  Cointegration, and Multivariate Ornstein-Uhlenbeck" -
  Formula (2) \url{http://ssrn.com/abstract=1404905}
}

