\name{FitOU}
\alias{FitOU}
\title{Fit the Ornstein-uhlenbeck process to model the behavior for different values of the timestep.}
\usage{
  FitOU(Y, tau)
}
\arguments{
  \item{Y}{a matrix containing the value of a process at
  various time steps.}

  \item{tau}{a numeric containing the timestep}
}
\value{
  a list containing

  Mu a vector containing the expectation of the process

  Sig a matrix containing the covariance of the resulting
  fitted OU process

  Th a transition matrix required for defining the fitted
  OU process

  \deqn{ x_{t+ \tau } = \big(I- e^{- \theta \tau } \big)
  \mu + e^{- \theta \tau } x_{t}, vec \big( \Sigma _{ \tau
  } \big) \equiv \big( \Theta \oplus \Theta \big) ^{-1}
  \big(I- e^{( \Theta \oplus \Theta ) \tau } \big) vec
  \big( \Sigma \big) }
}
\description{
  Fit the Ornstein-uhlenbeck process to model the behavior
  for different values of the timestep.
}
\author{
  Manan Shah \email{mkshah@cmu.edu}
}
\references{
  A. Meucci - "Review of Statistical Arbitrage,
  Cointegration, and Multivariate Ornstein-Uhlenbeck" -
  Formula (8),(9) \url{http://ssrn.com/abstract=1404905}
}

