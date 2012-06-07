\name{EfficientFrontier}
\alias{EfficientFrontier}
\usage{
  EfficientFrontier(X, p, Options)
}
\arguments{
  \item{X}{a matrix with the joint-scenario probabilities
  by asset (rows are joint-scenarios, columns are assets)}

  \item{p}{a vector of probabilities associated with each
  scenario in matrix X}

  \item{Options}{a list of options....TBD}
}
\value{
  A list with NumPortf efficient portfolios whos returns
  are equally spaced along the whole range of the efficient
  frontier Exps the NumPortf x 1 vector of expected returns
  for each asset Covs the NumPortf x N vector of security
  volatilities along the efficient frontier w the NumPortf
  x N matrix of compositions (security weights) for each
  portfolio along the efficient frontier e the NumPortf x 1
  matrix of expected returns for each portfolio along the
  efficient frontier s the NumPortf x 1 matrix of standard
  deviation of returns for each portfolio along the
  efficient frontier
}

