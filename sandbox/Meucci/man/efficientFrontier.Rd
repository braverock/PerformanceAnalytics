\name{efficientFrontier}
\alias{efficientFrontier}
\title{Construct the mean-variance efficient frontier using a quadratic solver}
\usage{
  efficientFrontier(discretizations, cov, mu,
    longonly = FALSE)
}
\arguments{
  \item{discretizations}{number of portfolios to generate
  along efficient frontier (where each portfolio is equally
  distanced in return spaced)}

  \item{cov}{arithmetic covariance matrix of asset returns}

  \item{mu}{a vector of arithmetic returns for each asset}

  \item{longonly}{a boolean which constrains weights to > 0
  if true}
}
\value{
  a list of portfolios along the frontier from least risky
  to most risk The indices in each list correspond to each
  other returns the expected portfolio returns along the
  frontier volatility the variance of the portfolio along
  the frontier weights the weights of the portfolio
  components along the frontier
}
\description{
  Construct a number of long-only or long-short portfolios
  on the mean-variance efficient frontier where each
  portfolio is equally distanced in return space
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}

