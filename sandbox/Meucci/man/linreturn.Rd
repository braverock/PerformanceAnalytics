\name{linreturn}
\alias{linreturn}
\title{Generate arithmetric returns and arithmetric covariance matrix given a distribution of log returns}
\usage{
  linreturn(mu, sigma)
}
\arguments{
  \item{mu}{a numeric containing the expected logarithmic
  returns for each security}

  \item{sigma}{a covariance matrix of log returns}
}
\value{
  a list containing two elements:

  arithmeticMean a numeric containing the mean arithmetic
  returns

  arithmeticCovariance a variance-covariance matrix in
  simple arithmetic return terms
}
\description{
  Generate arithmetric returns and arithmetric covariance
  matrix given a distribution of log returns
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}

