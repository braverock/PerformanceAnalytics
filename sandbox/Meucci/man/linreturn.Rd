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
  simple arithmetic return terms \deqn{ M_{ \tau }^{i} =
  e^{ \mu ^{\tau} _{i} + \frac{1}{2} \Sigma^{ii} _{\tau} },
  \\ S^{ij} = e^{ \mu ^{\tau} _{i} + \mu ^{\tau} _{j} +
  \frac{1}{2} \big(\Sigma^{ii} _{\tau} + \Sigma^{jj}
  _{\tau}\big) } \big(e^{\Sigma^{ij} _{\tau}} - 1\big) }
}
\description{
  Generate arithmetric returns and arithmetric covariance
  matrix given a distribution of log returns
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  # formula (7) and (8) on page 5 of Appendix to "Meucci -
  A Common Pitfall in Mean-Variance Estimation"
  \url{http://www.wilmott.com/pdfs/011119_meucci.pdf}
}

