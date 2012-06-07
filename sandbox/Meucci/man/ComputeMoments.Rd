\name{ComputeMoments}
\alias{ComputeMoments}
\title{Takes a matrix of joint-scenario probability distributions and generates expectations, standard devation, and correlation matrix for the assets}
\usage{
  ComputeMoments(X, p)
}
\arguments{
  \item{X}{a matrix of joint-probability scenarios (rows
  are scenarios, columns are assets)}

  \item{p}{a numeric vector containing the probabilities
  for each of the scenarios in the matrix X}
}
\value{
  means a numeric vector of the expectations (probability
  weighted) for each asset

  sd a numeric vector of standard deviations corresponding
  to the assets in the covariance matrix

  correlationMatrix the correlation matrix resulting from
  converting the covariance matrix to a correlation matrix
}
\description{
  Takes a matrix of joint-scenario probability
  distributions and generates expectations, standard
  devation, and correlation matrix for the assets
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}

