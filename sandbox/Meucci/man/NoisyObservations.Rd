\name{NoisyObservations}
\alias{NoisyObservations}
\title{Generate observations from a two asset covariance matrix and add outliers}
\usage{
  NoisyObservations(numGoodSamples, numOutliers,
    covarianceMatrix, shuffle = FALSE)
}
\arguments{
  \item{numGoodSamples}{number of observations drawn from
  the covariance matrix}

  \item{numOutliers}{number of outliers added to sample}

  \item{covarianceMatrix}{the covariance matrix for the
  asset returns from which good samples will be drawn}

  \item{shuffle}{a boolean suggesting whether order of the
  twos should be shuffled}
}
\value{
  sample a matrix of returns consisting of good and bad
  sample. Rows are observations, columns are the assets.
}
\description{
  Generate observations from a covariance matrix and add
  outliers
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}

