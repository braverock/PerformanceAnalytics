\name{RejectOutlier}
\alias{RejectOutlier}
\title{Finds the "worst" outlier in a multivariate time series}
\usage{
  RejectOutlier(sample)
}
\arguments{
  \item{sample}{a vector containing the input dataset with
  outliers}
}
\value{
  rejected a numeric indicating which observation in the
  index to reject
}
\description{
  Finds the "worst" outlier in a multivariate time-series
  We aim at finding the the observation x_t such that if we
  remove it from the set {x_1 ... x_t } the determinant of
  the resulting sample covariance is reduced the most This
  means that by dropping that observation the
  location-dispersion ellipsoid defined by the sample mean
  and covariance shrinks the most See Sec. 4.6.1 of "Risk
  and Asset Allocation" - Springer (2005), by A. Meucci for
  the theory and the routine implemented below
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  \url{http://www.symmys.com} See Meucci script for
  "RejectOutlier.m"
}

