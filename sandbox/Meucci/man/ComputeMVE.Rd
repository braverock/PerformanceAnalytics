\name{ComputeMVE}
\alias{ComputeMVE}
\title{Compute the minimum volume ellipsoid for a given (multi-variate) time-series}
\usage{
  ComputeMVE(data)
}
\arguments{
  \item{data}{a matrix time-series of data. Each row is a
  observation (date). Each column is an asset}
}
\value{
  list a list with MVE_Location a numeric with the location
  parameter of minimum volume ellipsoid MVE_Dispersion a
  numeric with the covariance matrix of the minimum volume
  ellipsoid
}
\description{
  Function computes the minimum volume ellipsoid for a
  given time series via the expectations-minimization
  algorithm The location and scatter parameters that define
  the ellipsoid are multivariate high-breakdown estimators
  of location and scatter See Sec. 4.6.1 of "Risk and Asset
  Allocation" - Springer (2005), by A. Meucci for the
  theory and the routine implemented below
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  \url{http://www.symmys.com} See Meucci script for
  "ComputeMVE.m"
}

