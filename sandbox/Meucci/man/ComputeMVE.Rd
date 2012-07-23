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
  given time series
}
\details{
  via the expectations-minimization algorithm

  \deqn{ w_{t} = \frac{1}{T} , t = 1,...,T \\ m \equiv
  \frac{1}{ \sum_{s=1}^T w_{s} } \sum_{t=1}^T w_{t} x_{t}
  \\ S \equiv \sum_{t=1}^T w_{t} \big(x_{t} - m\big)
  \big(x_{t} - m\big)' \\ Ma_{t}^{2} \equiv \big(x-m\big)'
  S^{-1} \big(x-m\big), t=1,...,T \\ w_{t} \mapsto w_{t}
  Ma_{t}^{2} \\ U = \big(x_{1}' - \hat{E}',...,x_{T}' -
  \hat{E}' \big) \\ \hat{Cov} \equiv \frac{1}{T} U'U }

  The location and scatter parameters that define the
  ellipsoid are multivariate high-breakdown estimators of
  location and scatter
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  \url{http://www.symmys.com/sites/default/files/Risk\%20and\%20Asset\%20Allocation\%20-\%20Springer\%20Quantitative\%20Finance\%20-\%20Estimation.pdf}
  See Sec. 4.6.1 of "Risk and Asset Allocation" - Springer
  (2005), by A. Meucci for the theory and the routine
  implemented below See Meucci script for "ComputeMVE.m"
}

