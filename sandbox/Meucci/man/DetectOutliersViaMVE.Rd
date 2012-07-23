\name{DetectOutliersViaMVE}
\alias{DetectOutliersViaMVE}
\title{Use the minimum volume ellipsoid to detect outliers}
\usage{
  DetectOutliersViaMVE(corruptSample)
}
\arguments{
  \item{corruptSample}{a matrix of returns with outlier
  data. Rows are observations, columns are assets.}
}
\value{
  a list containing: plotdata a matrix of data used to plot
  minimum volume ellipsoid as a function of its length
  cutofflist an ordering of observations with the highest
  Mahalanobis distance (i.e. ordering of outliers by their
  index )#' numOutliers returns the number of outliers
  based on the slope of the minimum volume ellipsoid as a
  function of sample data
}
\description{
  See Sec. 4.6.1 of "Risk and Asset Allocation" - Springer
  (2005), by A. Meucci for the theory and the routine
  implemented below
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  \url{http://www.symmys.com} See Meucci script for
  "S_HighBreakdownMVE.m"
}

