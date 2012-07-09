\name{pHist}
\alias{pHist}
\title{Generates histogram}
\usage{
  pHist(X, p, nBins, freq = FALSE)
}
\arguments{
  \item{X}{a vector containing the data points}

  \item{p}{a vector containing the probabilities for each
  of the data points in X}

  \item{nBins}{expected number of Bins the data set is to
  be broken down into}

  \item{freq}{a boolean variable to indicate whether the
  graphic is a representation of frequencies}
}
\value{
  a list with f the frequency for each midpoint x the
  midpoints of the nBins intervals
}
\description{
  Generates histogram
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  \url{http://www.symmys.com} See Meucci script pHist.m
  used for plotting
}

