\name{kernelinv}
\alias{kernelinv}
\title{Evaluates inverse probability distribution function for the input probability in order to get the data point}
\usage{
  kernelinv(p, xi, bw, wi)
}
\arguments{
  \item{p}{a vector containing the probabilities
  corresponding to which the data points have to be
  evaluated}

  \item{xi}{a vector containing the data set}

  \item{bw}{a numeric value containing Silverman bandwidth
  of the given data set}

  \item{wi}{a vector containing weights}
}
\value{
  x a vector containing the evaluated numeric values of
  length equal to p
}
\description{
  Evaluates inverse probability distribution function for
  the input probability in order to get the data point
}
\author{
  Manan Shah \email{mkshah@tepper.cmu.edu}
}

