\name{kernelcdf}
\alias{kernelcdf}
\title{Evaluates cumulative distribution function for the input numeric value}
\usage{
  kernelcdf(x, xi, bw, wi)
}
\arguments{
  \item{x}{a vector containing the numeric values for which
  cumulative distribution function has to be evaluated}

  \item{xi}{a vector containing the data set}

  \item{bw}{a numeric value containing Silverman bandwidth
  of the given data set}

  \item{wi}{a vector containing weights}
}
\value{
  p a numeric containing the cumulative probability
  distribution value of length equal to x
}
\description{
  Evaluates cumulative distribution function for the input
  numeric value
}
\author{
  Manan Shah \email{mkshah@tepper.cmu.edu}
}

