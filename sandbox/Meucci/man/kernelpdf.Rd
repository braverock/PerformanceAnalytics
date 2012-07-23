\name{kernelpdf}
\alias{kernelpdf}
\title{Evaluates probability distribution function for the input numeric value}
\usage{
  kernelpdf(x, xi, bw, wi)
}
\arguments{
  \item{x}{a vector containing the numeric values for which
  probability distribution function has to be evaluated}

  \item{xi}{a vector containing the data set}

  \item{bw}{a numeric value containing Silverman bandwidth
  of the given data set}

  \item{wi}{a vector containing weights}
}
\value{
  p a vector containing the probability distribution
  function value of length equal to x
}
\description{
  Evaluates probability distribution function for the input
  numeric value
}
\author{
  Manan Shah \email{mkshah@tepper.cmu.edu}
}

