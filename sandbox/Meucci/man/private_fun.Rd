\name{private_fun}
\alias{private_fun}
\title{Evaluates the difference between calculated cumulative distribution function for a data point and the
true value}
\usage{
  private_fun(x, xi, bw, wi, p)
}
\arguments{
  \item{x}{a vector containing the data points for which
  cdf has to be evaluated}

  \item{xi}{a vector containing the data set}

  \item{bw}{a numeric value containing Silverman bandwidth
  of the given data set}

  \item{wi}{a vector containing weights}

  \item{p}{a vector containing the true probabilities}
}
\value{
  f a vector containing the difference between x and p
  which is of length equal to x or p
}
\description{
  Evaluates the difference between calculated cumulative
  distribution function for a data point and the true value
}
\author{
  Manan Shah \email{mkshah@tepper.cmu.edu}
}

