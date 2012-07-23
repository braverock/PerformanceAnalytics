\name{MvnRnd}
\alias{MvnRnd}
\title{Generates normal simulations whose sample moments match the population moments}
\usage{
  MvnRnd(M, S, J)
}
\arguments{
  \item{M}{a numeric indicating the sample first moment of
  the distribution}

  \item{S}{a covariance matrix}

  \item{J}{a numeric indicating the number of trials}
}
\description{
  Adapted from file 'MvnRnd.m'. Most recent version of
  article and code available at
  http://www.symmys.com/node/162 see A. Meucci -
  "Simulations with Exact Means and Covariances", Risk,
  July 2009
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}
\references{
  \url{http://www.symmys.com} TODO: Add Schur
  decomposition. Right now function is only sampling from
  mvrnorm so sample moments do no match population moments
  I have sample code commented out below to implement this
  correctly but I require a function that returns the
  unitaryMatrix from a Schur decomposition
}

