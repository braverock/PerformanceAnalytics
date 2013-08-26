\name{golden_section}
\alias{golden_section}
\title{Golden Section Algorithm}
\usage{
  golden_section(a, b, minimum = TRUE, function_name, ...)
}
\arguments{
  \item{a}{initial point}

  \item{b}{final point}

  \item{minimum}{TRUE to calculate the minimum and FALSE to
  calculate the Maximum}

  \item{function_name}{The name of the function}

  \item{\dots}{any other passthru variable}
}
\description{
  The Golden Section Search method is used to find the
  maximum or minimum of a unimodal [a,b].) To make the
  discussion of the method simpler, let us assume that we
  are trying to find the maximum of a function. choose
  three points \eqn{x_l},\eqn{x_1} and \eqn{x_u} \eqn{(x_l
  \textless x_1 \textless x_u)} along the x-axis with the
  corresponding values of the function
  \eqn{f(x_l)},\eqn{f(x_1)} and \eqn{f(x_u)}, respectively.
  Since \eqn{f(x_1)\textgreater f(x_l)} and \eqn{f(x_1)
  \textgreater f(x_u)}, the maximum must lie between
  \eqn{x_l} and \eqn{x_u}. Now a fourth point denoted by
  \eqn{x_2} is chosen to be between the larger of the two
  intervals of \eqn{[x_l,x_1]} and \eqn{[x_1,x_u]}/
  Assuming that the interval \eqn{[x_l,x_1]} is larger than
  the interval \eqn{[x_1,x_u]} we would choose
  \eqn{[x_l,x_1]} as the interval in which \eqn{x_2} is
  chosen. If \eqn{f(x_2)>f(x_1)} then the new three points
  would be \eqn{x_l \textless x_2 \textless x_1} else if
  \eqn{f(x_2)<f(x_1)} then the three new points are
  \eqn{x_2<x_1<x_u}. This process is continued until the
  distance between the outer point is sufficiently small.
}
\author{
  Pulkit Mehrotra
}
\references{
  Bailey, David H. and Lopez de Prado, Marcos,
  Drawdown-Based Stop-Outs and the "Triple Penance"
  Rule(January 1, 2013).
}

