\name{gaussHermiteMesh}
\alias{gaussHermiteMesh}
\title{Generates grid reprensentation of a distribution according to the method suggested by Meucci and inspired from
Gauss-Hermite quadratures.}
\usage{
  gaussHermiteMesh(J)
}
\arguments{
  \item{J}{a numeric containing the number of points on the
  grid}
}
\value{
  x a matrix containing the zeroes of Hermite polynomials
  as a function of polynomial degree
}
\description{
  Grid representation by this method is an alternative to
  representing distribution of the market, such as
  importance sampling or stratified sampling.However, these
  techniques focus on sub-domains of the distribution, and
  thus, in order to apply such methods, we must forego the
  full flexibility on the specification of the views. A
  different approach, which preserves the generality of the
  views specification, consists in selecting the scenarios
  x_j deterministically as a grid and then associate with
  each of them the suitable probability p_j ( integrated
  over I_j ). The the generic interval _j contains the j-th
  point of the grid. Once the grid is defined, the entropy
  optimization can be applied to replace p_j with the new
  posterior probabilities to reflect the views. We generate
  the grid here using the Gauss-Hermite quadrature method.
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  A. Meucci - "Fully Flexible Extreme Views".
  \url{http://ssrn.com/abstract=1542083}
}

