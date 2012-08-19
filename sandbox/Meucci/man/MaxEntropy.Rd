\name{MaxEntropy}
\alias{MaxEntropy}
\title{This function computes the extreme frontier}
\usage{
  MaxEntropy(G, w_b, w_0, Constr)
}
\arguments{
  \item{G}{map weights -> conditional diversification
  distribution (square root of, not normalized)}

  \item{w_b}{a matrix containing the benchmark weights}

  \item{w_0}{a matrix containing the initial portfolio
  weights}

  \item{Constr}{a list containing the equality and
  inequality constraints}
}
\value{
  x a numeric containing the maximum entropy

  \deqn{ N_{ent} \equiv exp \big(-\sum_{n=k+1}^N p_{n} ln
  p_{n} \big), w_{ \varphi } \equiv argmax_{w \in C, \mu'w
  \geq \varphi } N_{ent} \big(w\big) }
}
\description{
  This function computes the extreme frontier
}
\author{
  Manan Shah \email{mkshah@cmu.edu}
}
\references{
  A. Meucci - "Managing Diversification", Risk Magazine,
  June 2009 - Formula (18,19)
  \url{http://ssrn.com/abstract=1358533}
}

