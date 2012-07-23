\name{Prior2Posterior}
\alias{Prior2Posterior}
\title{Calculate the full-confidence posterior distributions of Mu and Sigma}
\usage{
  Prior2Posterior(M, Q, M_Q, S, G, S_G)
}
\arguments{
  \item{M}{a numeric vector with the Mu of the normal
  reference model}

  \item{Q}{a numeric vector used to construct a view on
  expectation of the linear combination QX}

  \item{M_Q}{a numeric vector with the view of the
  expectations of QX}

  \item{S}{a covariance matrix for the normal reference
  model}

  \item{G}{a numeric vector used to construct a view on
  covariance of the linear combination GX}

  \item{S_G}{a numeric with the expectation associated with
  the covariance of the linear combination GX}
}
\value{
  a list with

  M_ a numeric vector with the full-confidence posterior
  distribution of Mu

  S_ a covariance matrix with the full-confidence posterior
  distribution of Sigma
}
\description{
  \deqn{ \tilde{ \mu } \equiv \mu + \Sigma Q' {\big(Q
  \Sigma Q' \big)}^{-1} \big( \tilde{\mu}_{Q} - Q \mu
  \big), \\ \tilde{ \Sigma } \equiv \Sigma + \Sigma G'
  \big({\big(G \Sigma G' \big)}^{-1} \tilde{ \Sigma }_G
  {\big(G \Sigma G' \big)}^{-1} - {\big(G \Sigma G'
  \big)}^{-1} \big) G \Sigma }
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  \url{http://www.symmys.com}
  \url{http://ssrn.com/abstract=1213325} A. Meucci - "Fully
  Flexible Views: Theory and Practice". See formula (21)
  and (22) on page 7 See Meucci script Prior2Posterior.m
  attached to Entropy Pooling Paper
}

