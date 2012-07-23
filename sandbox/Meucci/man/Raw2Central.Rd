\name{Raw2Central}
\alias{Raw2Central}
\title{Transforms the first n raw moments into the first n central moments}
\usage{
  Raw2Central(mu_)
}
\arguments{
  \item{mu_}{the raw (multi-period) non-central moment of
  Y-t}
}
\value{
  mu (multi-period) central moment of Y-t
}
\description{
  step 6 of projection process:
}
\details{
  compute multi-period central moments.

  Note the first central moment defined as expectation.

  \deqn{\tilde{ \mu } ^ {\big(n\big)} _{X} \equiv E \big\{
  X^{n} \big\}, \\ \mu ^{ \big(n\big) }_{X} \equiv
  \sum_0^{n-1} \big(-1\big)^{n-k} \mu ^{n-k}_{X} \tilde{
  \mu }^{k}_{X} + \tilde{ \mu }_{X}^{n} }
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}
\references{
  A. Meucci - "Exercises in Advanced Risk and Portfolio
  Management". See page 9 Symmys site containing original
  MATLAB source code \url{http://www.symmys.com}
}

