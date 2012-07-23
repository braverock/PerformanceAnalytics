\name{Central2Raw}
\alias{Central2Raw}
\title{Transforms first n central moments into first n raw moments (first central moment defined as expectation)}
\usage{
  Central2Raw(mu)
}
\arguments{
  \item{mu}{a vector of central moments}
}
\value{
  mu_ a vector of non-central moments
}
\description{
  step 2 of projection process: From the central moments of
  step 1, we compute the non-central moments. To do so we
  start with the first non-central moment and apply
  recursively an identity (formula 20)
}
\details{
  \deqn{ \tilde{ \mu }^{ \big(1\big) }_{X} \equiv \mu
  ^{\big(1\big)}_{X} \\ \tilde{ \mu }^{ \big(n\big) }_{X}
  \equiv \mu ^{n}_{X} \sum_{k=0}^{n-1} \big(-1\big)^{n-k+1}
  \mu ^{n-k}_{X} \tilde{ \mu }^{\big(k\big)}_{X} }
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}
\references{
  A. Meucci - "Exercises in Advanced Risk and Portfolio
  Management". See page 10. Symmys site containing original
  MATLAB source code \url{http://www.symmys.com}
}

