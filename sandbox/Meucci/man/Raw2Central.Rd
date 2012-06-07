\name{Raw2Central}
\alias{Raw2Central}
\title{Transforms raw moments into central moments}
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
  step 6 of projection process: compute multi-period
  central moments. Note the first central moment defined as
  expectation.
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com} TODO FIXME
  check these against the central moment functions in
  PerformanceAnalytics
}

