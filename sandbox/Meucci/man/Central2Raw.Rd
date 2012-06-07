\name{Central2Raw}
\alias{Central2Raw}
\title{Transforms central moments into raw moments (first central moment defined as expectation)}
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
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}

