\name{GenerateLogNormalDistribution}
\alias{GenerateLogNormalDistribution}
\title{Generate arbitrary distribution of a shifted-lognormal invariant}
\usage{
  GenerateLogNormalDistribution(J, a, m, s)
}
\arguments{
  \item{J}{a numeric with the number of scenarios}

  \item{a}{a numeric with the location shift parameter.
  Mean of distribution will be exp(a)}

  \item{m}{log of the mean of the distribution}

  \item{s}{log of the standard deviation of the
  distribution}
}
\value{
  X a numeric vector with i.i.d. lognormal samples based on
  parameters J, a, m, and s where X = a + exp( m + s * Z )
}
\description{
  \deqn{X = a + e^{ m + sZ }} (formula 14)
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}

