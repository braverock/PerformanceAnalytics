\name{GenerateLogNormalDistribution}
\alias{GenerateLogNormalDistribution}
\title{Generate arbitrary distribution of a shifted-lognormal invariant: X-t + a ~ LogN(m,s^2) (formula 14)}
\usage{
  GenerateLogNormalDistribution(J = 1e+05, a = -1, m = 0.2,
    s = 0.4)
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
  Generate arbitrary distribution of a shifted-lognormal
  invariant: X-t + a ~ LogN(m,s^2) (formula 14)
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}

