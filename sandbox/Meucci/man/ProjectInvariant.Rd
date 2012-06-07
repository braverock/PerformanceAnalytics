\name{ProjectInvariant}
\alias{ProjectInvariant}
\title{Annualization and Projection algorithm for invariant}
\usage{
  ProjectInvariant(N = 6, K = 251, X)
}
\arguments{
  \item{N}{a numeric with the number of the first N
  stadardized summary statistics to project}

  \item{K}{a numeric with an arbitrary projection horizon}

  \item{X}{a numeric vector consisting of a generic
  (additive) invariant the follows the general linear and
  square-root rules for projecting means and volatility}
}
\value{
  Ga a numeric vector with the first 'N' order statistics
  projected to the horizon 'K'
}
\description{
  Project summary statistics to arbitrary horizons under
  i.i.d. assumption SYMMYS - Last version of article and
  code available at http://symmys.com/node/136 Project
  summary statistics to arbitrary horizons under i.i.d.
  assumption see Meucci, A. (2010) "Annualization and
  General Projection of Skewness, Kurtosis and All Summary
  Statistics" GARP Risk Professional, August, pp. 52-54
}
\examples{
X = GenerateLogNormalDistribution( J = 100000 , a = 01 , m = .2 , s = .4 ) # X = a + exp( m + s * Z ) # generate log-normal distribution
          moments = ProjectInvariant( N = 6 , K = 251 , X )
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}

