\name{Cumul2Raw}
\alias{Cumul2Raw}
\title{Transforms cumulants of Y-t into raw moments}
\usage{
  Cumul2Raw(ka)
}
\arguments{
  \item{ka}{cumulants of Y}
}
\value{
  mu_ the raw non-central moments of Y
}
\description{
  step 5 of the projection process:
}
\details{
  From the cumulants of Y we compute the raw non-central
  moments of Y

  We do so recursively by the identity in formula (24)
  which follows from applying (21) and re-arranging terms

  \deqn{ \tilde{ \mu } ^{ \big(n\big) }_{Y} \equiv \kappa^{
  \big(n\big) }_{Y} + \sum_{k=1}^{n-1} (n-1)C_{k-1}
  \kappa_{Y}^{ \big(k\big) } \tilde{ \mu } ^{n-k}_{Y} }
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}
\references{
  A. Meucci - "Annualization and General Projection of
  Skewness, Kurtosis and All Summary Statistics" - formula
  (24) Symmys site containing original MATLAB source code
  \url{http://www.symmys.com/node/136}
}

