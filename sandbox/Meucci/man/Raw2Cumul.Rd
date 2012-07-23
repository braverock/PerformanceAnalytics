\name{Raw2Cumul}
\alias{Raw2Cumul}
\title{Transforms raw moments into cumulants}
\usage{
  Raw2Cumul(mu_)
}
\arguments{
  \item{mu_}{non-central moments of the invariant X-t}
}
\value{
  ka cumulants of X-t
}
\description{
  Step 3 of the projection process: From the non-central
  moments of X-t, we compute the cumulants.
}
\details{
  This process follows from the Taylor approximations for
  any small z and ln(1+x)~x for any small x, and from the
  definition of the first cumulant in (17). The we apply
  recursively the identity in formula (21). See Kendall and
  Stuart (1969)

  \deqn{ \kappa^{ \big(n\big) }_{X} \equiv \tilde{ \mu } ^{
  \big(n\big) }_{X} - \sum_{k=1}^{n-1} (n-1)C_{k-1}
  \kappa_{X}^{ \big(k\big) } \tilde{ \mu } ^{n-k}_{X} }
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}
\references{
  A. Meucci - "Annualization and General Projection of
  Skewness, Kurtosis and All Summary Statistics" - formula
  (21) Symmys site containing original MATLAB source code
  \url{http://www.symmys.com/node/136}
}

