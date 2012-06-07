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
  moments of X-t, we compute the cumulants. This process
  follows from the Taylor approximations for any small z
  and ln(1+x)~x for any small x, and from the definition of
  the first cumulant in (17). The we apply recursively the
  identity in formula (21). See Kendall and Stuart (1969)
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}

