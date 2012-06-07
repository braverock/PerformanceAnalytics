\name{SummStats}
\alias{SummStats}
\title{Compute summary stats}
\usage{
  SummStats(X, N)
}
\arguments{
  \item{X}{an invariant}

  \item{N}{the number of order statistics to collect}
}
\value{
  ga standardized statistics

  mu central moments
}
\description{
  step 0 in projection process: Compute summary stats
  (mean, skew, kurtosis, etc.) of the invariant X-t step 1
  in the project process We collect the first 'n' central
  moments of the invariant X-t.
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}

