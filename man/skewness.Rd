\name{skewness}
\alias{skewness}
\title{Skewness}
\usage{
  skewness(x, na.rm = FALSE,
    method = c("moment", "fisher"), ...)
}
\arguments{
  \item{na.rm}{a logical. Should missing values be
  removed?}

  \item{method}{a character string which specifies the
  method of computation. These are either \code{"moment"}
  or \code{"fisher"} The \code{"moment"} method is based on
  the definitions of skewnessfor distributions; these forms
  should be used when resampling (bootstrap or jackknife).
  The \code{"fisher"} method correspond to the usual
  "unbiased" definition of sample variance, although in the
  case of skewness exact unbiasedness is not possible.}

  \item{x}{a numeric vector or object.}

  \item{\dots}{arguments to be passed.}
}
\description{
  compute skewness of a univariate distribution.
}
\details{
  Skewness

  compute skewness of a univariate distribution.

  This function was ported from the RMetrics package
  fUtilities to eliminate a dependency on fUtiltiies being
  loaded every time.  The function is identical except for
  the addition of \code{\link{checkData} and column
  support.}
}
\examples{
## mean -
## var -
   # Mean, Variance:
   r = rnorm(100)
   mean(r)
   var(r)

## skewness -
   skewness(r)
data(managers)
skewness(managers)
}
\author{
  Diethelm Wuertz
}
\seealso{
  \code{\link{kurtosis}}
}
\keyword{univar}

