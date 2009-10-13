\name{kurtosis}
\alias{kurtosis}
\title{Kurtosis}
\description{
    compute kurtosis of a univariate distribution
}

\details{
    This function was ported from the RMetrics package fUtilities to eliminate a dependency on fUtilties being loaded every time.  This function is identical except for the addition of \code{\link{checkData}} and additional labeling.
}
\author{Diethelm Wuertz}
\usage{
kurtosis(x, na.rm = FALSE, method = c("excess", "moment", "fisher"), \dots)
}
\arguments{

    \item{na.rm}{
        a logical. Should missing values be removed?
        }
    \item{method}{
        a character string which specifies the method of computation.
        These are either \code{"moment"}, \code{"fisher"}, or \code{"excess"}.
        If \code{"excess"} is
        selected, then the value of the kurtosis is computed by the
        \code{"moment"} method and a value of 3 will be subtracted.
        The \code{"moment"} method is based on the definitions of
        kurtosis for distributions; these forms should
        be used when resampling (bootstrap or jackknife). The
        \code{"fisher"} method correspond to the usual "unbiased"
        definition of sample variance, although in the case of
        kurtosis exact unbiasedness is not possible.
        }
    \item{x}{
        a numeric vector or object.
        }
    \item{\dots}{
        arguments to be passed.
        }

}
\value{
    \code{kurtosis}
    returns the value of the statistics, a numeric value. An
    attribute which reports the used method is added.
}
\seealso{
    \code{\link{skewness}}.
}
\examples{
## mean -
## var -
   # Mean, Variance:
   r = rnorm(100)
   mean(r)
   var(r)

## kurtosis -
   kurtosis(r)

data(managers)
kurtosis(managers[,1:8])
}
\keyword{univar}

