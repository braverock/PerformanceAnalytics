\name{sd.multiperiod}
\alias{sd.multiperiod}
\alias{sd.annualized}
\alias{StdDev.annualized}
\alias{std}
\alias{StdDev}

\title{ calculate a multiperiod or annualized Standard Deviation }

\description{
Standard Deviation of a set of observations \eqn{R_{a}} is given by:

    \eqn{\sigma = variance(R_{a})  \cr std=\sqrt{\sigma} \cr}{std = sqrt(var(Ra))}

It should follow that the variance is not a linear function of the number of observations.  To determine possible variance over multiple periods, it wouldn't make sense to multiply the single-period variance by the total number of periods: this could quickly lead to an absurd result where total variance (or risk) was greater than 100\%.  It follows then that the total variance needs to demonstrate a decreasing period-to-period increase as the number of periods increases. Put another way, the increase in incremental variance per additional period needs to decrease with some relationship to the number of periods. The standard accepted practice for doing this is to apply the inverse square law. To normalize standard deviation across multiple periods, we multiply by the square root of the number of periods we wish to calculate over. To annualize standard deviation, we multiply by the square root of the number of periods per year.

    \deqn{\sqrt{\sigma}\cdot\sqrt{periods}}

Note that any multiperiod or annualized number should be viewed with suspicion if the number of observations is small.

}
\usage{
    sd.multiperiod(x, na.rm=TRUE, periods = 12, ...)
    sd.annualized(x, na.rm=TRUE, periods = 12, ...)
    StdDev.annualized(Ra, na.rm=TRUE, scale = 12, ...)
    StdDev(Ra)
    std(Ra)
}
\arguments{
  \item{x, Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{periods,scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{\dots}{ any other passthru parameters }
}
\value{
standard deviation value, scaled for multiple periods
}
\references{}
\author{ Brian G. Peterson }
\seealso{
    \code{\link[stats]{sd}} \cr
    \url{http://wikipedia.org/wiki/inverse-square_law}
}
\examples{
    data(edhec)
    head(edhec[,6,drop=FALSE])
    sd.annualized(edhec[,6])
    # now for three periods:
    sd.multiperiod(edhec[,6],scale=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
