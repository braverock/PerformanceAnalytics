\name{sd.multiperiod}
\alias{sd.multiperiod}
\alias{sd.annualized}
\alias{StdDev.annualized}

\title{ calculate a multiperiod or annualized Standard Deviation }

\description{
Standard Deviation of a set of observations \eqn{R_{a}} is given by:

    \eqn{\sigma = variance(R_{a})  \cr std=\sqrt{\sigma} \cr}{std = sqrt(var(R))}

It should follow that the variance is not a linear function of the number of observations.  To determine possible variance over multiple periods, it wouldn't make sense to multiply the single-period variance by the total number of periods: this could quickly lead to an absurd result where total variance (or risk) was greater than 100\%.  It follows then that the total variance needs to demonstrate a decreasing period-to-period increase as the number of periods increases. Put another way, the increase in incremental variance per additional period needs to decrease with some relationship to the number of periods. The standard accepted practice for doing this is to apply the inverse square law. To normalize standard deviation across multiple periods, we multiply by the square root of the number of periods we wish to calculate over. To annualize standard deviation, we multiply by the square root of the number of periods per year.

    \deqn{\sqrt{\sigma}\cdot\sqrt{periods}}

Note that any multiperiod or annualized number should be viewed with suspicion if the number of observations is small.

}
\usage{
    sd.multiperiod(x, scale = NA)
    sd.annualized(x, scale = NA)
    StdDev.annualized(R, scale = NA)
}
\arguments{
  \item{x, R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\value{
standard deviation value, scaled for multiple periods
}
\references{
Bacon, C. \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley. 2004. p. 27 \cr

}
\author{ Brian G. Peterson }
\seealso{
    \code{\link[stats]{sd}} \cr
    \url{http://wikipedia.org/wiki/inverse-square_law}
}
\examples{
    data(edhec)
    sd.annualized(edhec)
    sd.annualized(edhec[,6,drop=FALSE])
    # now for three periods:
    sd.multiperiod(edhec[,6,drop=FALSE],scale=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
