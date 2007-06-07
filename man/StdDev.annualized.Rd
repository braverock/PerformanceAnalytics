\name{sd.multiperiod}
\alias{sd.multiperiod}
\alias{sd.annualized}
\alias{StdDev.annualized}
\alias{std}
\alias{StdDev}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate an multiperiod or annualized Standard Deviation }
\description{
Standard Deviation of a set of observations \eqn{R_{a}} is given by:

\eqn{\sigma = variance(R_{a})  \cr std=\sqrt{\sigma} \cr}{std = sqrt(var(Ra))}

It should follow that the variance is not a linear function of the number of observatios.  To determine possible variance over multiple periods, it wouldn't make sense to multiply the single-period variance by the total number of periods: this could quickly lead to an absurd result where risk was greater than 100\%.  It follows then that the variance needs to demonstrated a decreassing period to period increase as the number of periods increases. To nortmalize standard deviation across multiple periods, we multiply by the square root of the number of periods we wish to calculate over. To annualize standard deviation, we multiply by the square root of the number of periods per year.

\deqn{\sqrt{\sigma}\cdot\sqrt{periods}}

Note that any multiperiod or annualized number should be viewed with suspicion if the number of observations is small.

}
\usage{
sd.multiperiod(x, na.rm=FALSE, periods = 12, ...)
sd.annualized(x, na.rm=FALSE, periods = 12, ...)
StdDev.annualized(Ra, na.rm=FALSE, scale = 12)
StdDev(Ra)
std(Ra)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\value{
standard deviation value, scaled for multiple periods
}
\references{}
\author{ Brian G. Peterson }
\seealso{
    \code{\link[stats]{sd}}
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
