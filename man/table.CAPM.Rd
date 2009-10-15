\name{table.CAPM}
\alias{table.CAPM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Asset-Pricing Model Summary: Statistics and Stylized Facts }
\description{
Takes a set of returns and relates them to a market benchmark. Provides a set of measures related to the excess return single index model, or CAPM.
}
\usage{
table.CAPM(Ra, Rb, scale = NA, Rf = 0, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector of returns to test, e.g., the asset to be examined }
  \item{Rb}{ a matrix, data.frame, or timeSeries of benchmark(s) to test the asset against. }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{Rf}{ risk free rate, in same period as your returns }
  \item{digits}{ number of digits to round results to }
}
\details{ This table will show statistics pertaining to an asset against a set of benchmarks, or statistics for a set of assets against a benchmark. }
\value{ A data frame with columns named for the columns in \code{Rb} and rows containing statistics.

}
\references{ }
\author{ Peter Carl }
\seealso{
    \code{\link{CAPM.alpha}} \cr
    \code{\link{CAPM.beta}}  \cr
    \code{\link{TrackingError}}  \cr
    \code{\link{ActivePremium}}  \cr
    \code{\link{InformationRatio}}  \cr
    \code{\link{TreynorRatio}}
}
\examples{
data(managers)
table.CAPM(managers[,1:3,drop=FALSE], managers[,8,drop=FALSE], Rf = managers[,10,drop=FALSE])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
