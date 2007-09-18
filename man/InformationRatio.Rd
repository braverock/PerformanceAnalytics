\name{InformationRatio}
\alias{InformationRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ InformationRatio = ActivePremium/TrackingError }
\description{
The Active Premium divided by the Tracking Error.

InformationRatio = ActivePremium/TrackingError

This relates the degree to which an investment has beaten the benchmark to the consistency with which the investment has beaten the benchmark.
}
\usage{
InformationRatio(Ra, Rb, scale = 12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\details{
William Sharpe now recommends \code{InformationRatio} preferentially to the original \code{\link{SharpeRatio}}.
}
\value{
information ratio (number)
}
\references{
Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio Management},Fall 1994, 49-58.
}
\author{ Peter Carl }
\note{
}
\seealso{
    \code{\link{TrackingError}} \cr
    \code{\link{ActivePremium}} \cr
    \code{\link{SharpeRatio}}
}
\examples{
data(managers)
InformationRatio(managers[,"HAM1",drop=FALSE], managers[, "SP500.TR", drop=FALSE])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }