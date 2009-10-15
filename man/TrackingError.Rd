\name{TrackingError}
\alias{TrackingError}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate Tracking Error of returns against a benchmark }
\description{
A measure of the unexplained portion of performance relative to a benchmark.
}
\usage{
TrackingError(Ra, Rb, scale = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\details{
Tracking error is calculated by taking the square root of the average of the squared deviations between the investment's returns and the benchmark's returns, then multiplying the result by the square root of the scale of the returns.

\deqn{ TrackingError = \sqrt{\sum\frac{(R_{a}-R_{b})^{2}}{len(R_{a})\sqrt{scale}}} }{ TrackingError = sqrt(sum(Ra - Rb)^2 / (length(R) - 1)) * sqrt(scale)}
}
\value{
Tracking Error (number)
}
\references{
Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio Management},Fall 1994, 49-58.
}
\author{ Peter Carl }
\seealso{
    \code{\link{InformationRatio}}
    \code{\link{TrackingError}}
}
\examples{
data(managers)
TrackingError(managers[,1,drop=FALSE], managers[,8,drop=FALSE]) 
TrackingError(managers[,1:6], managers[,8,drop=FALSE]) 
TrackingError(managers[,1:6], managers[,8:7,drop=FALSE])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
