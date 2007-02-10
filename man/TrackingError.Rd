\name{TrackingError}
\alias{TrackingError}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate Tracking Error of returns against a benchmark }
\description{
A measure of the unexplained portion of performance relative to a benchmark.
}
\usage{
TrackingError(R, Rb, scale = 12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\details{
Tracking error is calculated by taking the square root of the average of the squared deviations between the investment's returns and the benchmark's returns, then multiplying the result by the square root of the scale of the returns.

\eqn{TrackingError=\sqrt{\sum\frac{(R-R_{b})^{2}}{len(R)\sqrt{scale}}}}{TrackingError = sqrt(sum(assetReturns - benchmarkReturns)^2 /
                (length(assetReturns) - 1)) * sqrt(scale)}
}
\value{
Tracking Error (number)
}
\references{
Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio Management},Fall 1994, 49-58.
}
\author{ Peter Carl }
\note{
}
\seealso{
    \code{\link{InformationRatio}}
    \code{\link{TrackingError}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
