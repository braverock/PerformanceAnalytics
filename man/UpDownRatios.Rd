\name{UpDownRatios}
\alias{UpDownRatios}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate metrics on up and down markets for the benchmark asset }
\description{
Calculate metrics on how the asset in R performed in up and down markets, measured by periods when the benchmark asset was up or down.
}
\usage{
UpDownRatios(R, Rb, method = "capture", side = "up", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{method}{ "capture", "number", or "percentage" to indicate which measure to return }
  \item{side}{ "up" or "down" market statistics }
  \item{\dots}{ any other passthru parameters }
}
\details{
This is a function designed to calculate several related metrics:

Up (Down) Capture Ratio: this is a measure of an investment's compound return when the benchmark was up (down) divided by the benchmark's compound return when the benchmark was up (down). The greater (lower) the value, the better.

Up (Down) Number Ratio: similarly, this is a measure of the number of periods that the investment was up (down) when the benchmark was up (down), divided by the number of periods that the Benchmark was up (down).

Up (Down) Percentage Ratio: this is a measure of the number of periods that the investment outperformed the benchmark when the benchmark was up (down), divided by the number of periods that the benchmark was up (down). Unlike the prior two metrics, in both cases a higher value is better.

}
\value{
A data.table of n-period trailing calculations for each column in x.
}
\references{}
\author{ Peter Carl }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
