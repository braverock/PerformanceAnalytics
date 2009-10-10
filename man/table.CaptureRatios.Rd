\name{table.CaptureRatios}
\alias{table.CaptureRatios}
\alias{table.UpDownRatios}
\title{ Calculate and display a table of capture ratio and related statistics }
\description{
Creates a table of capture ratios and similar metrics for a set of returns against a benchmark.
}
\usage{
table.CaptureRatios(Ra, Rb, digits = 4)
table.UpDownRatios(Ra, Rb, digits = 4)
}
\arguments{
  \item{Ra}{ a vector of returns to test, e.g., the asset to be examined }
  \item{Rb}{ a matrix, data.frame, or timeSeries of benchmark(s) to test the asset against. }
  \item{digits}{ number of digits to round results to for presentation }
}
\details{
This table will show statistics pertaining to an asset against a set of benchmarks, or statistics for a set of assets against a benchmark.  \code{table.CaptureRatios} shows only the capture ratio; \code{table.UpDownRatios} shows three: the capture ratio, the number ratio, and the percentage ratio.
}
\value{
  A data.table of calculations for each set of returns against each benchmark.
}
\author{ Peter Carl }
\seealso{ \code{\link{UpDownRatios}}, \code{\link{chart.CaptureRatios}} }
\examples{
data(managers)
table.CaptureRatios(managers[,1:6], managers[,7,drop=FALSE])
table.UpDownRatios(managers[,1:6], managers[,7,drop=FALSE])
}
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
