\name{chart.Autocorrelation}
\alias{chart.Autocorrelation}
\title{Stacked Bar Plot of Autocorrelation Lag Coefficients}
\usage{
  chart.Autocorrelation(R, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of an asset return}
}
\value{
  Stack Bar plot of lagged return coefficients
}
\description{
  A wrapper to create box and whiskers plot of comparitive
  inputs
}
\details{
  We have also provided controls for all the symbols and
  lines in the chart. One default, set by
  \code{as.Tufte=TRUE}, will strip chartjunk and draw a
  Boxplot per recommendations by Burghardt, Duncan and
  Liu(2013)
}
\examples{
data(edhec[,1])
chart.Autocorrelation(edhec[,1])
}
\author{
  R
}
\references{
  Burghardt, Duncan and Liu(2013) \emph{It's the
  autocorrelation, stupid}. AlternativeEdge Note November,
  2012 }
}
\seealso{
  \code{\link[graphics]{boxplot}}
}
\keyword{Autocorrelation}
\keyword{factors}
\keyword{lag}

