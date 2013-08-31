\name{chart.Autocorrelation}
\alias{chart.Autocorrelation}
\title{Stacked Bar Autocorrelation Plot}
\usage{
  chart.Autocorrelation(R)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of an asset return}
}
\value{
  Stack Bar plot of lagged return coefficients
}
\description{
  A wrapper to create box and whiskers plot of lagged
  autocorrelation analysis
}
\details{
  We have also provided controls for all the symbols and
  lines in the chart. One default, set by
  \code{as.Tufte=TRUE}, will strip chartjunk and draw a
  Boxplot per recommendations by Burghardt, Duncan and
  Liu(2013)
}
\examples{
data(edhec)
chart.Autocorrelation(edhec[,1])
}
\author{
  Peter Carl, Brian Peterson, Shubhankit Mohan
}
\references{
  Burghardt, G., and L. Liu, \emph{ It's the
  Autocorrelation, Stupid (November 2012) Newedge working
  paper.}
  \url{http://www.amfmblog.com/assets/Newedge-Autocorrelation.pdf}
}
\seealso{
  \code{\link[graphics]{boxplot}}
}
\keyword{Autocorrelation}
\keyword{factors}
\keyword{lag}

