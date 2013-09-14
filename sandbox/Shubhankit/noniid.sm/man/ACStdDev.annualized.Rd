\name{ACStdDev.annualized}
\alias{ACStdDev.annualized}
\alias{StdDev.annualized}
\alias{sd.annualized}
\alias{sd.multiperiod}
\title{Autocorrleation adjusted Standard Deviation}
\usage{
  ACStdDev.annualized(R, lag = 6, scale = NA, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{lag}{: number of autocorrelated lag factors
  inputted by user}

  \item{scale}{number of periods in a year (daily scale =
  252, monthly scale = 12, quarterly scale = 4)}

  \item{\dots}{any other passthru parameters}
}
\description{
  Incorporating the component of lagged autocorrelation
  factor into adjusted time scale standard deviation
  translation
}
\details{
  Given a sample of historical returns R(1),R(2), . .
  .,R(T),the method assumes the fund manager smooths
  returns in the following manner, when 't' is the unit
  time interval: The square root time translation can be
  defined as : \deqn{ \sigma(T) = T \sqrt\sigma(t)}
}
\examples{
library(PerformanceAnalytics)
data(edhec)
ACStdDev.annualized(edhec,3)
}
\author{
  Peter Carl,Brian Peterson, Shubhankit Mohan
}
\references{
  Burghardt, G., and L. Liu, \emph{ It's the
  Autocorrelation, Stupid (November 2012) Newedge working
  paper.} Paper Available at :
  \url{http://www.amfmblog.com/assets/Newedge-Autocorrelation.pdf}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

