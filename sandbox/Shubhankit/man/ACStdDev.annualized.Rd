\name{ACStdDev.annualized}
\alias{ACStdDev.annualized}
\alias{sd.annualized}
\alias{sd.multiperiod}
\alias{StdDev.annualized}
\title{calculate a multiperiod or annualized Autocorrleation adjusted Standard Deviation}
\usage{
  ACStdDev.annualized(R, lag = 6, scale = NA, ...)
}
\arguments{
  \item{x}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{lag}{: number of autocorrelated lag factors
  inputted by user}

  \item{scale}{number of periods in a year (daily scale =
  252, monthly scale = 12, quarterly scale = 4)}

  \item{\dots}{any other passthru parameters}
}
\description{
  calculate a multiperiod or annualized Autocorrleation
  adjusted Standard Deviation
}
\examples{
data(edhec)
    ACsd.annualized(edhec,3)
}
\author{
  R
}
\references{
  Burghardt, G., and L. Liu, \emph{ It's the
  Autocorrelation, Stupid (November 2012) Newedge working
  paper.http://www.amfmblog.com/assets/Newedge-Autocorrelation.pdf
  \cr
}
\seealso{
  \code{\link[stats]{sd}} \cr
  \url{http://wikipedia.org/wiki/inverse-square_law}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

