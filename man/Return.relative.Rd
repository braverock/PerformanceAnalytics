\name{Return.relative}
\Rdversion{1.1}
\alias{Return.relative}
\title{ calculate the relative return of one asset to another }
\description{
Calculates the ratio of the cumulative performance for two assets through time.
}
\usage{
Return.relative(Ra, Rb, ...)
}
\arguments{
  \item{Ra}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return object for the benchmark asset }
  \item{\dots}{ ignored }
}
\value{
xts or other time series of relative return 
}
\author{
Peter Carl
}
\seealso{
\code{\link{chart.RelativePerformance}}
}
\examples{
data(managers)
head(Return.relative(managers[,1:3], managers[,8,drop=FALSE]),n=20)
}
\keyword{ ts }
