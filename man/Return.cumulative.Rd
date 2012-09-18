\name{Return.cumulative}
\alias{Return.cumulative}
\title{calculate a compounded (geometric) cumulative return}
\usage{
  Return.cumulative(R, geometric = TRUE)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{geometric}{utilize geometric chaining (TRUE) or
  simple/arithmetic chaining (FALSE) to aggregate returns,
  default TRUE}
}
\description{
  This is a useful function for calculating cumulative
  return over a period of time, say a calendar year.  Can
  produce simple or geometric return.
}
\details{
  product of all the individual period returns

  \deqn{(1+r_{1})(1+r_{2})(1+r_{3})\ldots(1+r_{n})-1=prod(1+R)-1}{prod(1+R)-1}
}
\examples{
data(managers)
Return.cumulative(managers[,1,drop=FALSE])
Return.cumulative(managers[,1:8])
Return.cumulative(managers[,1:8],geometric=FALSE)
}
\author{
  Peter Carl
}
\references{
  Bacon, Carl. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. p. 6
}
\seealso{
  \code{\link{Return.annualized}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

