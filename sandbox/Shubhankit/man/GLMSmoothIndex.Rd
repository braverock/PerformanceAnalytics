\name{GLMSmoothIndex}
\alias{GLMSmoothIndex}
\alias{Return.Geltner}
\title{Getmansky Lo Markov Smoothing Index Parameter}
\usage{
  GLMSmoothIndex(R = NULL, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}
}
\description{
  A useful summary statistic for measuring the
  concentration of weights is a sum of square of Moving
  Average lag coefficient. This measure is well known in
  the industrial organization literature as the Herfindahl
  index, a measure of the concentration of firms in a given
  industry. The index is maximized when one coefficient is
  1 and the rest are 0. In the context of smoothed returns,
  a lower value of x implies more smoothing, and the upper
  bound of 1 implies no smoothing, hence x is reffered as a
  ''smoothingindex' '.

  \deqn{ R_t = \mu + \beta \delta_t+ \xi_t}
}
\examples{
data(edhec)
head(GLMSmoothIndex(edhec))
}
\author{
  Peter Carl, Brian Peterson, Shubhankit Mohan
}
\references{
  "An econometric model of serial correlation and
  illiquidity in hedge fund returns" Mila Getmansky, Andrew
  W. Lo, Igor Makarov
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{non-iid}
\keyword{ts}

