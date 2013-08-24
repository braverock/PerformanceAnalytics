\name{GLMSmoothIndex}
\alias{GLMSmoothIndex}
\alias{Return.Geltner}
\title{GLM Index}
\usage{
  GLMSmoothIndex(R = NULL, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}
}
\description{
  Getmansky Lo Markov Smoothing Index is a useful summary
  statistic for measuring the concentration of weights is a
  sum of square of Moving Average lag coefficient. This
  measure is well known in the industrial organization
  literature as the \bold{ Herfindahl index}, a measure of
  the concentration of firms in a given industry. The index
  is maximized when one coefficient is 1 and the rest are
  0. In the context of smoothed returns, a lower value
  implies more smoothing, and the upper bound of 1 implies
  no smoothing, hence \eqn{\xi} is reffered as a
  '\bold{smoothingindex}'. \deqn{ \xi = \sum\theta(j)^2}
  Where j belongs to 0 to k,which is the number of lag
  factors input.
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

