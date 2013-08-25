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
  \emph{Getmansky, Mila, Lo, Andrew W. and Makarov, Igor}
  An Econometric Model of Serial Correlation and
  Illiquidity in Hedge Fund Returns (March 1, 2003). MIT
  Sloan Working Paper No. 4288-03; MIT Laboratory for
  Financial Engineering Working Paper No. LFE-1041A-03;
  EFMA 2003 Helsinki Meetings. Available at SSRN:
  \url{http://ssrn.com/abstract=384700}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{non-iid}
\keyword{ts}

