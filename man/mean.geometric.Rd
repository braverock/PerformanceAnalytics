\name{mean.geometric}
\alias{mean.geometric}
\alias{mean.LCL}
\alias{mean.stderr}
\alias{mean.UCL}
\alias{mean.utils}
\title{calculate attributes relative to the mean of the observation series given,
including geometric, stderr, LCL and UCL}
\usage{
  mean.geometric(x, ...)
}
\arguments{
  \item{x}{a vector, matrix, data frame, or time series to
  calculate the modified mean statistic over}

  \item{ci}{the confidence interval to use}

  \item{\dots}{any other passthru parameters}
}
\description{
  \tabular{ll}{ \code{\link{mean.geometric}} \tab geometric
  mean \cr \code{\link{mean.stderr}} \tab standard error of
  the mean (S.E. mean) \cr \code{\link{mean.LCL}} \tab
  lower confidence level (LCL) of the mean \cr
  \code{\link{mean.UCL}} \tab upper confidence level (UCL)
  of the mean \cr }
}
\examples{
data(edhec)
mean.geometric(edhec[,"Funds of Funds"])
mean.stderr(edhec[,"Funds of Funds"])
mean.UCL(edhec[,"Funds of Funds"])
mean.LCL(edhec[,"Funds of Funds"])
}
\author{
  Peter Carl
}
\seealso{
  \code{\link[stats]{sd}} \cr \code{\link[base]{mean}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

