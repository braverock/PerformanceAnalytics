\name{rollingFunction}
\alias{rollingFunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper to apply functions over a rolling period }
\description{
This is a wrapper for providing n-period trailing calculations for the data and functions provided.
}
\usage{
rollingFunction(R, width, trim = TRUE, na.rm = TRUE, digits = 4, rf = 0, FUN = "mean", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{width}{ the number of periods over which a function is to be calculated. Use the value zero (0) to roll the statistic from inception }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{digits}{ number of digits to round results to }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{FUN}{ function to apply rolling period over }
  \item{\dots}{ any other passthru parameters }
}
\value{
results over a rolling period
}
\details{
Examples:
\preformatted{
> rollingFunction(gg.ts[,1],n=3,FUN="Return.annualized")
                    Manager
2002-02-28           0.0306
2002-03-31           0.0521
2002-04-30           0.0387
...

> rollingFunction(gg.ts[,1],n=3,trim=FALSE,FUN="Return.annualized")
                    Manager
2001-12-31               NA
2002-01-31               NA
2002-02-28           0.0306
2002-03-31           0.0521
2002-04-30           0.0387
...
> rollingFunction(gg.ts[,1],n=3,trim=FALSE,FUN="SharpeRatio.annualized")
                    Manager
2001-12-31               NA
2002-01-31               NA
2002-02-28           1.5302
2002-03-31           4.3768
2002-04-30           6.9640
...
> rollingFunction(gg.ts[,1],n=3,trim=FALSE,FUN="SharpeRatio.annualized",rf=.03/12)
                    Manager
2001-12-31               NA
2002-01-31               NA
2002-02-28           0.0298
2002-03-31           1.8587
2002-04-30           1.5598}

}
\references{ }
\author{ Peter Carl }
\note{
Inspired by \code{\link[fMultivar]{rollFun}} written by Diethelm Wurtz.  We've extended the idea to all the columns provided.
}
\seealso{
   \code{\link[fMultivar]{rollFun}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
