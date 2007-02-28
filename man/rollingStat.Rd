\name{rollingStat}
\alias{rollingStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper to apply any function over a rolling time window }
\description{
Function to apply any time series function over a rolling period, say the last three months.  Pass in the function name as FUN, e.g., FUN = "mean".  Useful for passing in several periods and functions for comparing different series.
}
\usage{
rollingStat(R, period = 3, trim = TRUE, FUN, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{period}{ the number of periods over which to calculate the statistic. Setting the period to 0 calculates the statistic "from inception" or using all of the data passed in }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
  \item{FUN}{ function to apply over e.g., FUN = "mean" or FUN = "Return.cumulative" }
  \item{\dots}{ any other passthru parameters }
}
\details{
}
\value{
}
\references{ }
\author{ Peter Carl }
\seealso{ }
\examples{
    # Example includes how to pass in arguements to the function - must be in order:
    # > rollingStat(gg.ts@Data[,1],period=12,FUN="SharpeRatio.annualized",rf=.03/12)
    # [1] 1.476426
    # > rollingStat(gg.ts@Data[,1],period=3,FUN="SharpeRatio.annualized",rf=.03/12)
    # [1] 6.804358
    # > rollingStat(gg.ts@Data[,1],period=3,FUN="SharpeRatio.annualized",rf=0)
    # [1] 8.253612
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
