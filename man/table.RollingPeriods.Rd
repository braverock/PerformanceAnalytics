\name{table.TrailingPeriods}
\alias{table.TrailingPeriods}
\alias{table.TrailingPeriodsRel}
\alias{table.RollingPeriods}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Rolling Periods Summary: Statistics and Stylized Facts }
\description{
A table of estimates of rolling period return measures
}
\usage{
table.TrailingPeriods(R,  periods = subset(c(12,36,60), c(12,36,60)
< length(as.matrix(R[,1]))), FUNCS=c("mean","sd"), funcs.names = c("Average", "Std Dev"), digits = 4, ...)
table.TrailingPeriodsRel(R, Rb, periods = subset(c(12,36,60), c(12,36,60)
< length(as.matrix(R[,1]))), FUNCS=c("cor","CAPM.beta"), funcs.names = c("Correlation", "Beta"), digits = 4, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ an xts, vector, matrix, data frame, timeSeries or zoo object of index, benchmark, portfolio, or secondary asset returns to compare against }
  \item{periods}{ number of periods to use as rolling window(s), subset of \code{c(3, 6, 9, 12, 18, 24, 36, 48)} }
  \item{funcs.names}{ vector of function names used for labeling table rows }
  \item{FUNCS}{ list of functions to apply the rolling period to }
  \item{digits}{ number of digits to round results to }
  \item{\dots}{ any other passthru parameters for functions specified in FUNCS }
}

\author{ Peter Carl }
\seealso{ \code{\link[zoo]{rollapply}} }
\examples{
data(edhec)
table.TrailingPeriods(edhec[,10:13], FUNCS=c("SharpeRatio","VaR"), funcs.names = c("Sharpe Ratio", "Modified VaR"), periods=c(12,24,36))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }