\name{table.RollingPeriods}
\alias{table.RollingPeriods}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Rolling Periods Summary: Statistics and Stylized Facts }
\description{
A table of estimates of rolling period return measures
}
\usage{
table.RollingPeriods(R, firstcolumn = 1, periods = subset(c(3, 6, 9, 12, 18, 24, 36, 48), c(3, 6, 9, 12, 18, 24, 36, 48) < length(as.matrix(R[, 1]))), scale = 12, rf = 0, FUNCS = c("mean", "sd"), digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{firstcolumn}{ first column to use from multicolumn input data R }
  \item{periods}{ number of periods to use as rolling window(s), subset of \code{c(3, 6, 9, 12, 18, 24, 36, 48)} }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{FUNCS}{ list of functions to apply the rolling period to }
  \item{digits}{ number of digits to round results to }
}
\value{
A table of estimates of rolling period return measures
}
\references{ }
\author{ Peter Carl }
\seealso{ }
\examples{
data(edhec)
table.RollingPeriods(edhec[,10:13], FUNCS=c("SharpeRatio","VaR.CornishFisher"),periods=c(12,24,36))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }