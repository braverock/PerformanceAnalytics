\name{table.AnnualizedReturns}
\alias{table.AnnualizedReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Annualized Returns Summary: Statistics and Stylized Facts }
\description{
Table of Annualized Return, Annualized Std Dev, and Annualized Sharpe
}
\usage{
table.AnnualizedReturns(R, ci = 0.95, firstcolumn = 1, scale = 12, rf = 0, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{ci}{ confidence interval, defaults to 95\% }
  \item{firstcolumn}{ first column to use from multicolumn input data R }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{digits}{ number of digits to round results to }
}
\value{
A table of estimates of annualized returns and risk measures
}
\references{ }
\author{ Peter Carl }
\seealso{
    \code{\link{Return.annualized}}
    \code{\link{StdDev.annualized}}
    \code{\link{SharpeRatio.annualized}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
