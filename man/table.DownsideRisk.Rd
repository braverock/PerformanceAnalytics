\name{table.DownsideRisk}
\alias{table.DownsideRisk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Downside Risk Summary: Statistics and Stylized Facts }
\description{
Creates estimates of various downside risk measures.
}
\usage{
table.DownsideRisk(R, ci = 0.95, firstcolumn = 1, scale = 12, rf = 0, MAR = 0.1/12, p = 0.99, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{ci}{ confidence interval, defaults to 95\% }
  \item{firstcolumn}{ first column to use from multicolumn input data R }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{MAR}{ Minimum Acceptable Return, in the same periodicity as your returns }
  \item{p}{ confidence level for calculation, default p=.99 }
  \item{digits}{ number of digits to round results to }
}
\details{
}
\value{
A table of estimates of downside risk measures
}
\references{ }
\author{ Peter Carl }
\seealso{
    \code{\link{DownsideDeviation}}
    \code{\link{maxDrawdown}}
    \code{\link{VaR.CornishFisher}}
    \code{\link{VaR.Beyond}}
    \code{\link{VaR}}
    \code{\link{StdDev}}
}
\examples{
data(edhec)
table.DownsideRisk(edhec,rf=.04/12)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
