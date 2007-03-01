\name{SharpeRatio.modified}
\alias{SharpeRatio.modified}
\alias{modSharpe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate a modified Sharpe Ratio of Return/modVaR }
\description{
The Sharpe ratio is simply the return per unit of risk (represented by variability).  The higher the Sharpe ratio, the better the combined performance of "risk" and return.

The Sharpe Ratio is a risk-adjusted measure of return that uses standard deviation to represent risk.

A number of papers now recommend using a "modified Sharpe" ratio using a Modified Cornish-Fisher VaR as the measure of Risk.
}
\usage{
SharpeRatio.modified(R, rf = 0, p = 0.95, scale = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{p}{ confidence level for calculation, default p=.99 }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\details{
}
\value{
This function returns a modified Sharpe ratio for the same periodicity of the data being input (e.g., monthly data -> monthly SR)
}
\references{
Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002, v 5.
}
\author{ Brian G. Peterson }
\note{}
\seealso{
    \code{\link{VaR.CornishFisher}}
    \code{\link{SharpeRatio}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
