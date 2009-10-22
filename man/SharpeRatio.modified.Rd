\name{SharpeRatio.modified}
\alias{SharpeRatio.modified}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate a modified Sharpe Ratio of Return over VaR or ES }
\description{
The Sharpe ratio is simply the return per unit of risk (represented by variability).  The higher the Sharpe ratio, the better the combined performance of "risk" and return.

The Sharpe Ratio is a risk-adjusted measure of return that uses standard deviation to represent risk.

A number of papers now recommend using a "modified Sharpe" ratio using a Modified Cornish-Fisher VaR as the measure of Risk.

We have recently extended this concept to create multivariate modified Sharpe-like Ratios for standard deviation, Gaussian VaR, modified VaR, Gaussian Expected Shortfall, and modified Expected Shortfall. See \code{\link{VaR}} and \code{\link{ES}}.
}
\usage{
SharpeRatio.modified(R, Rf = 0, p = 0.95, FUNCT = c("VaR","ES"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rf}{ risk free rate, in same period as your returns }
  \item{p}{ confidence level for calculation, default p=.95 }
  \item{FUNCT}{ one of "VaR" or "ES" to use as the denominator }
  \item{\dots}{ any other passthru parameters to the VaR or ES functions }
}
\value{
This function returns a modified Sharpe ratio for the same periodicity of the data being input (e.g., monthly data -> monthly SR)
}
\references{
Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002, v 5.
}
\author{ Brian G. Peterson }
\seealso{
    \code{\link{VaR}} \cr
    \code{\link{ES}} \cr
    \code{\link{SharpeRatio}} \cr
%    \code{\link{MultivariateRisk}}
}
\examples{
data(edhec)
SharpeRatio.modified(edhec[, 6, drop = FALSE])
SharpeRatio.modified(edhec[, 6, drop = FALSE], Rf = .04/12)
SharpeRatio.modified(edhec[, 6, drop = FALSE], Rf = .04/12, method="gaussian")
SharpeRatio.modified(edhec[, 6, drop = FALSE], FUNCT="ES")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
