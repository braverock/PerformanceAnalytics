\name{SharpeRatio}
\alias{SharpeRatio.modified}
\alias{SharpeRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate a traditional or modified Sharpe Ratio of Return over StdDev or VaR or ES }
\description{
The Sharpe ratio is simply the return per unit of risk (represented by variability).  
In the classic case, the unit of risk is the standard deviation of the returns.  

\deqn{\frac{\overline{(R_{a}-R_{f})}}{\sqrt{\sigma_{(R_{a}-R_{f})}}}}

William Sharpe now recommends \code{\link{InformationRatio}} preferentially to the original Sharpe Ratio.


The higher the Sharpe ratio, the better the combined performance of "risk" and return.

As noted, the traditional Sharpe Ratio is a risk-adjusted measure of return that uses standard deviation to represent risk.

A number of papers now recommend using a "modified Sharpe" ratio using a Modified Cornish-Fisher VaR or CVaR/Expected Shortfall as the measure of Risk.

We have recently extended this concept to create multivariate modified Sharpe-like Ratios for standard deviation, Gaussian VaR, modified VaR, Gaussian Expected Shortfall, and modified Expected Shortfall. See \code{\link{VaR}} and \code{\link{ES}}.  
You can pass additional arguments to \code{\link{VaR}} and \code{\link{ES}} via \dots  The most important is probably the 'method' argument/

This function returns a traditional or modified Sharpe ratio for the same periodicity of the data being input (e.g., monthly data -> monthly SR)
}
\usage{
SharpeRatio.modified(R, Rf = 0, p = 0.95, FUN = c("StdDev","VaR","ES"), weights=NULL, ...)
SharpeRatio(R, Rf = 0, p = 0.95, FUN = c("StdDev","VaR","ES"), weights=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rf}{ risk free rate, in same period as your returns }
  \item{p}{ confidence level for calculation, default p=.95 }
  \item{FUN}{ one of "StdDev" or "VaR" or "ES" to use as the denominator }
  \item{weights}{ portfolio weighting vector, default NULL}
  \item{\dots}{ any other passthru parameters to the VaR or ES functions }
}

\references{
Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio Management},Fall 1994, 49-58.


Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002, v 5.
}
\author{ Brian G. Peterson }
\seealso{
    \code{\link{SharpeRatio.annualized}}  \cr
    \code{\link{InformationRatio}} \cr
    \code{\link{TrackingError}} \cr
    \code{\link{ActivePremium}} \cr
    \code{\link{SortinoRatio}} \cr
    \code{\link{VaR}} \cr
    \code{\link{ES}} \cr
}
\examples{
data(managers)
SharpeRatio(managers[,1,drop=FALSE], Rf=.035/12, FUN="StdDev") 
SharpeRatio(managers[,1,drop=FALSE], Rf = managers[,10,drop=FALSE], FUN="StdDev")
SharpeRatio(managers[,1:6], Rf=.035/12, FUN="StdDev") 
SharpeRatio(managers[,1:6], Rf = managers[,10,drop=FALSE], FUN="StdDev")



data(edhec)
SharpeRatio(edhec[, 6, drop = FALSE], FUN="VaR")
SharpeRatio(edhec[, 6, drop = FALSE], Rf = .04/12, FUN="VaR")
SharpeRatio(edhec[, 6, drop = FALSE], Rf = .04/12, FUN="VaR" , method="gaussian")
SharpeRatio(edhec[, 6, drop = FALSE], FUN="ES")

# and all the methods
SharpeRatio(managers[,1:9], Rf = managers[,10,drop=FALSE])
SharpeRatio(edhec,Rf = .04/12)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }