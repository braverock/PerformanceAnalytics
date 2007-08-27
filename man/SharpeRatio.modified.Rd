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
SharpeRatio.modified(Ra, rf = 0, p = 0.95, scale = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{p}{ confidence level for calculation, default p=.99 }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\details{
\deqn{modSharpe=\frac{(\overline{R_{a}-R_{f}})}{modVaR_{R_{a},p}}}{modSharpe=mean(Ra-rf)/(modVaR(Ra,p))}

or for an annualized measure:

\deqn{modSharpe=\frac{\sqrt[n]{prod(1+(R_{a}-R_{f}))^{scale}}-1}{\sqrt{scale}\cdot modVaR_{R_{a},p}}}{modSharpe=Return.annualized(Ra,rf)/(modVaR(Ra,p)*sqrt(scale))}
}

\value{
This function returns a modified Sharpe ratio for the same periodicity of the data being input (e.g., monthly data -> monthly SR), or scaled to multiple periods according to the scale parameter
}
\references{
Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002, v 5.
}
\author{ Brian G. Peterson }
\note{}
\seealso{
    \code{\link{VaR.CornishFisher}} \cr
    \code{\link{SharpeRatio}}
}
\examples{
data(edhec)
head(edhec[, 6, drop = FALSE])
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
start
end = rownames(edhec[edhec.length,])
rf = download.RiskFree(start=start, end=end)

SharpeRatio.modified(edhec[, 6, drop = FALSE])
SharpeRatio.modified(edhec[, 6, drop = FALSE], rf = .04/12)
SharpeRatio.modified(edhec[, 6, drop = FALSE], rf = rf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
