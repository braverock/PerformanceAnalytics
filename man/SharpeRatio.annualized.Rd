\name{SharpeRatio.annualized}
\alias{SharpeRatio.annualized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate annualized Sharpe Ratio }
\description{
The Sharpe Ratio is a risk-adjusted measure of return that uses standard deviation to represent risk.
}
\usage{
SharpeRatio.annualized(R, Rf = 0, scale = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rf}{ risk free rate, in same period as your returns }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\details{
The Sharpe ratio is simply the return per unit of risk (represented by variance).  The higher the Sharpe ratio, the better the combined performance of "risk" and return.

This function annualizes the number based on the scale parameter.

\deqn{\frac{\sqrt[n]{prod(1+R_{a})^{scale}}-1}{\sqrt{scale}\cdot\sqrt{\sigma}}}

Using an annualized Sharpe Ratio is useful for comparison of multiple return streams.  The annualized Sharpe ratio is computed by dividing the annualized mean monthly excess return by the annualized monthly standard deviation of excess return.

William Sharpe now recommends Information Ratio preferentially to the original Sharpe Ratio.
}
\value{
annualized Sharpe Ratio
}
\references{
Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio Management},Fall 1994, 49-58.
}
\author{ Peter Carl }
\note{
}
\seealso{
    \code{\link{SharpeRatio}} \cr
    \code{\link{InformationRatio}} \cr
    \code{\link{TrackingError}} \cr
    \code{\link{ActivePremium}} \cr
    \code{\link{SortinoRatio}}
}
\examples{
data(edhec)
head(edhec[, 6, drop = FALSE])
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
start
end = rownames(edhec[edhec.length,])
rf = download.RiskFree(start=start, end=end)
time(Rf=0)=as.yearmon(time(Rf=0))

SharpeRatio.annualized(edhec[, 6, drop = FALSE])
SharpeRatio.annualized(edhec[, 6, drop = FALSE], rf = .04/12)
SharpeRatio.annualized(edhec[, 6, drop = FALSE], rf = Rf=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }