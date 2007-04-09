\name{SharpeRatio}
\alias{SharpeRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Sharpe Ratio }
\description{
The Sharpe Ratio is a risk-adjusted measure of return that uses standard deviation to represent risk.
}
\usage{
SharpeRatio(Ra, rf = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{rf}{ risk free rate, in same period as your returns }
}
\details{
The Sharpe ratio is simply the return per unit of risk (represented by variance).  The higher the Sharpe Ratio, the better the combined performance of "risk" and return.

\deqn{\frac{\overline{(R_{a}-R_{f})}}{\sqrt{\sigma}}}

William Sharpe now recommends Information Ratio preferentially to the original Sharpe Ratio.

}
\value{
Sharpe Ratio
}
\references{
Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio Management},Fall 1994, 49-58.
}
\author{ Peter Carl }
\note{
}
\seealso{
    \code{\link{SharpeRatio.annualized}}
    \code{\link{InformationRatio}}
    \code{\link{TrackingError}}
    \code{\link{ActivePremium}}
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

SharpeRatio(edhec[, 6, drop = FALSE])
SharpeRatio(edhec[, 6, drop = FALSE], rf = .04/12)
SharpeRatio(edhec[, 6, drop = FALSE], rf = rf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }