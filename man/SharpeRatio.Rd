\name{SharpeRatio}
\alias{SharpeRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Sharpe Ratio }
\description{
The Sharpe Ratio is a risk-adjusted measure of return that uses standard deviation to represent risk.
}
\usage{
SharpeRatio(Ra, Rf = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rf}{ risk free rate, in same period as your returns }
}
\details{
The Sharpe ratio is simply the return per unit of risk (represented by variance).  The higher the Sharpe Ratio, the better the combined performance of "risk" and return.

\deqn{\frac{\overline{(R_{a}-R_{f})}}{\sqrt{\sigma_{(R_{a}-R_{f})}}}}

William Sharpe now recommends \code{\link{InformationRatio}} preferentially to the original Sharpe Ratio.

}

\references{
Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio Management},Fall 1994, 49-58.
}
\author{ Peter Carl }

\seealso{
    \code{\link{SharpeRatio.annualized}}  \cr
    \code{\link{InformationRatio}} \cr
    \code{\link{TrackingError}} \cr
    \code{\link{ActivePremium}} \cr
    \code{\link{SortinoRatio}} \cr
}
\examples{
data(managers)
SharpeRatio(managers[,1,drop=FALSE], Rf=.035/12) 
SharpeRatio(managers[,1,drop=FALSE], Rf = managers[,10,drop=FALSE])
SharpeRatio(managers[,1:6], Rf=.035/12) 
SharpeRatio(managers[,1:6], Rf = managers[,10,drop=FALSE])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
