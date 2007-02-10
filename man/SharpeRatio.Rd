\name{SharpeRatio}
\alias{SharpeRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Sharpe Ratio }
\description{
The Sharpe Ratio is a risk-adjusted measure of return that uses standard deviation to represent risk.
}
\usage{
SharpeRatio(R, rf = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{rf}{ risk free rate, in same period as your returns }
}
\details{
The Sharpe ratio is simply the return per unit of risk (represented by variance).  The higher the Sharpe ratio, the better the combined performance of "risk" and return.

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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }