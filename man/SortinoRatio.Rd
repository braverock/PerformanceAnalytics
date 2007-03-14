\name{SortinoRatio}
\alias{SortinoRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate Sortino Ratio of performance over downside risk }
\description{
Sortino proposed an improvement on the Sharpe Ratio to better account for skill and excess performance by using only downside semivariance as the measure of risk.
}
\usage{
SortinoRatio(Ra, MAR = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{MAR}{ Minimum Acceptable Return, in the same periodicity as your returns }
}
\details{
Sortino contends that risk should be measured in terms of not meeting the investment goal.  This gives rise to the notion of "Minimum Acceptable Return" or MAR.  All of Sortino's proposed measures include the MAR, and are more sensitive to downside or extreme risks than measues that use volatility(standard deviation of returns) as the measure of risk.

Choosing the MAR carefully is very important, especially when comparing disparate investment choices.  If the MAR is too low, it will not adequately capture the risks that concern the investor, and if the MAR is too high, it will unfavorably portray what may otherwise be a sound investment.  When comparing multiple investments, some papers recommend using the risk free rate as the MAR.  Practitioners may wish to choose one MAR for consistency, several standardized MAR values for reporting a range of scenarios, or a MAR customized to the objective of the investor.
}
\value{
Sortino ratio
}
\references{
Sortino, F. and Price, L. Performance Measurement in a Downside Risk Framework. \emph{Journal of Investing}. Fall 1994, 59-65.
}
\author{ Brian G. Peterson }
\note{}
\seealso{
    \code{\link{SharpeRatio}}
    \code{\link{DownsideDeviation}}
    \code{\link{SemiVariance}}
    \code{\link{SemiDeviation}}
    \code{\link{InformationRatio}}

}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
