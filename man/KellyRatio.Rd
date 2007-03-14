\name{KellyRatio}
\alias{KellyRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate Kelly criterion ratio (leverage or bet size) for a strategy }
\description{
Kelly criterion ratio (leverage or bet size) for a strategy.
}
\usage{
KellyRatio(Ra, rf = 0, method="half")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector of returns to perform a mean over }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{method}{ method=half will use the half-Kelly, this is the default}
}
\details{
The Kelly Criterion was identified by Bell Labs scientist John Kelly, and applied to blackjack and stock strategy sizing by Ed Thorpe.

The Kelly ratio can be simply stated as

bet size is the ratio of edge over odds

mathematically, you are maximizing log-utility

Kelly criterion says: f should equal the expected excess return of the strategy divided by the expected variance of the excess return, or

\deqn{leverage=\frac{(\overline{R}_{s}-R_{f})}{StdDev(R)^{2}}}{leverage = (mean(R)-rf)/StdDev(R)^2}

}
\value{
Kelly Ratio or Bet/Leverage Size
}
\references{
Thorp, Edward O. (1997; revised 1998). The Kelly Criterion in Blackjack, Sports Betting, and the Stock Market.
\url{http://www.bjmath.com/bjmath/thorp/paper.htm} \cr
\url{http://en.wikipedia.org/wiki/Kelly_criterion}
}
\author{ Brian G. Peterson }
\note{
}
\seealso{
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }