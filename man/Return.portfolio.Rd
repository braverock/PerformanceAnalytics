\name{Return.rebalancing}
\alias{Return.portfolio}
\alias{Return.rebalancing}
\title{Calculates weighted returns for a portfolio of assets}
\usage{
Return.rebalancing(R, weights, ...)

Return.portfolio(R, weights = NULL, wealth.index = FALSE,
  contribution = FALSE, geometric = TRUE, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{weights}{a time series or single-row matrix/vector
  containing asset weights, as percentages}

  \item{wealth.index}{TRUE/FALSE whether to return a wealth
  index, default FALSE}

  \item{contribution}{if contribution is TRUE, add the
  weighted return contributed by the asset in this period}

  \item{geometric}{utilize geometric chaining (TRUE) or
  simple/arithmetic chaining (FALSE) to aggregate returns,
  default TRUE}

  \item{\dots}{any other passthru parameters}
}
\value{
returns a time series of returns weighted by the
\code{weights} parameter, possibly including contribution
for each period
}
\description{
Calculates weighted returns for a portfolio of assets.  If
you have a single weighting vector, or want the equal
weighted portfolio, use \code{Return.portfolio}.  If you
have a portfolio that is periodically rebalanced, and
multiple time periods with different weights, use
\code{Return.rebalancing}.  Both functions will subset the
return series to only include returns for assets for which
\code{weight} is provided.
}
\details{
\code{Return.rebalancing} uses the date in the weights time
series or matrix for xts-style subsetting of rebalancing
periods.  Rebalancing periods can be thought of as taking
effect immediately after the close of the bar.  So, a March
31 rebalancing date will actually be in effect for April 1.
A December 31 rebalancing date will be in effect on Jan 1,
and so forth.  This convention was chosen because it fits
with common usage, and because it simplifies xts Date
subsetting via \code{endpoints}.

\code{Return.rebalancing} will rebalance only on daily or
lower frequencies. If you are rebalancing intraday, you
should be using a trading/prices framework, not a
weights-based return framework.
}
\examples{
data(edhec)
data(weights)

# calculate an equal weighted portfolio return
round(Return.portfolio(edhec),4)

# now return the contribution too
round(Return.portfolio(edhec,contribution=TRUE),4)

# calculate a portfolio return with rebalancing
round(Return.rebalancing(edhec,weights),4)
}
\author{
Brian G. Peterson
}
\references{
Bacon, C. \emph{Practical Portfolio Performance Measurement
and Attribution}. Wiley. 2004. Chapter 2\cr
}
\seealso{
\code{\link{Return.calculate}} \cr
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

