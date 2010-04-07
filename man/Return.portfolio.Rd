\name{Return.portfolio}
\Rdversion{1.1}
\alias{Return.portfolio}
\alias{Return.rebalancing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates weighted returns for a portfolio of assets
}
\description{
Calculates weighted returns for a portfolio of assets.  If you have a single weighting vector, or want the equal weighted portfolio, use \code{Return.portfolio}.  If you have a portfolio that is periodically rebalanced, and multiple time periods with different weights, use \code{Return.rebalancing}.  Both functions will subset the return series to only include returns for assets for which \code{weight} is provided.
}
\usage{
Return.portfolio(R, weights = NULL, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, ...)
Return.rebalancing(R, weights, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{weights}{a time series or single-row matrix/vector containing asset weights, as percentages}
  \item{wealth.index}{TRUE/FALSE whether to return a wealth index, default FALSE}
  \item{contribution}{if contribution is TRUE, add the weighted return contributed by the asset in this period}
  \item{geometric}{ generate geometric (TRUE) or simple (FALSE) returns, default TRUE }
  \item{\dots}{ any other passthru parameters }
}
\details{
\code{Return.rebalancing} uses the date in the weights time series or matrix for xts-style subsetting of rebalancing periods.  
Rebalancing periods can be  thought of as taking effect immediately after the close of the bar.  
So, a March 31 rebalancing date will actually be in effect for April 1.  A December 31 rebalancing date will be in effect on Jan 1, and so forth.  
This convention was chosen because it fits with common usage, and because it simplifies xts Date subsetting via \code{endpoints}.  

\code{Return.rebalancing} will rebalance only on daily or lower frequencies.  
If you are rebalancing intraday, you should be using a trading/prices framework, not a weights-based return framework. 
}
\value{
returns a time series of returns weighted by the \code{weights} parameter, possibly including contribution for each period
}
\references{
Bacon, C. \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley. 2004. Chapter 2\cr

}
\author{
Brian G. Peterson
}

\seealso{
\code{\link{Return.calculate}} \cr
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }