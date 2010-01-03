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
Return.portfolio(R, weights = NULL, wealth.index = FALSE, contribution = FALSE, method = c("simple", "compound"), ...)
Return.rebalancing(R, weights, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{weights}{a time series or sinlge-row matrix/vector containing asset weights, as percentages}
  \item{wealth.index}{TRUE/FALSE whether to return a wealth index, default FALSE}
  \item{contribution}{if contribution is TRUE, add the weighted return contributed by the asset in this period}
  \item{method}{"simple" or "compound"}
  \item{\dots}{ any other passthru parameters }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
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
Return.portfolio(edhec)

# now return the contribution too
Return.portfolio(edhec,contribution=TRUE)

# calculate a portfolio return with rebalancing
Return.rebalancing(edhec,weights)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }