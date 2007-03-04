\name{BetaCoKurtosis}
\alias{BetaCoKurtosis}
\alias{BetaCoK}
\alias{SystematicKurtosis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ systematic kurtosis of an asset to the initial portfolio }
\description{
Beta CoKurtosis is the beta of an asset to the kurtosis of an initial portfolio.  Used to determine diversification potential.
Also called "systematic kurtosis" or "systematic cokurtosis" by several papers.
}
\usage{
BetaCoKurtosis(Ri, Ra, na.rm = FALSE, method = c("moment", "excess", "fisher"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ri}{ return vector of initial portfolio }
  \item{Ra}{ return vector of asset being considered for addition to portfolio }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{method}{ method to use when computing \code{\link{kurtosis}} one of: \code{excess}, \code{moment}, \code{fisher} }
}
\details{
}
\value{
systematic kurtosis of two assets
}
\references{
Martellini L., Vaissie M., Ziemann V., October 2005. Investing in Hedge Funds: Adding Value through Active Style Allocation Decisions. Edhec Risk and Asset Management Research Centre
}
\author{ Brian G. Peterson }
\note{}
\seealso{
    \code{\link{CoKurtosis}}
    \code{\link[fBasics]{kurtosis}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
