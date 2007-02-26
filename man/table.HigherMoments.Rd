\name{table.HigherMoments}
\alias{table.HigherMoments}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Higher Moments Summary: Statistics and Stylized Facts }
\description{
Summary of the higher moements and Co-Moments of the return distribution. Used to determine diversification potential.
Also called "systematic" moments by several papers.
}
\usage{
table.HigherMoments(x, y, scale = 12, rf = 0, digits = 4, method = "moment")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data for the x axis, can take matrix,vector, or timeseries }
  \item{y}{ data for the y axis, can take matrix,vector, or timeseries }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{digits}{ number of digits to round results to }
  \item{method}{ method to use when computing \code{\link{kurtosis}} one of: \code{excess}, \code{moment}, \code{fisher} }
}
\value{
table of summary statistics
}
\references{
Martellini L., Vaissie M., Ziemann V. Investing in Hedge Funds: Adding Value through Active Style Allocation Decisions. October 2005. Edhec Risk and Asset Management Research Centre.
}
\author{ Peter Carl }
\seealso{
    \code{\link{CoSkewness}} \cr
    \code{\link{CoKurtosis}} \cr
    \code{\link{BetaCoVariance}} \cr
    \code{\link{BetaCoSkewness}} \cr
    \code{\link{BetaCoKurtosis}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }