\name{VaR.CornishFisher}
\alias{VaR.CornishFisher}
\alias{VaR.traditional}
\alias{VaR.mean}
\alias{VaR.multicolumn}
\alias{modifiedVaR}
\title{ calculate various Value at Risk (VaR) measures }
\description{
These functions calculate both traditional mean-VaR and modified Cornish-Fisher VaR
}
\usage{
VaR.CornishFisher(R, p = 0.99, modified = TRUE, column = 1)
modifiedVaR(R, p = 0.99, modified = TRUE, column = 1)
VaR.traditional(R, p = 0.99, modified = FALSE, column = 1)
VaR.mean(R, p = 0.99, modified = FALSE, column = 1)
VaR.multicolumn(R, p = 0.99, modified = TRUE, firstcolumn = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{p}{ confidence level for calculation, default p=.99 }
  \item{modified}{ TRUE/FALSE default TRUE, use Cornish Fisher Expansion to take higher moments into account }
  \item{column}{ calculate VaR for what column? }
  \item{firstcolumn}{ for \code{\link{VaR.multicolumn}} start with which column in multicolumn data, default 1 }
}
\details{
The limitations of mean Value-at-Risk are well covered in the literature. The limitations of traditional mean-VaR are all related to the use of a symetrical distribution function.  Use of simulations, resampling, or Pareto distributions all help in making a more accurate prediction, but they are still flawed for assets with significantly non-normal (skewed or kurtotic) distributions. Laurent Favre and Jose-Antonio Galeano published a paper in the Fall 2002, volume 5 of the Journal of Alternative Investment, "Mean-Modified Value-at-Risk optimization With Hedge Funds", that proposed a modified VaR calculation that takes the higher moments of non-normal distributions (skewness, kurtosis) into account, and collapses to standard (traditional) mean-VaR if the return stream follows a standard distribution. This measure is now widely cited and used in the literature, and is usually referred to as "Modified VaR" or "Modified Cornish-Fisher VaR".

Favre and Galeano utilize modified VaR in a modified Sharpe Ratio as the return/risk measure for their portfolio optimization analysis, see \code{\link{SharpeRatio.modified}} for more information.

Cornish-Fisher VaR collapses to traditional mean-VaR when returns are normally distributed. As such, the \code{\link{VaR.mean}} and \code{\link{VaR.traditional}} functions are wrappers for the \code{VaR.CornishFisher} function.
}
\value{
VaR measure
}
\references{
Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002, v 5.
}
\author{ Brian G. Peterson }
\note{
The prototype of this function was completed by Diethelm Wuertz.  It calculated the Taylor expansion, but did not handle the confidence level,exponents for each moment, and scale.
}
\seealso{
    \code{\link{VaR.Marginal}}
    \code{\link{VaR.Beyond}}
    \code{\link{CVaR}}
    \code{\link{SharpeRatio.modified}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }