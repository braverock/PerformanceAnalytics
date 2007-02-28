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
  \item{firstcolumn}{ for \code{\link{VaR.multicolumn}} start with what column in multicolumn data, default 1 }
}
\details{
The limitations of mean Value-at-Risk are well covered in the literature. Laurent Favre and Jose-Antonio Galeano published a paper in the Fall 2002, volume 5 of the Journal of Alternative Investment, "Mean-Modified Value-at-Risk optimization With Hedge Funds", that proposed a modified VaR calculation that takes the higher moments of non-normal distributions (skewness, kurtosis) into account, and collapses to standard (traditional) mean-VaR if the return stream follows a standard distribution. This measure is now widely cited and used in the literature, and is usually referred to as "Modified VaR" or "Modified Cornish-Fisher VaR"

Cornish-Fisher VaR collapses to traditional mean-VaR when returns are normally distributed. As such, the VaR.mean and VaR.traditional functions are wrappers for the VaR.CornishFisher function.
}
\value{
VaR measure
}
\references{
Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk optimization With Hedge Funds. Journal of Alternative Investment, Fall 2002, v 5.
}
\author{ Brian G. Peterson }
\note{
The prototype of this function was completed by Diethelm Wuertz.  It calculated the Taylor expansion, but incorrectly handled the confidence level,exponents for each moment, and scale.
}
\seealso{
    \code{\link{VaR.Marginal}}
    \code{\link{VaR.Beyond}}
    \code{\link{CVaR}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }