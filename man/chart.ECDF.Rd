\name{chart.ECDF}
\alias{chart.ECDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create an ECDF overlaid with a Normal CDF }
\description{
   Creates an emperical cumulative distribution function (ECDF) overlaid with a cumulative distribution function (CDF)
}
\usage{
chart.ECDF(R, main = "Empirical CDF", xlab = "x", ylab = "F(x)", colorset = c("black", "#005AFF"), lwd = 1, xlim = NULL, ylim = NULL, lty = c(1, 1), element.color = "darkgray", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis label, same as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, same as in \code{\link{plot}} }
  \item{colorset}{ color palette to use, defaults to c("black", "\#005AFF"), where first value is used to color the step function and the second color is used for the fitted normal  }
  \item{lwd}{ set the line width, same as in \code{\link{plot}} }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{element.color}{ specify the color of chart elements.  Default is "darkgray" }
  \item{lty}{ set the line type, same as in \code{\link{plot}} }
  \item{\dots}{ any other passthru parameters to \code{\link{plot}} }
}
\details{
  The empirical cumulative distribution function (ECDF for short) calculates the fraction of observations less or equal to a given value.  The resulting plot is a step function of that fraction at each observation.  This function uses \code{ecdf} and overlays the CDF for a fitted normal function as well.  Inspired by a chart in Ruppert (2004).

}

\references{
Ruppert, David. \emph{Statistics and Finance, an Introduction}. Springer. 2004. Ch. 2 Fig. 2.5

\url{http://www.stat.tamu.edu/~ljin/Finance/chapter2/Fig2_5.txt}
}
\author{ Peter Carl }
\seealso{
   \code{\link{plot}}, \code{\link{ecdf}}

}

\examples{
data(edhec)
chart.ECDF(edhec[, 1, drop=FALSE])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
