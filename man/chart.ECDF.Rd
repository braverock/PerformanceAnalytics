\name{chart.ECDF}
\alias{chart.ECDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create an ECDF overlaid with a CDF }
\description{
   Creates an emperical cumulative distribution function (ECDF) overlaid with a cumulative distribution function (CDF)
}
\usage{
chart.ECDF(R, main = "Empirical CDF", xlab = "x", ylab = "F(x)", colorset = c("black", "#005AFF"), lwd = 1, xlim = NULL, ylim = NULL, darken = FALSE, lty = c(1, 1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a matrix, data.frame, or timeSeries of returns }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, same as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis label, same as in \code{\link{plot}} }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{lwd}{ set the line width, same as in \code{\link{plot}} }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{darken}{ if true, draws the chart elements in "darkgray" rather than "gray".  Makes it easier to print for some printers. }
  \item{lty}{ set the line type, same as in \code{\link{plot}} }
  \item{\dots}{ any other passthru parameters }
}
\details{
  Inspired by a chart in Ruppert (2004) which is the most useful example of this type of chart we are aware of.
}
\value{
ECDF chart overlaid with a CDF
}
\references{
Ruppert, David. \emph{Statistics and Finance, an Introduction}. Springer. 2004. Ch. 2 Fig. 2.5

\url{http://www.stat.tamu.edu/~ljin/Finance/chapter2/Fig2_5.txt}
}
\author{ Peter Carl }
\seealso{
   \code{\link{plot}}
}
\examples{
## ~~ Need Exampole
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
