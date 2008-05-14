\name{chart.Regression}
\alias{chart.Regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Takes a set of returns and relates them to a market benchmark in a scatterplot }
\description{
  Uses a scatterplot to display the relationship of a set of returns to a market benchmark.  Fits a linear model and overlays the resulting model.  Also overlays a Loess line for comparison.
}
\usage{
chart.Regression(Ra, Rb, Rf, excess.returns = FALSE, reference.grid = TRUE, main = "Title", ylab = NULL, xlab = NULL, xlim = NA, colorset = 1:12, symbolset = 1:12, darken = FALSE, legend.loc = NULL, ylog = FALSE, fit = c("loess", "linear", "conditional"), span = 2/3, degree = 1, family = c("symmetric", "gaussian"), ylim = NA, evaluation = 50, cex = .8, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector of returns to test, e.g., the asset to be examined }
  \item{Rb}{ a matrix, data.frame, or timeSeries of benchmark(s) to test the asset against }
  \item{Rf}{ risk free rate, in same period as the returns }
  \item{excess.returns}{ logical; should excess returns be used? }
  \item{reference.grid}{ if true, draws a grid aligned with the points on the x and y axes }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis title, same as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis title, same as in \code{\link{plot}} }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{colorset}{ color palette to use }
  \item{symbolset}{ symbols to use, see also 'pch' in \code{\link{plot}} }
  \item{darken}{ if true, draws the chart elements in "darkgray" rather than "gray".  Makes it easier to print for some printers }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{ylog}{ Not used }
  \item{fit}{ for values of "loess" or "linear", plots a line to fit the data }
  \item{span}{ passed to loess line fit, as in \code{\link{loess.smooth}} }
  \item{degree}{ passed to loess line fit, as in \code{\link{loess.smooth}} }
  \item{family}{ passed to loess line fit, as in \code{\link{loess.smooth}} }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{evaluation}{ passed to loess line fit, as in \code{\link{loess.smooth}} }
  \item{cex}{ set the cex size, same as in \code{\link{plot}} }
  \item{\dots}{ any other passthru parameters to \code{\link{plot}} }
}
\value{
  scatterplot with fitted lines
}
\references{ Chapter 7 of Ruppert(2004) gives an extensive overview of CAPM, its assumptions and deficiencies. }
\author{ Peter Carl }

\seealso{ \code{\link{plot}} }
\examples{
data(managers)
chart.Regression(managers[, 1:2, drop = FALSE], managers[, 8, drop = FALSE], Rf = managers[, 10, drop = FALSE], excess.returns = TRUE, fit = c("loess", "linear"), legend.loc = "topleft")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
