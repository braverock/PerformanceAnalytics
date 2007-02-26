\name{chart.Histogram}
\alias{chart.Histogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ histogram of returns }
\description{
Create a histogram of returns, with optional curve fits for density and normal
}
\usage{
chart.Histogram(R, rf = 0, main = NULL, add.names = TRUE, xlab = "Returns", ylab = "Frequency", border.col = "white", add.density = TRUE, add.fit = TRUE, density.col = "black", fit.col = "darkgray", colorset = "gray", lwd = 2, xlim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{add.names}{ ~~Describe \code{add.names} here~~ }
  \item{xlab}{ set the x-axis label, as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, as in \code{\link{plot}} }
  \item{border.col}{ ~~Describe \code{border.col} here~~ }
  \item{add.density}{ ~~Describe \code{add.density} here~~ }
  \item{add.fit}{ ~~Describe \code{add.fit} here~~ }
  \item{density.col}{ ~~Describe \code{density.col} here~~ }
  \item{fit.col}{ ~~Describe \code{fit.col} here~~ }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{lwd}{ ~~Describe \code{lwd} here~~ }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{\dots}{ any other passthru parameters }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ }
\author{ Peter Carl }
\note{
Code inspired by a chart on:
\url{http://zoonek2.free.fr/UNIX/48_R/03.html}
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
