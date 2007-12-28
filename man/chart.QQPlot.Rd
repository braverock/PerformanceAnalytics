\name{chart.QQPlot}
\alias{chart.QQPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper for qq.plot, with sensible defaults }
\description{
A wrapper to create a QQPlot chart of relative returns through time
}
\usage{
chart.QQPlot(R, distribution = "norm", ylab = NULL,
                 xlab = paste(distribution, "Quantiles"), main = NULL,
                 las = par("las"), envelope = FALSE, labels = FALSE,
                 col = c(1, 4), lwd = 2, pch = 1, cex = 1,
                 line = c("quartiles", "robust", "none"),
                 elementcolor = "lightgray", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{distribution}{ ~~Describe \code{distribution} here~~ }
  \item{xlab}{ set the x-axis label, as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, as in \code{\link{plot}} }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{las}{ ~~Describe \code{las} here~~ }
  \item{envelope}{ ~~Describe \code{envelope} here~~ }
  \item{labels}{ ~~Describe \code{labels} here~~ }
  \item{col}{ ~~Describe \code{col} here~~ }
  \item{lwd}{ set the line width, as in \code{\link{plot}} }
  \item{pch}{ symbols to use, see also \code{\link{plot}} }
  \item{cex}{ ~~Describe \code{cex} here~~ }
  \item{line}{ ~~Describe \code{line} here~~ }
  \item{elementcolor}{ provides the color for drawing less-important chart elements, such as the box lines, axis lines, etc. replaces \code{darken} }
  \item{\dots}{ any other passthru parameters }
}
\value{
QQ Plot
}
\references{ }
\author{ Peter Carl }
\seealso{
  \code{\link[stats]{qqplot}}

  \code{\link{plot}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
