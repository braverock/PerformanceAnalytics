\name{chart.Boxplot}
\alias{chart.Boxplot}
\title{box whiskers plot wrapper}
\usage{
chart.Boxplot(R, horizontal = TRUE, names = TRUE, as.Tufte = FALSE,
  sort.by = c(NULL, "mean", "median", "variance"), colorset = "black",
  symbol.color = "red", mean.symbol = 1, median.symbol = "|",
  outlier.symbol = 1, show.data = NULL, add.mean = TRUE,
  sort.ascending = FALSE, xlab = "Return",
  main = "Return Distribution Comparison", element.color = "darkgray", ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{horizontal}{TRUE/FALSE plot horizontal (TRUE) or
  vertical (FALSE)}

  \item{names}{logical. if TRUE, show the names of each
  series}

  \item{as.Tufte}{logical. default FALSE. if TRUE use
  method derived for Tufte for limiting chartjunk}

  \item{sort.ascending}{logical.  If TRUE sort the
  distributions by ascending \code{sort.by}}

  \item{sort.by}{one of "NULL", "mean", "median",
  "variance"}

  \item{colorset}{color palette to use, set by default to
  rational choices}

  \item{symbol.color}{draws the symbols described in
  \code{mean.symbol},\code{median.symbol},\code{outlier.symbol}
  in the color specified}

  \item{mean.symbol}{symbol to use for the mean of the
  distribution}

  \item{median.symbol}{symbol to use for the median of the
  distribution}

  \item{outlier.symbol}{symbol to use for the outliers of
  the distribution}

  \item{show.data}{numerical vector of column numbers to
  display on top of boxplot, default NULL}

  \item{add.mean}{logical. if TRUE, show a line for the
  mean of all distributions plotted}

  \item{xlab}{set the x-axis label, same as in
  \code{\link{plot}}}

  \item{main}{set the chart title, same as in
  \code{\link{plot}}}

  \item{element.color}{specify the color of chart elements.
  Default is "darkgray"}

  \item{\dots}{any other passthru parameters}
}
\value{
box plot of returns
}
\description{
A wrapper to create box and whiskers plot with some
defaults useful for comparing distributions.
}
\details{
We have also provided controls for all the symbols and
lines in the chart. One default, set by
\code{as.Tufte=TRUE}, will strip chartjunk and draw a
Boxplot per recommendations by Edward Tufte. It can also be
useful when comparing several series to sort them in order
of ascending or descending "mean", "median", "variance" by
use of \code{sort.by} and \code{sort.ascending=TRUE}.
}
\examples{
data(edhec)
chart.Boxplot(edhec)
chart.Boxplot(edhec,as.Tufte=TRUE)
}
\author{
Peter Carl
}
\references{
Tufte, Edward R.  \emph{The Visual Display of Quantitative
Information}. Graphics Press. 1983. p. 124-129
}
\seealso{
\code{\link[graphics]{boxplot}}
}
\keyword{distribution}
\keyword{hplot}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

