\name{chart.Boxplot}
\alias{chart.Boxplot}
\title{box whiskers plot wrapper}
\usage{
chart.Boxplot(R, horizontal = TRUE, names = TRUE, as.Tufte = FALSE,
  as.Notch = FALSE, sort.by = NULL, sort.base = NULL,
  sort.ascending = FALSE, colorset = "black", symbol.color = "red",
  mean.symbol = 1, median.symbol = "|", outlier.symbol = 1,
  show.data = NULL, add.mean = TRUE, xlab = "Return",
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

  \item{as.Notch}{logical. default FALSE. if TRUE a notch
  is drawn in each side of the boxes. See
  \code{\link[graphics]{boxplot}}}

  \item{sort.by}{one of the return or risk measure
  c("NULL", "mean", "median", "variance", "sharp ratio",
  "mean absolute deviation", "std dev", "sterling ratio",
  "calmar ratio", "burke ratio", "pain index", "ulcer
  index","martin ratio", "downside risk", "omega ratio",
  "sortino ratio", "upside risk","upside potential ratio",
  "omega sharpe ratio"). default is "NULL".}

  \item{sort.base}{one of the return or risk metrics as
  listed in \code{sort.by}, add the base order number next
  to the labels sorted by \code{sort.by}}

  \item{sort.ascending}{logical.  If TRUE sort the
  distributions by ascending \code{sort.by} and
  \code{sort.base}}

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
Boxplot per recommendations by Edward Tufte. Another
default, set by \code{as.Notch=TRUE}, will draw a notch in
each side of the boxes.  It can also be useful when
comparing several series to sort them in the order of
ascending or descending return or risk measurement by use
of \code{sort.by} and \code{sort.ascending=TRUE}. In
addition, one can compare this with base order, e.g., to
see the relative change of the orders of the series between
the two metrics used in \code{sort.by} and \code{sort.base}
respectively.
}
\examples{
data(edhec)
chart.Boxplot(edhec)
chart.Boxplot(edhec,as.Tufte=TRUE)
chart.Boxplot(R=edhec,sort.by="upside risk",
		horizontal=TRUE, sort.base="std dev",
		sort.ascending=TRUE)
}
\author{
Peter Carl

Ke Li \email{kirkli@stat.washington.edu}
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

