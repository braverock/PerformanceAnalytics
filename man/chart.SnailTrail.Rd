\name{chart.SnailTrail}
\alias{chart.SnailTrail}
\title{chart risk versus return over rolling time periods}
\usage{
chart.SnailTrail(R, Rf = 0, main = "Annualized Return and Risk",
  add.names = c("all", "lastonly", "firstandlast", "none"),
  xlab = "Annualized Risk", ylab = "Annualized Return", add.sharpe = c(1,
  2, 3), colorset = 1:12, symbolset = 16, legend.loc = NULL,
  xlim = NULL, ylim = NULL, width = 12, stepsize = 12, lty = 1,
  lwd = 2, cex.axis = 0.8, cex.main = 1, cex.lab = 1, cex.text = 0.8,
  cex.legend = 0.8, element.color = "darkgray", ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rf}{risk free rate, in same period as your returns}

  \item{main}{set the chart title, same as in \code{plot}}

  \item{add.names}{plots the row name with the data point.
  default TRUE. Can be removed by setting it to NULL}

  \item{xlab}{set the x-axis label, as in
  \code{\link{plot}}}

  \item{ylab}{set the y-axis label, as in
  \code{\link{plot}}}

  \item{add.sharpe}{this draws a Sharpe ratio line that
  indicates Sharpe ratio levels of \code{c(1,2,3)}.  Lines
  are drawn with a y-intercept of the risk free rate and
  the slope of the appropriate Sharpe ratio level.  Lines
  should be removed where not appropriate (e.g.,
  sharpe.ratio = NULL).}

  \item{colorset}{color palette to use, set by default to
  rational choices}

  \item{symbolset}{from \code{pch} in \code{\link{plot}},
  submit a set of symbols to be used in the same order as
  the data sets submitted}

  \item{cex.legend}{The magnification to be used for sizing
  the legend relative to the current setting of 'cex'.}

  \item{element.color}{provides the color for drawing chart
  elements, such as the box lines, axis lines, etc. Default
  is "darkgray"}

  \item{legend.loc}{places a legend into one of nine
  locations on the chart: bottomright, bottom, bottomleft,
  left, topleft, top, topright, right, or center.}

  \item{xlim}{set the x-axis limit, same as in
  \code{\link{plot}}}

  \item{ylim}{set the y-axis limit, same as in
  \code{\link{plot}}}

  \item{width}{number of periods to apply rolling
  calculations over, sometimes referred to as a 'window'}

  \item{stepsize}{the frequency with which to make the
  rolling calculation}

  \item{lty}{set the line type, same as in
  \code{\link{plot}}}

  \item{lwd}{set the line width, same as in
  \code{\link{plot}}}

  \item{cex.lab}{The magnification to be used for sizing
  the label relative to the current setting of 'cex',
  similar to \code{\link{plot}}.}

  \item{cex.main}{The magnification to be used for sizing
  the main chart relative to the current setting of 'cex',
  as in \code{\link{plot}}.}

  \item{cex.axis}{The magnification to be used for sizing
  the axis text relative to the current setting of 'cex',
  similar to \code{\link{plot}}.}

  \item{cex.text}{The magnification to be used for sizing
  the text relative to the current setting of 'cex',
  similar to \code{\link{plot}}.}

  \item{\dots}{any other passthru parameters}
}
\description{
A chart that shows rolling calculations of annualized
return and annualized standard deviation have proceeded
through time.  Lines and dots are darker for more recent
time periods.
}
\examples{
data(managers)
chart.SnailTrail(managers[,c("HAM2","SP500 TR"),drop=FALSE],
		width=36, stepsize=12,
		colorset=c('red','orange'),
		add.names="firstandlast",
		rf=.04/12,
		main="Trailing 36-month Performance Calc'd Every 12 Months")
}
\author{
Peter Carl
}
\references{
~put references to the literature/web site here ~
}
\seealso{
\code{\link{chart.RiskReturnScatter}}
}
\keyword{ts}

