\name{chart.RiskReturnScatter}
\alias{chart.RiskReturnScatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ scatter chart of returns vs risk for comparing multiple instruments }
\description{
A wrapper to create a scatter chart of annualized returns versus annualized risk (standard deviation) for comparing manager performance. Also puts a box plot into the margins to help identify the relative performance quartile.
}
\usage{
chart.RiskReturnScatter(R, rf = 0, main = "Annualized Return and Risk", add.names = TRUE, xlab = "Annualized Risk", ylab = "Annualized Return", method = "calc", add.sharpe = c(1, 2, 3), add.boxplots = FALSE, colorset = 1, symbolset = 1, darken = FALSE, legend.loc = NULL, xlim = NULL, ylim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{add.names}{ plots the row name with the data point.  default TRUE. Can be removed by setting it to NULL }
  \item{xlab}{ set the x-axis label, as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, as in \code{\link{plot}} }
  \item{method}{ Used to select the risk parameter to use in the chart.BarVaR.  May be any of: \cr
    modVaR - uses CF modified VaR \cr
    VaR - uses traditional Value at Risk \cr
    StdDev - monthly standard deviation of trailing 12 month returns
  }
  \item{add.sharpe}{ this draws a Sharpe ratio line that indicates Sharpe ratio levels of \code{c(1,2,3)}.  Lines are drawn with a y-intercept of the risk free rate and the slope of the appropriate Sharpe ratio level.  Lines should be removed where not appropriate (e.g., sharpe.ratio = NULL). }
  \item{add.boxplots}{ TRUE/FALSE adds a boxplot summary of the data on the axis }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{symbolset}{ from \code{pch} in \code{\link{plot}},  submit a set of symbols to be used in the same order as the data sets submitted }
  \item{darken}{ if true, draws the chart elements in "darkgray" rather than "gray".  Makes it easier to print for some printers. }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
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
http://zoonek2.free.fr/UNIX/48_R/03.html
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
