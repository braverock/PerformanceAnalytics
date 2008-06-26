\name{chart.SnailTrail}
\alias{chart.SnailTrail}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ chart risk versus return over rolling time periods }
\description{
A chart that shows rolling calculations of annualized return and annualized standard deviation have proceeded through time.  Lines and dots are darker for more recent time periods.
}
\usage{
chart.SnailTrail(R, rf = 0, main = "Annualized Return and Risk", add.names = c("all", "lastonly", "firstandlast", "none"), xlab = "Annualized Risk", ylab = "Annualized Return", add.sharpe = c(1, 2, 3), colorset = NULL, symbolset = 16, darken = FALSE, legend.loc = NULL, xlim = NULL, ylim = NULL, width = 12, stepsize = 12, lty = 1, lwd = 2, cex.axis = 0.8, cex.main = 1, cex.lab = 0.9, cex.text = 0.8, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{add.names}{ plots the row name with the data point.  default TRUE. Can be removed by setting it to NULL }
  \item{xlab}{ set the x-axis label, as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, as in \code{\link{plot}} }
  \item{add.sharpe}{ this draws a Sharpe ratio line that indicates Sharpe ratio levels of \code{c(1,2,3)}.  Lines are drawn with a y-intercept of the risk free rate and the slope of the appropriate Sharpe ratio level.  Lines should be removed where not appropriate (e.g., sharpe.ratio = NULL). }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{symbolset}{ from \code{pch} in \code{\link{plot}},  submit a set of symbols to be used in the same order as the data sets submitted }
  \item{darken}{ if true, draws the chart elements in "darkgray" rather than "gray".  Makes it easier to print for some printers. }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{width}{ number of periods to apply rolling calculations over, sometimes referred to as a 'window' }
  \item{stepsize}{ the frequency with which to make the rolling calculation }
  \item{lty}{ set the line type, same as in \code{\link{plot}} }
  \item{lwd}{ set the line width, same as in \code{\link{plot}} }
  \item{cex.axis}{ ~~Describe \code{cex.axis} here~~ }
  \item{cex.main}{ ~~Describe \code{cex.main} here~~ }
  \item{cex.lab}{ ~~Describe \code{cex.lab} here~~ }
  \item{cex.text}{ ~~Describe \code{cex.text} here~~ }
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
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ \code{\link{chart.RiskReturnScatter}}}
\examples{
data(managers)
chart.SnailTrail(managers[,c("HAM2","SP500.TR"),drop=FALSE], width=36, stepsize=12, colorset=c('red','orange'),add.labels="firstandlast", rf=.04/12, main="Trailing 36-month Performance Calc'd Every 12 Months")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
