\name{chart.Histogram}
\alias{chart.Histogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ histogram of returns }
\description{
Create a histogram of returns, with optional curve fits for density and normal.  This is wrapper function for \code{\link[graphics]{hist}}, see the help for that function for additional arguments you may wish to pass in.
}
\usage{
chart.Histogram(R, rf = 0, breaks="FD", main = NULL, add.names = TRUE, xlab = "Returns", ylab = "Frequency", border.col = "white", add.density = TRUE, add.fit = TRUE, density.col = "black", fit.col = "darkgray", colorset = "gray", lwd = 2, xlim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{breaks}{how to calculate the breaks between the bars, see \code{\link[graphics]{hist}} for details, default "FD"}
  \item{add.names}{ TRUE/FALSE whether to add names from the source data to the graph }
  \item{add.density}{ TRUE/FALSE whether to add the density plot to the graph }
  \item{add.fit}{ TRUE/FALSE whether to add a fitted curve to the graph }
  \item{fit.col}{ number of columns to fit the data over }
  \item{border.col}{ colorset to use for the border }
  \item{density.col}{ colorset to use for the density plot }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, same as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis label, same as in \code{\link{plot}} }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{lwd}{ set the line width, same as in \code{\link{plot}} }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{\dots}{ any other passthru parameters }
}
\details{
}
\value{
chart of histogram of returns
}
\references{ }
\author{ Peter Carl }
\note{
Code inspired by a chart on:
\url{http://zoonek2.free.fr/UNIX/48_R/03.html}
}
\seealso{
  \code{\link[graphics]{hist}}
}
\examples{
    #first get some data
    data(edhec)
    #default chart is too busy
    chart.Histogram(edhec[,'Equity.Market.Neutral',drop=FALSE])

    # version with more breaks and the standard close fit density distribution
    chart.Histogram(edhec[,'Equity.Market.Neutral',drop=FALSE], breaks=40, methods = c("add.density", "add.rug") )

    chart.Histogram(edhec[,'Equity.Market.Neutral',drop=FALSE], methods = c( "add.centered") )

    # version with just the histogram and normal distribution centered on 0
    chart.Histogram(edhec[,'Equity.Market.Neutral',drop=FALSE], methods = c( "add.centered") )

    # version with histogram, normal, and close fit distribtuion
    chart.Histogram(edhec[,'Equity.Market.Neutral',drop=FALSE], methods = c( "add.centered", "add.density") )

    # add a rug to the previous plot for more granularity on precisely where the distribution fell
    chart.Histogram(edhec[,'Equity.Market.Neutral',drop=FALSE], methods = c( "add.centered", "add.density", "add.rug") )

    # now show a qqplot to give us another view on how normal the data are
    chart.Histogram(edhec[,'Equity.Market.Neutral',drop=FALSE], methods = c( "add.centered", "add.density", "add.rug", "add.qqplot") )

    # add risk measure(s) to show where those are in relation to observed returns
    chart.Histogram(edhec[,'Equity.Market.Neutral',drop=FALSE], methods = c("add.density", "add.centered", "add.rug", "add.risk") )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
