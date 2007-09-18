\name{chart.Histogram}
\alias{chart.Histogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ histogram of returns }
\description{
Create a histogram of returns, with optional curve fits for density and normal.  This is wrapper function for \code{\link[graphics]{hist}}, see the help for that function for additional arguments you may wish to pass in.
}
\usage{
chart.Histogram(R, breaks="FD", main = NULL, xlab = "Returns", ylab = "Frequency", methods = c(NULL,"add.density", "add.normal", "add.centered", "add.rug", "add.risk", "add.qqplot"), colorset = c("lightgray", "#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000"), border.col = "white", box.col = "white", lwd = 2, xlim = NULL, ylim = NULL, darken = FALSE, note.lines = NULL, note.labels = NULL, note.color = "darkgray", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{breaks}{how to calculate the breaks between the bars, one of:
    \itemize{
      \item a vector giving the breakpoints between histogram cells,
      \item a single number giving the number of cells for the histogram,
      \item a character string naming an algorithm to compute the number of cells (see Details),
      \item a function to compute the number of cells.
    }
    In the last three cases the number is a suggestion only.
    see \code{\link[graphics]{hist}} for details, default "FD"
  }
  \item{methods} { what to graph, one or more of:
    \itemize{
        \item "add.density" to display the density plot,
        \item "add.normal" to display a fitted normal distibution line over the mean,
        \item "add.centered" to display a fitted normal line over zero,
        \item "add.rug" to display a rug of the observations,
        \item "add.risk" to display common risk metrics,
        \item "add.qqplot" to display a small qqplot in the upper corner of the histogram plot
    }
  }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, same as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis label, same as in \code{\link{plot}} }
  \item{ylim}{ set the y-axis limits, same as in \code{\link{plot}} }
  \item{border.col}{ color to use for the border }
  \item{box.col}{ color to use for the box }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{lwd}{ set the line width, same as in \code{\link{plot}} }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{darken}{ if true, draws the chart elements in "darkgray" rather than "gray".  Makes it easier to print for some printers. }
  \item{note.lines}{ draws a vertical line through the value given. }
  \item{note.labels}{ adds a text label to vertical lines specified for note.lines. }
  \item{note.color}{ specifies the color(s) of the vertical lines drawn. }
  \item{\dots}{ any other passthru parameters }
}
\details{
  The default for \code{breaks} is \code{"FD"}. Other names for which algorithms
  are supplied are \code{"Sturges"} (see
  \code{\link{nclass.Sturges}}), \code{"Scott"}, and \code{"FD"} /
  \code{"Freedman-Diaconis"} (with corresponding functions
  \code{\link{nclass.scott}} and \code{\link{nclass.FD}}).
  Case is ignored and partial matching is used.
  Alternatively, a function can be supplied which
  will compute the intended number of breaks as a function of \code{R}.
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
    #default chart was too busy
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
