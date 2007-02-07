\name{charts.PerformanceSummary}
\alias{charts.PerformanceSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
charts.PerformanceSummary(R, rf = 0, main = NULL, method = "ModifiedVaR", n = 0, event.labels = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{method}{ ~~Describe \code{method} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{event.labels}{ ~~Describe \code{event.labels} here~~ }
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
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (R, rf = 0, main = NULL, method = "ModifiedVaR", n = 0, event.labels = NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a wealth index chart, bars for monthly performance,
    # and underwater chart for drawdown.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest, the next
    #   columns are assumed to be relevant benchmarks for comparison.
    # rf: this is the risk free rate.  Remember to set this to the same
    #   periodicity as the data being passed in.
    # method: Used to select the risk parameter to use in the chart.BarVaR.  May
    #   be any of:
    #     modVaR - uses CF modified VaR
    #     VaR - uses traditional Value at Risk
    #     StdDev - monthly standard deviation of trailing 12 month returns
    #

    # Outputs:
    # A stack of three related timeseries line charts

    # FUNCTION:
    x = checkDataMatrix(R, na.rm = TRUE)
    colnames = colnames(x)
    ncols = ncol(x)

    if(ncols > 1)
        legend.loc = "topleft"
    else
        legend.loc = NULL

    if(is.null(main))
        main = paste(colnames[1]," Performance")

    # First, we lay out the graphic as a three row, one column format
    plot.new()
    layout(matrix(c(1,2,3)),height=c(2,1,1.3),width=1)
    # to see the resulting layout, use layout.show(3)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first row is the cumulative returns line plot
    par(mar=c(1,4,4,2))
    chart.CumReturns(x, main = main, xaxis = FALSE, ylab = "Cumulative Returns", legend.loc = legend.loc, event.labels = event.labels, ...)

    # The second row is the monthly returns bar plot
    par(mar=c(1,4,0,2))
#    chart.BarVaR(as.matrix(R[,1]), main = "", xaxis = FALSE, ylab = "Monthly Return", method = method)
    chart.BarVaR(x, main = "", xaxis = FALSE, n = n, ylab = "Monthly Return", method = method, event.labels = NULL, ...)

    # The third row is the underwater plot
    par(mar=c(5,4,0,2))
    chart.Drawdown(x, main = "", ylab = "From Peak", event.labels = NULL, ...)

    # If we wanted to add a fourth row with the table of monthly returns
    # Unfortunately, the textplot function doesn't provide a lot of control over
    # formatting.  Also, it requires the gplots package.
    #par(mar=c(0,0,0,0))
    #textplot(table.Returns(as.matrix(R)),cex=.7,cmar=1.5,rmar=0.5,halign="center", valign="center")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
