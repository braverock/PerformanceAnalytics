\name{charts.RollingPerformance}
\alias{charts.RollingPerformance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
charts.RollingPerformance(R, n = 12, rf = 0, main = NULL, trim = TRUE, event.labels = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{rf}{ ~~Describe \code{rf} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{trim}{ ~~Describe \code{trim} here~~ }
  \item{event.labels}{ ~~Describe \code{event.labels} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (R, n=12, rf = 0, main = NULL, trim = TRUE, event.labels = NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a rolling annualized returns chart, rolling annualized
    # standard deviation chart, and a rolling annualized sharpe ratio chart.

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
    x = checkDataMatrix(R)
    colnames = colnames(x)
    ncols = ncol(x)

    if(is.null(main))
        main = paste(colnames[1]," Rolling ",n,"-Month Performance",sep="")

    # First, we lay out the graphic as a three row, one column format
    plot.new()
    layout(matrix(c(1,2,3)),height=c(1,0.75,1),width=1)
    # to see the resulting layout, use layout.show(3)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    par(mar=c(1,4,4,2))

    if(ncols > 1)
        legend.loc = "topleft"
    else
        legend.loc = NULL


    # The first row is the annualized returns
    chart.RollingPerformance(R, n=n, main = main, xaxis = FALSE, ylab = "Annualized Return", trim = trim, FUN = "Return.annualized", legend.loc = legend.loc, , event.labels = event.labels, ...)

    # The second row is the annualized standard deviation
    par(mar=c(1,4,0,2))
    chart.RollingPerformance(R, n=n, main = "", xaxis = FALSE, ylab = "Annualized Standard Deviation", trim = trim, FUN = "StdDev.annualized", event.labels= NULL, ...)

    # The third row is the annualized SR
    par(mar=c(5,4,0,2))
    chart.RollingPerformance(R, n=n, main = "", ylab = "Annualized Sharpe Ratio", trim = trim, rf = rf, FUN = "SharpeRatio.annualized", event.labels= NULL, ...)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
