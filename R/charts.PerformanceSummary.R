#' Create combined wealth index, period performance, and drawdown chart
#' 
#' For a set of returns, create a wealth index chart, bars for per-period
#' performance, and underwater chart for drawdown.
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rf risk free rate, in same period as your returns
#' @param p confidence level for calculation, default p=.95
#' @param main set the chart title, as in \code{plot}
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param methods Used to select the risk parameter of trailing \code{width}
#' returns to use in the \code{\link{chart.BarVaR}} panel: May be any of:
#' \itemize{ \item None - does not add a line, \item ModifiedVaR - uses
#' Cornish-Fisher modified VaR, \item GaussianVaR - uses traditional Value at
#' Risk, \item HistoricalVaR - calculates historical Value at Risk, \item
#' ModifiedES - uses Cornish-Fisher modified Expected Shortfall, \item
#' GaussianES - uses traditional Expected Shortfall, \item HistoricalES -
#' calculates historical Expected Shortfall, \item StdDev - per-period standard
#' deviation }
#' @param begin Align shorter series to: \itemize{ \item first - prior value of
#' the first column given for the reference or longer series or, \item axis -
#' the initial value (1 or zero) of the axis.  } passthru to
#' \code{\link{chart.CumReturns}}
#' @param event.labels TRUE/FALSE whether or not to display lines and labels
#' for historical market shock events
#' @param wealth.index if \code{wealth.index} is \code{TRUE}, shows the "value
#' of $1", starting the cumulation of returns at 1 rather than zero
#' @param width number of periods to apply rolling function window over
#' @param gap numeric number of periods from start of series to use to train
#' risk calculation
#' @param ylog TRUE/FALSE set the y-axis to logarithmic scale, similar to
#' \code{\link{plot}}, default FALSE
#' @param legend.loc sets the legend location in the top chart.  Can be set to
#' NULL or nine locations on the chart: bottomright, bottom, bottomleft, left,
#' topleft, top, topright, right, or center.
#' @param \dots any other passthru parameters
#' @note Most inputs are the same as "\code{\link{plot}}" and are principally
#' included so that some sensible defaults could be set.
#' @author Peter Carl
#' @seealso \code{\link{chart.CumReturns}} \cr \code{\link{chart.BarVaR}} \cr
#' \code{\link{chart.Drawdown}}
#' @keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(edhec)
#' charts.PerformanceSummary(edhec[,c(1,13)])
#' 
#' @export 
charts.PerformanceSummary <-
function (R, Rf = 0, main = NULL, geometric=TRUE, methods = "none", width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, gap = 12, begin=c("first","axis"), legend.loc="topleft", p=0.95,...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a wealth index chart, bars for monthly peRformance,
    # and underwater chart for drawdown.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest, the next
    #   columns are assumed to be relevant benchmarks for comparison.
    # Rf: this is the risk free rate.  Remember to set this to the same
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
    begin = begin[1]
    x = checkData(R)
    colnames = colnames(x)
    ncols = ncol(x)

# This repeats a bit of code from chart.CumReturns, but it's intended
# to align the start dates of all three charts.  Basically, it assumes
# that the first column in the list is the column of interest, and 
# starts everything from that start date

    length.column.one = length(x[,1])
# find the row number of the last NA in the first column
    start.row = 1
    start.index = 0
    while(is.na(x[start.row,1])){
        start.row = start.row + 1
    }
    x = x[start.row:length.column.one,]

    if(ncols > 1)
        legend.loc = legend.loc
    else
        legend.loc = NULL

    if(is.null(main))
        main = paste(colnames[1],"Performance", sep=" ")

    if(ylog)
        wealth.index = TRUE

    op <- par(no.readonly=TRUE)

    # First, we lay out the graphic as a three row, one column format
#    plot.new()
    layout(matrix(c(1,2,3)),heights=c(2,1,1.3),widths=1)
    # to see the resulting layout, use layout.show(3)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first row is the cumulative returns line plot
    par(mar=c(1,4,4,2))
    chart.CumReturns(x, main = main, xaxis = FALSE, legend.loc = legend.loc, event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, begin = begin, geometric = geometric, ylab="Cumulative Return",...)

    # The second row is the monthly returns bar plot
    par(mar=c(1,4,0,2))

    freq = periodicity(x)

    switch(freq$scale,
	seconds = { date.label = "Second"},
	minute = { date.label = "Minute"},
	hourly = {date.label = "Hourly"},
	daily = {date.label = "Daily"},
	weekly = {date.label = "Weekly"},
	monthly = {date.label = "Monthly"},
	quarterly = {date.label = "Quarterly"},
	yearly = {date.label = "Annual"}
    )

    chart.BarVaR(x, main = "", xaxis = FALSE, width = width, ylab = paste(date.label,"Return"), methods = methods, event.labels = NULL, ylog=FALSE, gap = gap, p=p, ...)

    # The third row is the underwater plot
    par(mar=c(5,4,0,2))
    chart.Drawdown(x, geometric = geometric, main = "", ylab = "Drawdown", event.labels = NULL, ylog=FALSE, ...)

    # If we wanted to add a fourth row with the table of monthly returns
    #par(mar=c(0,0,0,0))
    #textplot(table.Returns(as.matrix(R)),cex=.7,cmar=1.5,rmar=0.5,halign="center", valign="center")
    par(op)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
