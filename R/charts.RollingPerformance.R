#' rolling performance chart
#' 
#' A wrapper to create a rolling annualized returns chart, rolling annualized
#' standard deviation chart, and a rolling annualized sharpe ratio chart.
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param width number of periods to apply rolling function over
#' @param Rf risk free rate, in same period as your returns
#' @param main set the chart title, same as in \code{plot}
#' @param event.labels TRUE/FALSE whether or not to display lines and labels
#' for historical market shock events
#' @param legend.loc places a legend into one of nine locations on the chart:
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#' center.
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso \code{\link{chart.RollingPerformance}}
#' @keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(managers)
#' charts.RollingPerformance(managers[,1:8], Rf=managers[,10,drop=FALSE], colorset=tim8equal, main="Rolling 12-Month Performance", legend.loc="topleft")
#' 
#' @export 
charts.RollingPerformance <-
function (R, width = 12, Rf = 0, main = NULL, event.labels = NULL, legend.loc=NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a rolling annualized returns chart, rolling annualized
    # standard deviation chart, and a rolling annualized sharpe ratio chart.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest, the next
    #   columns are assumed to be relevant benchmarks for comparison.
    # Rf: this is the risk free rate.  Remember to set this to the same
    #   periodicity as the data being passed in.
    #

    # Outputs:
    # A stack of three related timeseries line charts

    # FUNCTION:
    x = checkData(R)
    colnames = colnames(x)
    ncols = ncol(x)

    if(is.null(main)){
      freq = periodicity(R)

      switch(freq$scale,
          minute = {freq.lab = "minute"},
          hourly = {freq.lab = "hour"},
          daily = {freq.lab = "day"},
          weekly = {freq.lab = "week"},
          monthly = {freq.lab = "month"},
          quarterly = {freq.lab = "quarter"},
          yearly = {freq.lab = "year"}
      )

      main = paste("Rolling",width,freq.lab, "Performance", sep=" ")
    }
    
    op <- par(no.readonly=TRUE)

    # First, we lay out the graphic as a three row, one column format
#    plot.new()
    layout(matrix(c(1,2,3)),heights=c(1,0.75,1),widths=1)
    # to see the resulting layout, use layout.show(3)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    par(mar=c(1,4,4,2))

    # The first row is the annualized returns
    chart.RollingPerformance(R, width = width, main = main, xaxis = FALSE, ylab = "Annualized Return", FUN = "Return.annualized", legend.loc = legend.loc, event.labels = event.labels, ...)

    # The second row is the annualized standard deviation
    par(mar=c(1,4,0,2))
    chart.RollingPerformance(R, width = width, main = "", xaxis = FALSE, ylab = "Annualized Standard Deviation", FUN = "StdDev.annualized", event.labels= NULL, ...)

    # The third row is the annualized SR
    par(mar=c(5,4,0,2))
    chart.RollingPerformance(R, width = width, main = "", ylab = "Annualized Sharpe Ratio", Rf = Rf, FUN = "SharpeRatio.annualized", event.labels= NULL, ...)

    par(op)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
