#' @rdname chart.RollingRegression
#' @export 
charts.RollingRegression = function (Ra, Rb, width = 12, Rf = 0, main = NULL, legend.loc = NULL, event.labels=NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a panel of RollingRegression charts that demonstrates
    # how the attributes change through time.

    # Inputs:
    # Ra: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest, the next
    #   columns are assumed to be relevant benchmarks for comparison.
    # Rb: a matrix, data frame, or timeSeries that is a set of returns of the
    #   same scale and periodicity as R.
    # Rf: the risk free rate.  Remember to set this to the same periodicity
    #   as the data being passed in.
    # attribute: Used to select the regression parameter to use in the chart  May
    #   be any of:
    #     Alpha - shows the y-intercept
    #     Beta - shows the slope of the regression line
    #     R-Squared - shows the fit of the regression to the data
    #

    # Outputs:
    # A stack of three related timeseries line charts

    # FUNCTION:

    columns.a = ncol(Ra)
    columns.b = ncol(Rb)

#     if(columns.a > 1 | columns.b > 1)
#         legend.loc = "topleft"
#     else
#         legend.loc = NULL

#    plot.new()

    op <- par(no.readonly=TRUE)

    layout(matrix(c(1,2,3)),heights=c(1.3,1,1.3),widths=1)

    par(mar=c(1,4,4,2))
    if(is.null(main)){
      freq = periodicity(Ra)

      switch(freq$scale,
          minute = {freq.lab = "minute"},
          hourly = {freq.lab = "hour"},
          daily = {freq.lab = "day"},
          weekly = {freq.lab = "week"},
          monthly = {freq.lab = "month"},
          quarterly = {freq.lab = "quarter"},
          yearly = {freq.lab = "year"}
      )

      main = paste("Rolling ",width,"-",freq.lab," Regressions", sep="")
    }

    chart.RollingRegression(Ra, Rb, width = width, Rf = Rf, attribute = "Alpha", xaxis = FALSE, main = main, ylab = "Alpha", legend.loc=legend.loc, event.labels = event.labels, ...)

    par(mar=c(1,4,0,2))

    chart.RollingRegression(Ra, Rb, width = width, Rf = Rf, attribute = "Beta", main = "", ylab = "Beta", xaxis = FALSE, event.labels = NULL, ...)

    par(mar=c(5,4,0,2))

    chart.RollingRegression(Ra, Rb, width = width, Rf = Rf, attribute = "R-Squared", main = "", ylab = "R-Squared", event.labels = NULL, ...)

    par(op)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################