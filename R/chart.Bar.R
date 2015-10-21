#' wrapper for barchart of returns
#' 
#' A wrapper to create a chart of periodic returns in a bar chart.  This is a
#' difficult enough graph to read that it doesn't get much use.  Still, it is
#' useful for viewing a single set of data.
#' 
#' This is really a wrapper for chart.TimeSeries, so several other attributes
#' can also be passed.
#' 
#' Creates a plot of time on the x-axis and vertical lines for each period to
#' indicate value on the y-axis.
#' 
#' @aliases chart.Bar charts.Bar
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param legend.loc places a legend into one of nine locations on the chart:
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#' center
#' @param colorset color palette to use, set by default to rational choices
#' @param cex.legend sets the legend text size, such as in
#' \code{\link{chart.TimeSeries}}
#' @param cex.main sets the title text size, such as in
#' \code{\link{chart.TimeSeries}}
#' @param main sets the title text, such as in \code{\link{chart.TimeSeries}}
#' @param \dots any other passthru parameters, see \code{plot}
#' @author Peter Carl
#' @seealso \code{\link{chart.TimeSeries}} \cr \code{\link{plot}}
###keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(edhec)
#' chart.Bar(edhec[,"Funds of Funds"], main="Monthly Returns")
#' 
#' @export
chart.Bar <- function (R, legend.loc = NULL, colorset = (1:12), ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of monthly returns in a bar chart.  This is
    # a difficult enough graph to read that it doesn't get much use.  Still,
    # it is useful for viewing a single set of data.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns

    # Outputs:
    # A timeseries bar chart of the data series

    # FUNCTION:

    # Transform input data to a matrix
    x = checkData(R)

    chart.TimeSeries(x, type = "h", colorset = colorset, legend.loc = legend.loc, lend="butt",...)

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
