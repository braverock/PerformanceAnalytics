#' wrapper to draw scatter plot with sensible defaults
#' 
#' Draws a scatter chart.  This is another chart "primitive", since it only
#' contains a set of sensible defaults.
#' 
#' 
#' @param x data for the x axis, can take matrix,vector, or timeseries
#' @param y data for the y axis, can take matrix,vector, or timeseries
#' @param reference.grid if true, draws a grid aligned with the points on the x
#' and y axes
#' @param main set the chart title, same as in \code{plot}
#' @param ylab set the y-axis label, as in \code{\link{plot}}
#' @param xlab set the x-axis label, as in \code{\link{plot}}
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param colorset color palette to use, set by default to rational choices
#' @param symbolset from \code{pch} in \code{\link{plot}}, submit a set of
#' symbols to be used in the same order as the data sets submitted
#' @param element.color provides the color for drawing chart elements, such as
#' the box lines, axis lines, etc. Default is "darkgray"
#' @param cex.lab The magnification to be used for x- and y-axis labels
#' relative to the current setting of 'cex'
#' @param cex.axis The magnification to be used for axis annotation relative to
#' the current setting of 'cex', same as in \code{\link{plot}}.
#' @param cex.main The magnification to be used for the main title relative to
#' the current setting of 'cex'.
#' @param cex.legend The magnification to be used for sizing the legend
#' relative to the current setting of 'cex'.
#' @param \dots any other passthru parameters
#' @note Most inputs are the same as "\code{\link{plot}}" and are principally
#' included so that some sensible defaults could be set.
#' @author Peter Carl
#' @seealso \code{\link{plot}}
###keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(edhec)
#' chart.Scatter(edhec[,1],edhec[,2])
#' 
#' @export
chart.Scatter <-
function (x, y, reference.grid = TRUE, main = "Title", ylab=NULL, xlab=NULL, xlim = NA, ylim = NA, colorset = 1, symbolset = 1, element.color = "darkgray", cex.axis = 0.8, cex.legend = 0.8, cex.lab = 1, cex.main = 1, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # Draws a scatter chart.  This is another chart "primitive", since it
    # only contains a set of sensible defaults.  This function is
    # intended to be used in a wrapper that is written for a particular purpose.
    # This is just a handy way to standardize the formatting of multiple charts.

    # Inputs:
    # x and y = assumes that data is a regular time series, not irregular.  Can take
    # any type of object, whether a matrix, data frame, or timeSeries.
    # legend.loc = use this to locate the legend, e.g., "topright"
    # colorset = use the name of any of the palattes above
    # reference.grid = if true, draws a grid aligned with the points on the
    #    x and y axes.

    # All other inputs are the same as "plot" and are principally included
    # so that some sensible defaults could be set.

    # Output:
    # Draws a scatter chart with some sensible defaults.

    # FUNCTION:
    x = checkData(x, method = "vector") ### ???
    y = checkData(y, method = "vector")

    # pass in: cex.axis = cex.axis, cex.main = cex.main, cex.lab = cex.lab
    plot(y~x, main = main, pch = symbolset, col=colorset, ...)

    if(reference.grid) {
        grid(col = element.color)
        abline(h = 0, col = element.color)
        abline(v = 0, col = element.color)
    }

    rug(side=1, x, col = element.color)
    rug(side=2, y, col = element.color)

    box(col = element.color)

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
