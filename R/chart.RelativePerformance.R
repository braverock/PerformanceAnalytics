#' relative performance chart between multiple return series
#' 
#' Plots a time series chart that shows the ratio of the cumulative performance
#' for two assets at each point in time and makes periods of under- or
#' out-performance easier to see.
#' 
#' To show under- and out-performance through different periods of time, a time
#' series view is more helpful. The value of the chart is less important than
#' the slope of the line. If the slope is positive, the first asset (numerator)
#' is outperforming the second, and vice versa. May be used to look at the
#' returns of a fund relative to each member of the peer group and the peer
#' group index. Alternatively, it might be used to assess the peers
#' individually against an asset class or peer group index.
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param main set the chart title, same as in \code{plot}
#' @param xaxis if true, draws the x axis
#' @param colorset color palette to use, set by default to rational choices
#' @param elementcolor provides the color for drawing less-important chart
#' elements, such as the box lines, axis lines, etc. replaces \code{darken}
#' @param legend.loc places a legend into one of nine locations on the chart:
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#' center.
#' @param ylog TRUE/FALSE set the y-axis to logarithmic scale, similar to
#' \code{\link{plot}}, default FALSE
#' @param cex.legend the magnification to be used for sizing the legend
#' relative to the current setting of 'cex'.
#' @param lty set the line type, same as in \code{\link{plot}}
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso \code{\link{Return.relative}}
###keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(managers)
#' chart.RelativePerformance(managers[, 1:6, drop=FALSE], 
#' 		managers[, 8, drop=FALSE], 
#' 		colorset=rich8equal, legend.loc="bottomright", 
#' 		main="Relative Performance to S&P")
#' 
#' @export
chart.RelativePerformance <-
function (Ra, Rb, main = "Relative Performance", xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ylog = FALSE, elementcolor = "darkgray", lty = 1, cex.legend=.7, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # Rb: a matrix, data frame, or timeSeries of returns for a benchmark

    # Outputs:
    # A timeseries line chart of the calculated series

    # FUNCTION:

    # Transform input data to a matrix
    Ra = checkData(Ra)
    Rb = checkData(Rb)

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        for(column.b in 1:columns.b) { # against each asset passed in as Rb
            merged.columns = merge(Ra[, column.a, drop = FALSE], Rb[, column.b, drop = FALSE])
            cumulative = cumprod(1+na.omit(merged.columns))
            column.calc = cumulative[,1,drop=FALSE]/cumulative[,2,drop=FALSE]
            colnames(column.calc) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = "/")
            if(column.a == 1 & column.b == 1)
                Result.calc = column.calc
            else
                Result.calc = merge(Result.calc,column.calc)
        }
    }
columnnames = colnames(Result.calc)
    chart.TimeSeries(Result.calc, xaxis = xaxis, main = main, colorset = colorset, ylog = ylog, lty = lty, ...)
    abline(h=1,col=elementcolor)
    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = cex.legend, border.col = elementcolor, lty = lty, lwd = 2, bg = "white", legend = columnnames)
    }
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
