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
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################