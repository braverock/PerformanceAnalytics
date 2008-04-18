`chart.Scatter` <-
function (x, y, reference.grid = TRUE, main = "Title", ylab=NULL, xlab=NULL, xlim = NA, ylim = NA, colorset = 1, symbolset = 1, darken = FALSE , legend.loc = NULL, ylog = FALSE, cex.axis = 0.8, cex.legend = 0.8,...)
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
    # darken = if true, draws the chart elements in "darkgray" rather than
    #    "gray".  Makes it easier to print for some printers.


    # All other inputs are the same as "plot" and are principally included
    # so that some sensible defaults could be set.

    # Output:
    # Draws a scatter chart with some sensible defaults.

    # FUNCTION:
    x = checkData(x, method = "vector")
    y = checkData(y, method = "vector")

    # Set color for key elements, easy to darken for the printer
    if(darken)
        elementcolor = "darkgray" #better for the printer
    else
        elementcolor = "lightgray" #better for the screen

    # Add fit line

    plot(x~y, main = main, pch = symbolset, ylog = ylog, col=colorset, cex.axis = cex.axis, ...)

    if(reference.grid) {
        grid(col = elementcolor)
        abline(h = 0, col = elementcolor)
        abline(v = 0, col = elementcolor)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Scatter.R,v 1.5 2008-04-18 03:51:00 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2007/04/25 20:07:12  peter
# - added x-y axes
#
# Revision 1.3  2007/04/02 21:58:42  peter
# - changed colorset default
# - changed to use checkData
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################