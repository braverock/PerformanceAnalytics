`chart.Scatter` <-
function (x, y, reference.grid = TRUE, main = "Title", ylab=NULL, xlab=NULL, xlim = NA, ylim = NA, colorset = (1:12), symbolset = 1, darken = FALSE , legend.loc = NULL, ylog = FALSE, ...)
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
    # Draws a timeseries graph of type "line" with some sensible defaults.

    # FUNCTION:
    x = checkDataVector(x)
    y = checkDataVector(y)

    # Set color for key elements, easy to darken for the printer
    if(darken)
        elementcolor = "darkgray" #better for the printer
    else
        elementcolor = "lightgray" #better for the screen



    plot(x~y, main = main, pch = symbolset, ylog = ylog, col=colorset, ...)

    if(reference.grid)
        grid(col = elementcolor)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Scatter.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################