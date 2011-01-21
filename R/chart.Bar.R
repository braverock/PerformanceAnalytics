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
# Copyright (c) 2004-2011 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################