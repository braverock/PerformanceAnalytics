`chart.RelativePerformance` <-
function (R, Rb, main = "Relative Performance", xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ylog = FALSE, ...)
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
    x = checkDataMatrix(R)
    y = checkDataMatrix(Rb)
    n = ncol(y)

    cumulativex = cumprod.column(1+x)
    cumulativey = cumprod.column(1+y)

    # Assumes that the first column is the one we want to compare to everything
    # in y.
    results = cumulativex[,1]/cumulativey[,1:n]

    # Removes the first color in the colorset to keep consistant with other graphics
    colorset = colorset[-1]

    chart.TimeSeries(results, xaxis = xaxis, main = main, legend.loc = legend.loc, col = colorset, ylog = ylog, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.RelativePerformance.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################