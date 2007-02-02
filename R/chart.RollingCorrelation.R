`chart.RollingCorrelation` <-
function (R, Rb, n=12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), trim = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of rolling performance metrics in a line chart

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # FUN: any function that can be evaluated using a single set of returns
    #   (e.g., rolling beta won't work, but Return.annualizeds will)

    # Outputs:
    # A timeseries line chart of the calculated series

    # FUNCTION:

    # Transform input data to a matrix
    x = checkDataMatrix(R)
    y = checkDataMatrix(Rb)

    chart.TimeSeries(rollingCorrelation(x = x, y = y, n = n, trim = trim), xaxis = xaxis, col = colorset, legend.loc = legend.loc, ylim = c(-1,1), ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.RollingCorrelation.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################