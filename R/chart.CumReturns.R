`chart.CumReturns` <-
function (R, wealth.index = FALSE, legend.loc = NULL, colorset = (1:12), ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper function that cumulates the returns given and draws a line graph
    # of the results as a "wealth index".

    # Inputs:
    # R = a matrix, data frame, or timeSeries of returns
    # wealth.index = if true, shows the "value of $1", starting the cumulation
    #    of returns at 1 rather than zero
    # legend.loc: use this to locate the legend, e.g., "topright"
    # colorset: use the name of any of the palattes above

    # Outputs:
    # A timeseries line chart of the cumulative return series

    # FUNCTION:

    # Transform input data to a matrix
    x = checkDataMatrix(R)

#     # Get dimensions and labels
#     columns = ncol(x)
#     rows = nrow(x)
#     columnnames = colnames(x)
#     rownames = rownames(x)

    # Calculate the cumulative return
    if(!wealth.index)
        Return.cumulative = cumprod.column(1+x) - 1
    else
        Return.cumulative = cumprod.column(1+x)

    # Chart the cumulative returns series
    chart.TimeSeries(Return.cumulative, col = colorset, legend.loc = legend.loc, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.CumReturns.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################