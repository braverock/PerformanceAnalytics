`chart.CumReturns` <-
function (R, wealth.index = FALSE, legend.loc = NULL, colorset = (1:12), ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # Cumulates the returns given and draws a line graph of the results as
    # a cumulative return or a "wealth index".

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
    x = checkDataZoo(R)
    
    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)

    # Calculate the cumulative return
    one = 0
    if(!wealth.index)
        one = 1

    for(column in 1:columns) {
        column.Return.cumulative = cumprod(1+na.omit(x[,column])) - one
        if(column == 1)
            Return.cumulative = column.Return.cumulative
        else
            Return.cumulative = merge(Return.cumulative,column.Return.cumulative)
    }

    colnames(Return.cumulative) = columnnames

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
# $Id: chart.CumReturns.R,v 1.3 2007-03-10 05:08:04 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################