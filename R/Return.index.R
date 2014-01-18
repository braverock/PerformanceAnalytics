Return.index <-
function (R, wealth.index = TRUE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # Cumulates the returns given as a cumulative return or a "wealth index".

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # wealth.index:  if true, shows the "value of $1", starting the cumulation
    #    of returns at 1 rather than zero

    # Outputs:
    # A timeseries line chart of the cumulative return series

    # FUNCTION:

    # Transform input data to a matrix
    x = checkData(R)

    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)

    # Calculate the cumulative return
    one = 0
    if(!wealth.index)
        one = 1

    for(column in 1:columns) {
        column.Return.cumulative = na.skip(x[,column,drop=FALSE],FUN = function(x,one) {cumprod(1+x) - one},one=one)
        if(column == 1)
            Return.cumulative = column.Return.cumulative
        else
            Return.cumulative = merge(Return.cumulative,column.Return.cumulative)
    }
    if(columns == 1)
        Return.cumulative = as.xts(Return.cumulative)
    colnames(Return.cumulative) = columnnames
    reclass(Return.cumulative,match.to=x)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################