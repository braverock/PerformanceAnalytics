`Drawdowns` <-
function (R)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculate the drawdown levels in a timeseries

    # FUNCTION:

    x = checkData(R, method="zoo")

    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)

    for(column in 1:columns) {
        Return.cumulative = cumprod(1+na.omit(x[,column])) 
        maxCumulativeReturn = cummax(c(1,Return.cumulative))[-1]
        column.drawdown = Return.cumulative/maxCumulativeReturn - 1

        if(column == 1)
            drawdown = column.drawdown
        else
            drawdown = merge(drawdown,column.drawdown)
    }

    if(columns == 1) {# coersion required when only one column
        drawdown = as.matrix(drawdown)
        colnames(drawdown) = columnnames
        drawdown = zoo(drawdown, order.by = rownames(drawdown))
    }
    else
        colnames(drawdown) = columnnames

    return(drawdown)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Drawdowns.R,v 1.1 2007-03-21 14:09:10 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################