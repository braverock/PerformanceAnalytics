#' @export 
#' @rdname chart.Drawdown
Drawdowns <-
function (R, geometric = TRUE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculate the drawdown levels in a timeseries

    # FUNCTION:

    x = checkData(R)

    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)

    colDrawdown <- function(x, geometric) {
        if(geometric)
            Return.cumulative = cumprod(1+x)
        else
            Return.cumulative = 1+cumsum(x)
        maxCumulativeReturn = cummax(c(1,Return.cumulative))[-1]
        column.drawdown = Return.cumulative/maxCumulativeReturn - 1
    }

    for(column in 1:columns) {
	column.drawdown <- na.skip(x[,column],FUN=colDrawdown, geometric = geometric)

        if(column == 1)
            drawdown = column.drawdown
        else
            drawdown = merge(drawdown,column.drawdown)
    }

    colnames(drawdown) = columnnames
    drawdown = reclass(drawdown, x)
    return(drawdown)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
