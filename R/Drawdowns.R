`Drawdowns` <-
function (R)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculate the drawdown levels in a timeseries

    # FUNCTION:

    x = checkData(R)

    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)

    colDrawdown <- function(x) {
        Return.cumulative = cumprod(1+x) 
        maxCumulativeReturn = cummax(c(1,Return.cumulative))[-1]
        column.drawdown = Return.cumulative/maxCumulativeReturn - 1
    }

    for(column in 1:columns) {
	column.drawdown <- na.skip(x[,column],FUN=colDrawdown)

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
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Drawdowns.R,v 1.6 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2009-09-22 02:56:13  peter
# - added reclass
#
# Revision 1.4  2009-08-31 20:51:27  brian
# - add new function na.skip to deal with non-contiguous NA's in data, may eventually go to xts
# - fix components of charts.PerformanceSummary to use na.skip
#
# Revision 1.3  2009-06-02 03:14:49  peter
# - converted internal to xts, removed zoo coersion
#
# Revision 1.2  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.1  2007/03/21 14:09:10  peter
# - separated function from the chart.Drawdowns.R
#
###############################################################################