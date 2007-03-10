`chart.Drawdown` <-
function (R, legend.loc = NULL, colorset = (1:12), ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart demonstrating drawdowns from peak equity
    # attained through time.

    # To find the maximum drawdown in a return series, we need to first
    # calculate the cumulative returns and the maximum cumulative return to
    # that point.  Any time the cumulative returns dips below the maximum
    # cumulative returns, it's a drawdown.  Drawdowns are measured as a
    # percentage of that maximum cumulative return, in effect, measured from
    # peak equity.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # legend.loc: use this to locate the legend, e.g., "topleft".  If it's
    #   left as NULL, then no legend is drawn.
    # colorset: use the name of any of the palattes above.

    # Outputs:
    # A timeseries line chart of the drawdown series

    # FUNCTION:

    # Transform input data to a matrix
    x = checkDataZoo(R)

    # Calculate drawdown level
#     Return.cumulative = cumprod.column(1+x)
#     maxCumulativeReturn = cummax.column(Return.cumulative)
#     drawdown = Return.cumulative/maxCumulativeReturn - 1
    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)
#
    for(column in 1:columns) {
        Return.cumulative = cumprod(1+na.omit(x[,column])) 
        maxCumulativeReturn = cummax(c(1,Return.cumulative))[-1]
        column.drawdown = Return.cumulative/maxCumulativeReturn - 1

        if(column == 1)
            drawdown = column.drawdown
        else
            drawdown = merge(drawdown,column.drawdown)
    }

    colnames(drawdown) = columnnames

    # Chart the drawdown level
    chart.TimeSeries(drawdown, col = colorset, legend.loc = legend.loc, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Drawdown.R,v 1.4 2007-03-10 19:46:35 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/02/26 13:50:57  brian
# - change cumMax.column to cummax.column to reflect change in function name to pass "R CMD check"
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################