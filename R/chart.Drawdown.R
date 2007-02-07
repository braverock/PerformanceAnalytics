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
    x = checkDataMatrix(R)

    # Calculate drawdown level
    Return.cumulative = cumprod.column(1+x)
    maxCumulativeReturn = cumMax.column(Return.cumulative)
    drawdown = Return.cumulative/maxCumulativeReturn - 1

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
# $Id: chart.Drawdown.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################