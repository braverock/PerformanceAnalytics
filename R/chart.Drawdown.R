`chart.Drawdown` <-
function (R, geometric = TRUE, legend.loc = NULL, colorset = (1:12), ...)
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

    # Calculate drawdown level
    drawdown = Drawdowns(R, geometric)

    # workaround provided by Samuel Le to handle single-column input
    if(NCOL(R)==1)
    {
        drawdown<-as.xts(drawdown)
        colnames(drawdown)<-colnames(R)
    }
    
    # Chart the drawdown level
    chart.TimeSeries(drawdown, col = colorset, legend.loc = legend.loc, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.8  2009-08-31 20:51:27  brian
# - add new function na.skip to deal with non-contiguous NA's in data, may eventually go to xts
# - fix components of charts.PerformanceSummary to use na.skip
#
# Revision 1.7  2009-03-20 03:22:53  peter
# - added xts
#
# Revision 1.6  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.5  2007/03/13 03:57:37  peter
# - uses checkData function
#
# Revision 1.4  2007/03/10 19:46:35  peter
# - handles unequal periods
# - correctly calculates when first period is negative
#
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