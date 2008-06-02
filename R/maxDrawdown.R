`maxDrawdown` <-
function (R)
{ # @author Peter Carl

    # DESCRIPTION:
    # To find the maximum drawdown in a return series, we need to first
    # calculate the cumulative returns and the maximum cumulative return to
    # that point.  Any time the cumulative returns dips below the maximum
    # cumulative returns, it's a drawdown.  Drawdowns are measured as a
    # percentage of that maximum cumulative return, in effect, measured from
    # peak equity.

    # FUNCTION:

    x = checkDataVector(R)


    Return.cumulative = cumprod(1+na.omit(x)) 
    maxCumulativeReturn = cummax(c(1,Return.cumulative))[-1]
    drawdown = Return.cumulative/maxCumulativeReturn - 1

    # if you want to see the drawdown series, plot(drawdown,type="l")
    return(min(drawdown))

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: maxDrawdown.R,v 1.4 2008-06-02 16:05:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/06/05 13:10:10  peter
# - fixed calculation for negative value in first month
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################