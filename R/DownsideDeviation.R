`DownsideDeviation` <-
function (Ra, MAR = 0, method=c("full","subset"))
{ # @author Peter Carl

    # DESCRIPTION:
    # Downside deviation, similar to semi deviation, eliminates positive returns
    # when calculating risk.  To calculate it, we take the returns that are less
    # than the target (or Minimum Acceptable Returns (MAR)) returns and take the
    # differences of those to the target.  We sum the squares and divide by the
    # total number of returns to get a below-target semi-variance.

    # This is also useful for calculating semi-deviation by setting
    # MAR = mean(x)

    method = method[1] # grab the first value if this is still a vector, to avoid varnings
    # FUNCTION:

    Ra = checkData(Ra, method="vector")
    r = subset(Ra,Ra < MAR)

    switch(method,
        full   = {len = length(Ra)},
        subset = {len = length(R)}
    ) # end switch
    return(sqrt(sum((r - MAR)^2)/len))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: DownsideDeviation.R,v 1.6 2007-08-03 14:58:26 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2007/06/21 21:36:08  brian
# - fixed to use length of entire series, per Platinga, van der Meer, Sortino 2001
#
# Revision 1.4  2007/06/21 21:24:39  brian
# - update to use length rather than length-1 after reviewing several original Sortino papers
#
# Revision 1.3  2007/03/14 00:54:06  brian
# - updates to parameters for standardization
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################