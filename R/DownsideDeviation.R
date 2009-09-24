`DownsideDeviation` <-
function (R, MAR = 0, method=c("subset","full"))
{ # @author Peter Carl

    # DESCRIPTION:
    # Downside deviation, similar to semi deviation, eliminates positive returns
    # when calculating risk.  To calculate it, we take the returns that are less
    # than the target (or Minimum Acceptable Returns (MAR)) returns and take the
    # differences of those to the target.  We sum the squares and divide by the
    # total number of returns to get a below-target semi-variance.

    # This is also useful for calculating semi-deviation by setting
    # MAR = mean(x)

    method = method[1] 

    if (is.vector(R)) {
        R = na.omit(R)

        if(!is.null(dim(MAR)))
            MAR = mean(checkData(MAR, method = "vector"))
        # we have to assume that Ra and a vector of Rf passed in for MAR both cover the same time period

        r = subset(R, R < MAR)

        switch(method,
            full   = {len = length(R)},
            subset = {len = length(r)} #previously length(R)
        ) # end switch
        return(sqrt(sum((r - MAR)^2)/len))
    }
    else {
        R = checkData(R, method = "matrix")
        apply(R, MARGIN = 2, DownsideDeviation, MAR = MAR, method = method)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: DownsideDeviation.R,v 1.11 2009-09-24 03:35:59 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.10  2008-09-30 21:17:24  brian
# - both DownsideDeviation and UpsidePotentialRatio now support "method argument to use full or subset of series
# - use subset as default method
# - updated documentation to reflect change
#
# Revision 1.9  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.8  2007/10/11 03:22:04  peter
# - fixed "subset" to use r instead of R
#
# Revision 1.7  2007/08/16 14:09:33  peter
# - added checkData for MAR in case it is a vector of Rf
#
# Revision 1.6  2007/08/03 14:58:26  brian
# - add use of length of full series or subset below MAR
# - set proper values for SemiVariance(subset), and SemiDeviation(full)
# - allow DownsideDeviation user to choose, default method="full"
#
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