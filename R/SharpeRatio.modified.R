`SharpeRatio.modified` <-
function (R, Rf = 0, p = 0.95, ...)
{ # @author Brian G. Peterson

    # DESCRIPTION:
    # The Sharpe ratio is simply the return per unit of risk (represented by
    # variability).  The higher the Sharpe ratio, the better the combined
    # performance of "risk" and return.

    # The Sharpe Ratio is a risk-adjusted measure of return that uses
    # standard deviation to represent risk.

    # A number of papers now recommend using a "modified Sharpe" ratio
    # using a Modified Cornish-Fisher VaR as the measure of Risk.

    # Inputs:
    # R: in this case, the function anticipates having a return stream as input,
    #    rather than prices.
    #
    # rf: the risk free rate MUST be in the same periodicity as the data going in.
    #
    # p: probability at which to calculate the modified VaR (defaults to 95%)

    # Outputs:
    # This function returns a modified Sharpe ratio for the same periodicity of the
    # data being input (e.g., monthly data -> monthly SR)

    # FUNCTION:

    R = checkData(R)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    srm <-function (R, Rf, p, ...)
    {
        xR = Return.excess(R, Rf)
        SRM = mean(xR, na.rm=TRUE)/VaR(R, p, invert=FALSE, ...)
        SRM
    }

    result = apply(R, 2, srm, Rf=Rf, p=p, ...)
    return (result)
}

`modSharpe` <-
function (R, Rf = 0, p = 0.95, ...)
{ # @author Brian G. Peterson

    # wrapper for SharpeRatio.modified
    return(SharpeRatio.modified(R, Rf, p, ...))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: SharpeRatio.modified.R,v 1.7 2009-10-01 02:41:35 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.6  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.5  2007/04/02 21:54:42  peter
# - modified to use CheckData
# - modified to use Return.excess
#
# Revision 1.4  2007/03/14 00:54:06  brian
# - updates to parameters for standardization
#
# Revision 1.3  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.2  2007/02/05 19:08:22  brian
# - add modSharpe wrapper function
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################