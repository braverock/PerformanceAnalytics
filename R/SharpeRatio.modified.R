`SharpeRatio.modified` <-
function (R, rf = 0, p=0.95, scale=1)
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

    x = checkDataVector(R)
    return( (mean(x, na.rm = TRUE) - rf)*scale/(VaR.CornishFisher(x, p)*sqrt(scale)) )
    #return((mean(x) - rf)/sd(x))
}

`modSharpe` <-
function (R, rf = 0, p=0.95, scale=1)
{ # @author Brian G. Peterson

    # wrapper for SharpeRatio.modified
    return(SharpeRatio.modified(R, rf, p, scale))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: SharpeRatio.modified.R,v 1.2 2007-02-05 19:08:22 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged foles to version control
# Bug 890
#
###############################################################################