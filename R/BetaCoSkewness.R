`BetaCoSkewness` <-
function(Ri, Ra, na.rm=FALSE)
{ # @author Brian G. Peterson

    # Description:
    # Beta CoSkewness is the beta of an asset to the skewness
    # of an initial portfolio.  Used to determine diversification potential.
    # also called "systematic skewness" or "systematic co-skewness"
    # by several papers.
    # as defined in
    # Favre, L. and Renaldo, A., October 2003
    # How to Price Hedge Funds: From Two- to Four-Moment CAPM
    # UBS and Edhec Business School
    # Equation [5] p. 10

    # Ri = return vector of initial portfolio
    # Ra = return vector of asset being considered for addition to portfolio

    # Setup

    Ri = as.vector(Ri)
    Ra = as.vector(Ra)

    if(na.rm) {
        Ri <- Ri[!is.na(Ri)]
        Ra <- Ra[!is.na(Ra)]
    }

    # FUNCTION:

    # systematic skewness of two assets
    skB = CoSkewness(Ra,Ri)/(mean(Ri-mean(Ri)^3))

    result = skB

    # Return Value:
    result
}

###############################################################################

`BetaCoS` <-
function(Ri, Ra, na.rm=FALSE)
{
    # wrapper function with a shorter name
    result = BetaCoSkewness(Ri, Ra, na.rm)
    # Return Value:
    result
}

###############################################################################

`SystematicSkewness` <-
function(Ri, Ra, na.rm=FALSE)
{
    # wrapper function with a shorter name
    result = BetaCoSkewness(Ri, Ra, na.rm)
    # Return Value:
    result
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: BetaCoSkewness.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################