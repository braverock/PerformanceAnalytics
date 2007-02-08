`CoSkewness` <-
function(Ri, Ra, na.rm=FALSE)
{ # @author Brian G. Peterson

    # Description:
    # CoSkewness is the product of the third higher moments of two assets,
    # as defined in
    # Martellini L. and Ziemann V., 2005,
    # Marginal Impacts on Portfolio Distributions,
    # Working Paper, Edhec Risk and Asset Management Research Centre
    # and in:
    # Martellini L., Vaissie M., Ziemann V., October 2005,
    # Investing in Hedge Funds:
    #   Adding Value through Active Style Allocation Decisions
    # Edhec Risk and Asset Management Research Centre

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

    # CoSkewness of two assets
    CoS = sum((Ri - mean(Ri))*((Ra-mean(Ra))^2))

    result = CoS

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
# $Id: CoSkewness.R,v 1.3 2007-02-08 21:38:52 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################