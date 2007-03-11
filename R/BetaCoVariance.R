`BetaCoVariance` <-
function(Ra, Ri, na.rm=FALSE)
{ # @author Brian G. Peterson

    # Description:
    # Beta covariance is the beta of an asset to the variance and covariance
    # of an initial portfolio.  Used to determine diversification potential.
    # also called "systematic beta" by several papers
    #
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

    # CovarianceBeta of two assets
    covB = cov(Ra,Ri)/var(Ri)

    # can also be written as:
    # covB = sum((Ri - mean(Ri))*(Ra-mean(Ra)))/sum(Ri -mean(Ri))^2
    # I've used the full symbolic equation here for linkage into the
    # systematic skewness and systematic kurtosis functions, which use
    # the same equation varying only in exponent/moment

    result = covB

    # Return Value:
    result
}

###############################################################################

`BetaCoV` <-
function(Ri, Ra, na.rm=FALSE)
{
    # wrapper function with a shorter name
    result = BetaCoVariance(Ri, Ra, na.rm)
    # Return Value:
    result
}

###############################################################################

`SystematicBeta` <-
function(Ri, Ra, na.rm=FALSE)
{
    # wrapper function with a shorter name
    result = BetaCoVariance(Ri, Ra, na.rm)
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
# $Id: BetaCoVariance.R,v 1.4 2007-03-11 16:53:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/02/22 18:25:42  brian
# - add comments about symbolic notation
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################