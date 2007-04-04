`BetaCoKurtosis` <-
function( Ra, Ri, na.rm=FALSE, method=c("moment", "excess", "fisher") )
{ # @author Brian G. Peterson

    # Description:
    # Beta CoKurtosis is the beta of an asset to the kurtosis
    # of an initial portfolio.  Used to determine diversification potential.
    # Also called "systematic kurtosis" or "systematic cokurtosis" by several papers.
    #
    # as defined in
    # Martellini L., Vaissie M., Ziemann V., October 2005,
    # Investing in Hedge Funds:
    #   Adding Value through Active Style Allocation Decisions
    # Edhec Risk and Asset Management Research Centre


    # Ri = return vector of initial portfolio
    # Ra = return vector of asset being considered for addition to portfolio

    # Setup

    Ri = checkDataVector(Ri)
    Ra = checkDataVector(Ra)

    if(na.rm) {
        Ri <- Ri[!is.na(Ri)]
        Ra <- Ra[!is.na(Ra)]
    }

    # FUNCTION:

    # Beta CoKurtosis of two assets
    # ktB = CoKurtosis(Ra,Ri)/(sum((Ri-mean(Ri))^4))
    ktB = CoKurtosis(Ra,Ri)/kurtosis(Ra, na.rm ,method ) #method = c("excess", "moment", "fisher")

    result = ktB

    # Return Value:
    result
}

###############################################################################

`BetaCoK` <-
function( Ri, Ra, na.rm=FALSE, method=c("moment", "excess", "fisher") )
{
    # wrapper function with a shorter name
    result = BetaCoKurtosis(Ri, Ra, na.rm, method)
    # Return Value:
    result
}

###############################################################################

`SystematicKurtosis` <-
function( Ri, Ra, na.rm=FALSE, method=c("moment", "excess", "fisher") )
{
    # wrapper function with a shorter name
    result = BetaCoKurtosis(Ri, Ra, na.rm, method)
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
# $Id: BetaCoKurtosis.R,v 1.5 2007-04-04 00:23:01 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2007/03/11 16:58:07  brian
# - replace as.vector() with checkDataVector()
#
# Revision 1.3  2007/03/11 16:53:19  brian
# - add equations and text to documentation
# - standardize on Ra as the Return of the Asset
# - standardize on Ra as first argument where that wasn't previously true
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################