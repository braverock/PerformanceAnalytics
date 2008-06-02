`UpsidePotentialRatio` <-
function (Ra, MAR = 0)
{ # @author Brian G. Peterson

    # Description:
    # Sortino proposed to better account for skill and excess performance
    # by using only downside semivariance as the measure of risk.
    # UpsidePotentialRatio is an attempted improvement over the SortinoRatio

    # Ra    return vector
    # MAR   minimum acceptable return
    # Function:

    Ra = checkData(Ra, method = "vector")

    r = subset(Ra,Ra > MAR)
    return( ( sum(r - MAR)/(length(Ra)) )/ DownsideDeviation(Ra, MAR) )
}

`UPR`<-
function (Ra, MAR = 0)
{ # @author Brian G. Peterson
    UpsidePotentialRatio(Ra=Ra, MAR=MAR)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: UpsidePotentialRatio.R,v 1.2 2008-06-02 16:05:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/06/22 02:16:52  brian
# - initial commit of functions and documentation for Sortino's Upside
# Potential Ratio
#
###############################################################################