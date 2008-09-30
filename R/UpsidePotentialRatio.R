`UpsidePotentialRatio` <-
function (Ra, MAR = 0, method=c("subset","full"))
{ # @author Brian G. Peterson

    # Description:
    # Sortino proposed to better account for skill and excess performance
    # by using only downside semivariance as the measure of risk.
    # UpsidePotentialRatio is an attempted improvement over the SortinoRatio

    # Ra    return vector
    # MAR   minimum acceptable return

    method = method[1] # grab the first value if this is still a vector, to avoid varnings

    # Function:

    Ra = checkData(Ra, method = "vector")

    if(!is.null(dim(MAR)))
        MAR = mean(checkData(MAR, method = "vector"))

    r = subset(Ra,Ra > MAR)

    switch(method,
        full   = {len = length(Ra)},
        subset = {len = length(r)} #previously length(R)
    ) # end switch

    return( ( sum(r - MAR)/len )/ DownsideDeviation(Ra, MAR=MAR , method=method) )
}

`UPR`<-
function (Ra, MAR = 0, method=c("subset","full"))
{ # @author Brian G. Peterson
    UpsidePotentialRatio(Ra=Ra, MAR=MAR, method=method)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: UpsidePotentialRatio.R,v 1.3 2008-09-30 21:17:24 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.2  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.1  2007/06/22 02:16:52  brian
# - initial commit of functions and documentation for Sortino's Upside
# Potential Ratio
#
###############################################################################