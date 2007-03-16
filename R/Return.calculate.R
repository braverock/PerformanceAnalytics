`CalculateReturns` <-
function(Prices, method = "compound")
{ # @ author Peter Carl

    #  Calculate returns from a price stream

    # Required inputs

    # Prices:
    # method: "simple", "compound"

    # FUNCTION:

    prices = checkData(Prices, method = "zoo")

    if(method=="simple")
        Returns = prices/lag(prices,-1) - 1

    if(method=="compound") {
        Returns = diff(log(prices))
    }

    Returns

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.calculate.R,v 1.6 2007-03-16 14:01:14 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
#
###############################################################################
