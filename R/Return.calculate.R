Return.calculate <-
function(prices, method = c("compound","simple"))
{ # @ author Peter Carl

    #  Calculate returns from a price stream

    # Required inputs

    # Prices: data object containing ordered price observations
    # method: "simple", "compound"

    # FUNCTION:

    method = method[1]
    pr = checkData(prices, method = "xts")

    if(method=="simple")
        Returns = pr/lag(pr,k=1) - 1

    if(method=="compound") {
        Returns = diff(log(pr))
    }

    reclass(Returns,match.to=pr)

}

CalculateReturns <-
function(prices, method = c("compound","simple"))
{ # @ author Peter Carl
    Return.calculate(prices=prices, method=method)
}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################