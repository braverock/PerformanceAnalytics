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
# $Id: Return.calculate.R,v 1.7 2007-03-20 14:34:22 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.6  2007/03/16 14:01:14  peter
# - added cvs footer
#
# Revision 1.5 2007-03-16 03:47 peter
# - returns a zoo object rather than a data frame
# - uses zoo functions to simplify calculations
#
# Revision 1.4 2007-03-14 02:39 peter
# - made "compound" default method
#
# Revision 1.3 2007-03-04 08:59 brian
# - minor changes to pass R CMD check
#
# Revision 1.2 2007-02-27 04:37 peter
# - fixed row labeling problem
#
# Revision 1.1 2007-02-27 10:01 peter
# - added function to cvs
###############################################################################