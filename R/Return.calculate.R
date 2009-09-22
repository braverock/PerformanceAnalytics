`Return.calculate` <-
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

    reclass(Returns,match.to=prices)

}

`CalculateReturns` <-
function(prices, method = c("compound","simple"))
{ # @ author Peter Carl
    Return.calculate(prices=prices, method=method)
}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.calculate.R,v 1.12 2009-09-22 02:43:57 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.11  2009-09-02 12:14:25  brian
# - convert to xts internally
# - use positive lag for lag.xts
# - add reclass to returned series
#
# Revision 1.10  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.9  2007/08/04 15:06:19  brian
# - change primary function name to Return.calculate
# - provide alias to old fn name CalculateReturns
# - rename in CVS
#
# Revision 1.8  2007/04/14 19:12:28  brian
# -lowercase first parameter prices
#
# Revision 1.7  2007/03/20 14:34:22  brian
# - restored CVS revision log data
#
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