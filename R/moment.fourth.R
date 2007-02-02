`moment.fourth` <-
function(Ri,na.rm=FALSE)
{ # @author Brian G. Peterson

    # Description:
    # The fourth mathematical moment of the return function.
    # Favre and Renaldo use this as separate from kurtosis in developing a
    # four-moment CAPM model
    #
    # as defined in:
    # Favre, L. and Renaldo, A., October 2003
    # How to Price Hedge Funds: From Two- to Four-Moment CAPM
    # UBS and Edhec Business School

    # Setup

    Ri = as.vector(Ri)

    if(na.rm) {
        Ri <- Ri[!is.na(Ri)]
    }

    # FUNCTION:

    K = (mean((Ri-mean(Ri)^4)))^(1/4)

    result = K
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
# $Id: moment.fourth.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################