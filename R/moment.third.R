`moment.third` <-
function(R,na.rm=FALSE)
{ # @author Brian G. Peterson

    # Description:
    # The third mathematical moment of the return function.
    # Favre and Renaldo use this as separate from skewness in developing a
    # four-moment CAPM model
    #
    # as defined in:
    # Favre, L. and Renaldo, A., October 2003
    # How to Price Hedge Funds: From Two- to Four-Moment CAPM
    # UBS and Edhec Business School

    # Setup

    R = as.vector(R)

    if(na.rm) {
        R <- R[!is.na(R)]
    }

    # FUNCTION:

    S = (mean((R-mean(R)^3)))^(1/3)

    result = S

    # Return Value:
    result
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Rsk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: moment.third.R,v 1.3 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.2  2007/02/07 12:32:46  brian
# - rename parameter Ri to R for consistency with other functions
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################