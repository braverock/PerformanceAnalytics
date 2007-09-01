`Return.Geltner` <-
function (Ra, ...)
{ # @author Brian G. Peterson

    # Description:
    # Geltner Returns came from real estate where they are used to uncover a
    # liquidity-adjusted return series.

    # Ra    return vector

    # Function:

    if (is.vector(Ra)) {
        # clean the data and get rid of NAs
        Ra = checkData (Ra,na.rm=TRUE, method="zoo", ...=...)
        # compute the lagged return series
        lagRa=lag(Ra, k=-1)
        # compute the first order autocorrelation
        f_acf=as.numeric(acf(Ra,plot=FALSE)[1][[1]])
        # now calculate and return the Geltner series
        geltner=(Ra-(lagRa*f_acf))/(1-f_acf)
        # put back in rownames, since those have been lost by now
        # rownames(geltner)=rownames(Ra)
        # return
        return(geltner)
    } else {
        apply(Ra, 2, Return.Geltner, ...=...)
    }

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.Geltner.R,v 1.1 2007-09-01 16:29:55 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################