HerfindahlIndex <-
function (Ra, ...)
{ # @author Brian G. Peterson

    # Description:

    # Ra    return vector

    # Function:

    if (is.vector(Ra)) {
        # clean the data and get rid of NAs
        Ra = checkData (Ra,na.rm=TRUE, method="vector", ...=...)
        # compute the simple acf
        Racf=acf(Ra,plot=FALSE)[[1]]
        # get the subset of positive acf values
        pos_acf=subset(Racf,Racf>=0)[-1]
        # scale the positive acf to get percent contribution to total positive acf
        scaled_acf=pos_acf/sum(pos_acf)
        # Herfindal Index is the sum of the squared percentage contributions
        return(sum(scaled_acf^2))
    } else {
        apply(Ra, 2, HerfindahlIndex, ...=...)
    }

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: HerfindahlIndex.R,v 1.3 2007/09/01 04:05:28 brian Exp $
#
###############################################################################