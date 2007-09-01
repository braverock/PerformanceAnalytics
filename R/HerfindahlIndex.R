`HerfindahlIndex` <-
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
        #break me, compute the squares
        Racf=Racf^2
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
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: HerfindahlIndex.R,v 1.2 2007-09-01 03:06:55 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/08/31 22:12:53  brian
# - initial revision of Herfindahl autocorrelation index
#
###############################################################################