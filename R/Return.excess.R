`Return.excess` <-
function (R, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculates the returns of an asset in excess of the given 
    # "risk free rate" for the period.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # Rf: a measure of the risk free rate, whether a period average
    #     (a single number) or a timeseries vector

    # Outputs:
    # A timeseries of the calculated series

    # FUNCTION:

    # Transform input data to a timeseries (xts) object
    R = checkData(R)

    # if the risk free rate is delivered as a timeseries, we'll check it
    # and convert it to an xts object.
    if(!is.null(dim(Rf))){
        Rf = checkData(Rf)
        indexseries=index(cbind(R,Rf))
        columnname.Rf=colnames(Rf)
    }
    else {
        indexseries=index(R)
        columnname.Rf=Rf
        Rf=xts(rep(Rf, length(indexseries)),order.by=indexseries)
    }

    ## prototype
    ## xts(apply(managers[,1:6],2,FUN=function(R,Rf,order.by) {xts(R,order.by=order.by)-Rf}, Rf=xts(managers[,10,drop=F]),order.by=index(managers)),order.by=index(managers))
    
    return.excess <- function (R,Rf)
    { # a function to be called by apply on the inner loop
        xR = coredata(as.xts(R)-as.xts(Rf))
    }
    
    result = apply(R, MARGIN=2, FUN=return.excess, Rf=Rf)
    colnames(result) = paste(colnames(R), ">", columnname.Rf)
    result = reclass(result, R)

    # RESULTS:
    return(result)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.excess.R,v 1.19 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.18  2009-10-08 17:35:18  peter
# - modified to fix unequal length issue between R and Rf
#
# Revision 1.17  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.16  2009-09-24 18:00:48  peter
# - fixed to handle scalar Rf
#
# Revision 1.15  2009-09-24 17:46:31  peter
# - fixed column renaming
#
# Revision 1.14  2009-09-24 17:39:22  peter
# - fixed reclass
# - added column renaming
#
# Revision 1.13  2009-09-24 17:11:30  brian
# - convert to use apply
#
# Revision 1.12  2009-09-22 02:47:21  peter
# - added reclass
#
# Revision 1.11  2009-09-17 03:00:38  peter
# - reverting back to zoo until rollapply works for xts
#
# Revision 1.10  2009-09-15 20:35:50  peter
# - converted to use xts internally
#
# Revision 1.9  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.8  2007/09/26 02:54:58  peter
# - fixed labeling problem in multi-column asset results
#
# Revision 1.7  2007/08/14 23:20:07  peter
# - added conditional labeling to columns
#
# Revision 1.6  2007/08/14 21:37:05  peter
# - removed support for multiple columns in Rf
# - now works for numeric Rf
#
# Revision 1.5  2007/08/14 01:19:40  peter
# - function handles multiple columns for both R and Rf
#
# Revision 1.4  2007/05/15 19:47:38  peter
# - handles multiple column objects
#
# Revision 1.3  2007/03/16 13:59:20  peter
# - added cvs footer
#
#
###############################################################################
