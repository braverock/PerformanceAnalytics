Return.excess <-
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
    if (!is.matrix(result)) result = matrix(result, ncol=ncol(R))
    colnames(result) = paste(colnames(R), ">", columnname.Rf)
    result = reclass(result, R)

    # RESULTS:
    return(result)
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