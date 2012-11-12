#' Calculates the returns of an asset in excess of the given risk free rate
#' 
#' Calculates the returns of an asset in excess of the given "risk free rate"
#' for the period.
#' 
#' Ideally, your risk free rate will be for each period you have returns
#' observations, but a single average return for the period will work too.
#' 
#' Mean of the period return minus the period risk free rate
#' 
#' \deqn{\overline{(R_{a}-R_{f})}}{mean(Ra-Rf=0)}
#' 
#' OR
#' 
#' mean of the period returns minus a single numeric risk free rate
#' 
#' \deqn{\overline{R_{a}}-R_{f}}{mean(R)-rf}
#' 
#' Note that while we have, in keeping with common academic usage, assumed that
#' the second parameter will be a risk free rate, you may also use any other
#' timeseries as the second argument.  A common alteration would be to use a
#' benchmark to produce excess returns over a specific benchmark, as
#' demonstrated in the examples below.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rf risk free rate, in same period as your returns, or as a sinlge
#' digit average
#' @author Peter Carl
#' @references Bacon, Carl. \emph{Practical Portfolio Performance Measurement
#' and Attribution}. Wiley. 2004. p. 47-52
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' head(Return.excess(managers[,1,drop=FALSE], managers[,10,drop=FALSE]))
#' head(Return.excess(managers[,1,drop=FALSE], .04/12))
#' head(Return.excess(managers[,1:6], managers[,10,drop=FALSE]))
#' head(Return.excess(managers[,1,drop=FALSE], managers[,8,drop=FALSE]))
#' 
#' @export
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
        #indexseries=index(R)
        columnname.Rf=Rf
        #Rf=xts(rep(Rf, length(indexseries)),order.by=indexseries)
        Rf = reclass(rep(Rf,nrow(R)),R) #patch thanks to Josh to deal w/ TZ issue
    }

    ## prototype
    ## xts(apply(managers[,1:6],2,FUN=function(R,Rf,order.by) {xts(R,order.by=order.by)-Rf}, Rf=xts(managers[,10,drop=F]),order.by=index(managers)),order.by=index(managers))
    
    return.excess <- function (R,Rf)
    { # a function to be called by apply on the inner loop
        xR = coredata(as.xts(R)-Rf)
    }
    
    result = do.call(merge, lapply(1:NCOL(R), function(nc) R[,nc] - Rf)) # thanks Jeff!
    if (!is.matrix(result)) result = matrix(result, ncol=ncol(R))
    colnames(result) = paste(colnames(R), ">", columnname.Rf)
    result = reclass(result, R)

    # RESULTS:
    return(result)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
