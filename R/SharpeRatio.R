SharpeRatio <-
function (R, Rf = 0, p = 0.95, FUN=c("StdDev", "VaR","ES"), weights=NULL, ...)
{ # @author Brian G. Peterson

    # DESCRIPTION:
    # The Sharpe ratio is simply the return per unit of risk (represented by
    # variability).  The higher the Sharpe ratio, the better the combined
    # performance of "risk" and return.

    # The Sharpe Ratio is a risk-adjusted measure of return that uses
    # standard deviation to represent risk.

    # A number of papers now recommend using a "modified Sharpe" ratio
    # using a Modified Cornish-Fisher VaR as the measure of Risk.

    # Inputs:
    # R: in this case, the function anticipates having a return stream as input,
    #    rather than prices.
    #
    # Rf: the risk free rate MUST be in the same periodicity as the data going in.
    #
    # p: probability at which to calculate the modified risk measure (defaults to 95%)

    # Outputs:
    # This function returns a (modified) Sharpe ratio for the same periodicity of the
    # data being input (e.g., monthly data -> monthly SR)

    # @TODO: annualize using multiperiod VaR and ES calcs

    # FUNCTION:

    R = checkData(R)
    
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    if(is.null(weights)) 
        xR = Return.excess(R, Rf)
    
    srm <-function (R, ..., Rf, p, FUNC)
    {
        FUNCT <- match.fun(FUNC)
        xR = Return.excess(R, Rf)
        SRM = mean(xR, na.rm=TRUE)/FUNCT(R=R, p=p, ...=..., invert=FALSE)
        SRM
    }
    
    i=1
    if(is.null(weights)){
        result = matrix(nrow=length(FUN), ncol=ncol(R)) 
        colnames(result) = colnames(R) 
    } 
    else {
        result = matrix(nrow=length(FUN))
    }
    
    tmprownames=vector()
    
    for (FUNCT in FUN){
        if (is.null(weights)){
            result[i,] = apply(R, MARGIN=2, FUN=srm, Rf=Rf, p=p, FUNC=FUNCT, ...)
        }
        else {
            result[i,] = weighted.mean(xR,w=weights,na.rm=TRUE)/match.fun(FUNCT)(R, Rf=Rf, p=p, weights=weights, portfolio_method="single", ...=...)
        }
        tmprownames = c(tmprownames, paste(FUNCT, " Sharpe: ", " (Rf=", round(mean(Rf)*100,1), "%, p=", round(p*100,1),"%)", sep=""))
        i=i+1 #increment counter
    }
    rownames(result)=tmprownames
    return (result)
}

SharpeRatio.modified <-
function (R, Rf = 0, p = 0.95, FUN=c("StdDev", "VaR","ES"), weights=NULL, ...) {
    .Deprecated("SharpeRatio", package="PerformanceAnalytics", "The SharpeRatio.modified function has been deprecated in favor of a newer SharpeRatio wrapper that will cover both the classic case and a larger suite of modified Sharpe Ratios.  This deprecated function may be removed from future versions")

    return(SharpeRatio(R = R, Rf = Rf, p = p, FUN = FUN, weights=weights, ...))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2011 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################