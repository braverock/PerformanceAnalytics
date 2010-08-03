SharpeRatio.annualized <-
function (R, Rf = 0, scale = NA, geometric=TRUE)
{ # @author Peter Carl

    # DESCRIPTION:

    # Using an annualized Sharpe Ratio is useful for comparison.  The annualized
    # Sharpe ratio is computed by dividing the annualized mean monthly excess
    # return by the annualized monthly standard deviation of excess return.

    # @todo: monthly standard deviation of ***excess*** return

    # Inputs:
    # R: in this case, the function anticipates having a return stream as input,
    #     rather than prices.
    # Rf: the risk free rate MUST be in the same periodicity as the data going in.

    # FUNCTION:
    R = checkData(R)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    if(is.na(scale)) {
        freq = periodicity(R)
        switch(freq$scale,
            minute = {stop("Data periodicity too high")},
            hourly = {stop("Data periodicity too high")},
            daily = {scale = 252},
            weekly = {scale = 52},
            monthly = {scale = 12},
            quarterly = {scale = 4},
            yearly = {scale = 1}
        )
    }

    sr <-function (R, Rf, scale)
    {
        xR = Return.excess(R, Rf)
        SR = Return.annualized(xR, scale=scale, geometric=geometric)/StdDev.annualized(R, scale=scale)
        SR
    }

    result = apply(R, 2, sr, Rf=Rf, scale=scale)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = paste("Annualized Sharpe Ratio (Rf=", round(mean(Rf)*scale*100,1), "%)", sep="")
    return (result)

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