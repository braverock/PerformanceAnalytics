CAPM.beta <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a CAPM beta.

    # Inputs:
    # Ra: vector of returns for the asset being tested
    # Rb: vector of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as x and y.

    # Output:
    # 

    # FUNCTION:
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    beta <-function (xRa, xRb)
    {
        merged = as.data.frame(na.omit(cbind(xRa, xRb)))
        model.lm = lm(merged[,1] ~ merged[,2], merged)
        beta = coef(model.lm)[[2]]
        beta
    }

    result = apply(pairs, 1, FUN = function(n, xRa, xRb) beta(xRa[,n[1]], xRb[,n[2]]), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

CAPM.beta.bull <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a conditional CAPM beta for up markets.

    # Inputs:
    # Ra: time series of returns for the asset being tested
    # Rb: time series of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a time series
    #     of the same length as x and y.

    # Output:
    # Bear market beta

    # FUNCTION:
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    beta <-function (xRa, xRb)
    {
        merged = na.omit(merge(xRa, xRb))
        merged = as.data.frame(merged)
        colnames(merged) = c("xRa","xRb")
        model.lm = lm(xRa ~ xRb, merged, subset= (xRb > 0))
        beta = coef(model.lm)[[2]]
        beta
    }

    result = apply(pairs, 1, FUN = function(n, xRa, xRb) beta(xRa[,n[1]], xRb[,n[2]]), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Bull Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

CAPM.beta.bear <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a conditional CAPM beta for down markets 

    # Inputs:
    # Ra: time series of returns for the asset being tested
    # Rb: time series of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a time series
    #     of the same length as Ra and Rb.

    # Output:
    # Bear market beta

    # FUNCTION:
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    beta <-function (xRa, xRb)
    {
        merged = na.omit(merge(xRa, xRb))
        merged = as.data.frame(merged)
        colnames(merged) = c("xRa","xRb")
        model.lm = lm(xRa ~ xRb, merged, subset= (xRb < 0))
        beta = coef(model.lm)[[2]]
        beta
    }

    result = apply(pairs, 1, FUN = function(n, xRa, xRb) beta(xRa[,n[1]], xRb[,n[2]]), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Bear Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}


TimingRatio <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function calculates the ratio of the two conditional CAPM betas (up and down).

    beta.bull = CAPM.beta.bull(Ra, Rb, Rf = Rf)
    beta.bear = CAPM.beta.bear(Ra, Rb, Rf = Rf)
    result = beta.bull/beta.bear

    if(length(result) ==1)
        return(result)
    else {
        names = colnames(Rb)
        rownames(result) = paste("Timing Ratio:", names)
        return(result)
    }
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