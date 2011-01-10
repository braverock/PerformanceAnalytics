CAPM.alpha <- function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a CAPM alpha.

    # Inputs:
    # R: vector of returns for the asset being tested
    # Rb: vector of returns for the benchmark the asset is being gauged against
    # R and Rb are assumed to be matching periods
    # Rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as R and y.

    # Output:
    # CAPM alpha

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

    alpha <-function (xRa, xRb)
    {
        merged = as.data.frame(na.omit(cbind(xRa, xRb)))
        model.lm = lm(merged[,1] ~ merged[,2], merged)
        alpha = coef(model.lm)[[1]]
        alpha
    }

    result = apply(pairs, 1, FUN = function(n, xRa, xRb) alpha(xRa[,n[1]], xRb[,n[2]]), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Alpha:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
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