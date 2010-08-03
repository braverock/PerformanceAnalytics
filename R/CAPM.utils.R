# This file exists to contain several related and small CAPM utility functions.
# CAPM.alpha and CAPM.beta could probably have gone in here too, but they're already in separate files

CAPM.CML.slope <- function (Rb, Rf = 0 )
{ #author Brian G. Peterson

    #the Capital Market Line slope is a wrapper for the Sharpe Ratio on the benchmark asset
    #
    # Rb = Return vector of the benchmark or market portfolio
    result = SharpeRatio(Rb,Rf,FUN="StdDev")
    names = colnames(Rb)
    rownames(result) = paste("Capital Market Line Slope:", names)
    return(result) 
}

CAPM.CML <- function (Ra, Rb, Rf = 0)
{ #@author Brian G. Peterson

    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    cml <-function (Ra, Rb, Rf)
    {
        CML = mean(Rf, na.rm=TRUE) + CAPM.CML.slope(Rb, Rf)*mean(Ra, na.rm=TRUE)
        return(CML)
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb, Rf) cml(Ra[,n[1]], Rb[,n[2]], Rf), Ra = Ra, Rb = Rb, Rf = Rf)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Capital Market Line:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

CAPM.RiskPremium <- function (Ra, Rf = 0)
{ #@author Brian G. Peterson

    Ra = checkData(Ra)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    xRa = Return.excess(Ra, Rf)
    result = apply(xRa, 2, mean, na.rm=TRUE)
    dim(result) = c(1,NCOL(Ra))
    colnames(result) = colnames(Ra)
    rownames(result) = paste("Risk Premium (Rf=", round(mean(Rf)*100,1),"%)", sep="")
    return (result)
}

CAPM.SML.slope <- function (Rb, Rf = 0)
{ #@author Brian G. Peterson

    result = 1/CAPM.RiskPremium(Rb, Rf)
    names = colnames(Rb)
    rownames(result) = paste("Security Market Line:", names)
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