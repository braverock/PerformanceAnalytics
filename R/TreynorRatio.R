TreynorRatio <-
function (Ra, Rb, Rf = 0, scale = NA)
{ # @author Peter Carl

    # DESCRIPTION:
    #

    # FUNCTION:
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    if(is.na(scale)) {
        freq = periodicity(Ra)
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

    tr <-function (xRa, xRb, scale)
    {
        beta = CAPM.beta(xRa, xRb)
        TR = (Return.annualized(xRa, scale = scale))/beta
        TR
    }

    result = apply(pairs, 1, FUN = function(n, xRa, xRb, scale) tr(xRa[,n[1]], xRb[,n[2]], scale), xRa = xRa, xRb = xRb, scale = scale)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Treynor Ratio:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
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