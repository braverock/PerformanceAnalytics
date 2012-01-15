TrackingError <-
function (Ra, Rb, scale = NA)
{ # @author Peter Carl

    # DESCRIPTION
    # TrackingError = sqrt(sum(assetReturns.vec - benchmarkReturns.vec)^2 /
    #                   (length(assetReturns.vec) - 1)) * sqrt(scale)

    # Inputs:
    # Outputs:

    # FUNCTION
    Ra = checkData(Ra)
    Rb = checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

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

    te <-function (Ra, Rb, scale)
    {
        TE = sd(Return.excess(Ra, Rb), na.rm=TRUE) * sqrt(scale)
        return(TE)
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb, scale) te(Ra[,n[1]], Rb[,n[2]], scale), Ra = Ra, Rb = Rb, scale = scale)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Tracking Error:", colnames(Rb))
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