InformationRatio <-
function (Ra, Rb, scale = NA)
{ # @author Peter Carl

    # DESCRIPTION
    # InformationRatio = ActivePremium/TrackingError

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

    ir <-function (Ra, Rb, scale)
    {
        ap = ActivePremium(Ra, Rb, scale = scale)
        te = TrackingError(Ra, Rb, scale = scale)
        IR = ap/te
        return(IR)
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb, scale) ir(Ra[,n[1]], Rb[,n[2]], scale), Ra = Ra, Rb = Rb, scale = scale)

    if(length(result) ==1)
        return(result)
    else {
        result = matrix(result, ncol=Ra.ncols, nrow=Rb.ncols, byrow=TRUE)
        rownames(result) = paste("Information Ratio:", colnames(Rb))
        colnames(result) = colnames(Ra)
        return(result)
    }
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