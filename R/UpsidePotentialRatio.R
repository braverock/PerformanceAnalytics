UpsidePotentialRatio <-
function (R, MAR = 0, method=c("subset","full"))
{ # @author Brian G. Peterson

    # Description:
    # Sortino proposed to better account for skill and excess peRformance
    # by using only downside semivariance as the measure of risk.
    # UpsidePotentialRatio is an attempted improvement over the SortinoRatio

    # Ra    return vector
    # MAR   minimum acceptable return

    # Function:

    method = method[1] 

    if (is.vector(R)) {
        if(!is.null(dim(MAR)))
            MAR = mean(checkData(MAR, method = "vector"), rm.na=TRUE)
        r = subset(R, R > MAR)
        switch(method,
            full   = {len = length(R)},
            subset = {len = length(r)} #previously length(R)
        ) # end switch
        result = (sum(r - MAR)/len)/DownsideDeviation(R, MAR=MAR , method=method)
        return(result)
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, MARGIN = 2, UpsidePotentialRatio, MAR = MAR, method = method)
        dim(result) = c(1,NCOL(R))
        colnames(result) = colnames(R)
        rownames(result) = paste("Upside Potential (MAR = ",round(MAR*100,1),"%)", sep="")
        return(result)
    }
}

UPR<-
function (R, MAR = 0, method=c("subset","full"))
{ # @author Brian G. Peterson
    UpsidePotentialRatio(R=R, MAR=MAR, method=method)
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