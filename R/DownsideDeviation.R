DownsideDeviation <-
function (R, MAR = 0, method=c("subset","full"), potential=FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # Downside deviation, similar to semi deviation, eliminates positive returns
    # when calculating risk.  To calculate it, we take the returns that are less
    # than the target (or Minimum Acceptable Returns (MAR)) returns and take the
    # differences of those to the target.  We sum the squares and divide by the
    # total number of returns to get a below-target semi-variance.

    # This is also useful for calculating semi-deviation by setting
    # MAR = mean(x)

    method = method[1] 

    if (is.vector(R)) {
        R = na.omit(R)

        r = subset(R, R < MAR)

        if(!is.null(dim(MAR))){
            if(is.timeBased(index(MAR))){
                MAR <-MAR[index(r)] #subset to the same dates as the R data
            } else{
                MAR = mean(checkData(MAR, method = "vector"))
                # we have to assume that Ra and a vector of Rf passed in for MAR both cover the same time period
            }   
        }
        
        switch(method,
            full   = {len = length(R)},
            subset = {len = length(r)} #previously length(R)
        ) # end switch
        p=2
        if(potential) p=1 # calculates downside potential instead
        result = sqrt(sum((r - MAR)^p)/len)
        return(result)
    }
    else {
        R = checkData(R)
        result = apply(R, MARGIN = 2, DownsideDeviation, MAR = MAR, method = method)
        result<-t(result)
        colnames(result) = colnames(R)
        if(potential)
            rownames(result) = paste("Downside Potential (MAR = ", round(mean(MAR)*100,1),"%)", sep="")
        else
            rownames(result) = paste("Downside Deviation (MAR = ", round(mean(MAR)*100,1),"%)", sep="")
        return(result)
    }
}

DownsidePotential <-
function (R)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function is just a wrapper of DownsideDeviation with
    # MAR = mean(x) and potential = TRUE
    # see below

    # FUNCTION:

    if (is.vector(R)) {
        R = na.omit(R)
        return(DownsideDeviation(R, MAR=mean(R), method="full", potential=TRUE))
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, 2, DownsidePotential)
        result = matrix(result, nrow=1)
        colnames(result) = colnames(R)
        rownames(result) = paste("Downside Potential (MAR = ", round(mean(MAR)*100,1),"%)", sep="")
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