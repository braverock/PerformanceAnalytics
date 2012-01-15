kurtosis <-
    function (x, na.rm = FALSE, method = c("excess", "moment", "fisher"), ...)
{
    # @author Diethelm Wuertz
    # @author Brian Peterson   (modify for PerformanceAnalytics)

    # Description:
    #   Returns the value of the kurtosis of a distribution function.

    # Details:
    #   Missing values can be handled.

    # FUNCTION:

    # Method:
    method = match.arg(method)

    R=checkData(x,method="matrix")

    columns = ncol(R)
    columnnames=colnames(R)
    # FUNCTION:
    for(column in 1:columns) {
        x = as.vector(na.omit(R[,column]))
        #x = R[,column]

        if (!is.numeric(x)) stop("The selected column is not numeric")

        # Remove NAs:
        if (na.rm) x = x[!is.na(x)]

        # Warnings:
        if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
            warning("argument is not numeric or logical: returning NA")
            return(as.numeric(NA))}


        # Kurtosis:
        n = length(x)
        if (is.integer(x)) x = as.numeric(x)
        if (method == "excess") {
            kurtosis = sum((x-mean(x))^4/var(x)^2)/length(x) - 3
        }
        if (method == "moment") {
            kurtosis = sum((x-mean(x))^4/var(x)^2)/length(x)
        }
        if (method == "fisher") {
            kurtosis = ((n+1)*(n-1)*((sum(x^4)/n)/(sum(x^2)/n)^2 -
                (3*(n-1))/(n+1)))/((n-2)*(n-3))
        }
        kurtosis=array(kurtosis)
        if (column==1) {
            #create data.frame
            result=data.frame(kurtosis=kurtosis)
        } else {
            kurtosis=data.frame(kurtosis=kurtosis)
            result=cbind(result,kurtosis)
        }
    } #end columns loop

    if(ncol(result) == 1) {
        # some backflips to name the single column zoo object
        result = as.numeric(result)
    }
    else{
        colnames(result) = columnnames
        rownames(result) = "Excess Kurtosis"
    }
    # Return Value:
    result

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