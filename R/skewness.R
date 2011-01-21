skewness <-
    function (x, na.rm = FALSE, method = c("moment", "fisher"), ...)
{
    # @author Diethelm Wuertz
    # @author Brian Peterson   (modify for PerformanceAnalytics)

    # Description:
    #   Returns the value of the skewness of a distribution function.

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

        # Skewness:
        n = length(x)
        if (is.integer(x)) x = as.numeric(x)

        # Selected Method:
        if (method == "moment") {
            skewness = sum((x-mean(x))^3/sqrt(var(x))^3)/length(x)
        }
        if (method == "fisher") {
            if (n < 3)
                skewness = NA
            else
                skewness = ((sqrt(n*(n-1))/(n-2))*(sum(x^3)/n))/((sum(x^2)/n)^(3/2))
        }

        skewness=array(skewness)
        if (column==1) {
            #create data.frame
            result=data.frame(skewness=skewness)
        } else {
            skewness=data.frame(skewness=skewness)
            result=cbind(result,skewness)
        }

    } #end columns loop

    if(ncol(result) == 1) {
        # some backflips to name the single column zoo object
        result = as.numeric(result)
    }
    else{
        colnames(result) = columnnames
        rownames(result) = "Skewness"
    }
    # Return Value:
    result

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