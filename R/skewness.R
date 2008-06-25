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

    # Warnings:
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(as.numeric(NA))}

    x = checkData(x, method="vector")

    # Remove NAs:
    if (na.rm) x = x[!is.na(x)]

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

    # Add Control Attribute:
    attr(skewness, "method") <- method

    # Return Value:
    skewness
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: skewness.R,v 1.1 2008-06-25 13:50:42 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $