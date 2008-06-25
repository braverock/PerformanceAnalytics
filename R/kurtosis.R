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

    x=checkData(x)

    # Warnings:
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(as.numeric(NA))}

    # Remove NAs:
    if (na.rm) x = x[!is.na(x)]

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

    # Add Control Attribute:
    attr(kurtosis, "method") <- method

    # Return Value:
    kurtosis
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: kurtosis.R,v 1.1 2008-06-25 13:50:42 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $