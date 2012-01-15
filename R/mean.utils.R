# collection of mean-related statistics
#
# NOTE: we would prefer that the first argument be R for returns,
#       but the package check complains about mismatch of the
#       first parameter with R command mean()

mean.geometric <-
function (x, ...)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the mean geometric return for a return series

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Returns the geometric return

    # FUNCTION:
    if (is.vector(x)) {
        x = na.omit(x)
        mean.geometric = exp(mean(log(1+x)))-1
        return(mean.geometric)
    }
    else {
        x = checkData(x, method = "matrix", ... = ...)
        result = apply(x, 2, mean.geometric, ... = ...)
        dim(result) = c(1,NCOL(x))
        colnames(result) = colnames(x)
        rownames(result) = "Geometric Mean"
        return(result)
    }
}

mean.stderr <-
function (x, ...)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the standard error of the mean for a return series

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Returns the standard error of the mean for the return

    # FUNCTION:
    if (is.vector(x)) {
        x = na.omit(x)
        stderr = sqrt(var(x)/length(x))
        return(stderr)
    }
    else {
        x = checkData(x, method = "matrix", ... = ...)
        result = apply(x, 2, mean.stderr, ... = ...)
        result = matrix(result, nrow=1)
        colnames(result) = colnames(x)
        rownames(result) = "Standard Error"
        return(result)
    }
}

mean.LCL <-
function (x, ci = 0.95, ...)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the standard error of the mean for a return series

    # Inputs:
    # R: Assumes returns rather than prices
    # ci: Confidence interval

    # Output:
    # Uses the standard error of the mean to calculate a lower bound
    # for the confidence interval given

    # FUNCTION:
    if (is.vector(x)) {
        x = na.omit(x)
        n = length(x)
        if (n <= 1)
            return(NA)
        se.mean = sqrt(var(x)/n)
        t.val = qt((1 - ci)/2, n - 1)
        lcl = mean(x) + se.mean * t.val
        return(lcl)
    }
    else {
        x = checkData(x, method = "matrix", ... = ...)
        result = apply(x, 2, mean.LCL, ... = ...)
        result = matrix(result, nrow=1)
        colnames(result) = colnames(x)
        rownames(result) = "Lower Confidence Level"
        return(result)
    }
}

mean.UCL <-
function (x, ci = 0.95, ...)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the standard error of the mean for a return series

    # Inputs:
    # R: Assumes returns rather than prices
    # ci: Confidence interval

    # Output:
    # Uses the standard error of the mean to calculate an upper bound
    # for the confidence interval given

    # FUNCTION:
    if (is.vector(x)) {
        x = na.omit(x)
        n = length(x)
        if (n <= 1)
            return(NA)
        se.mean = sqrt(var(x)/n)
        t.val = qt((1 - ci)/2, n - 1)
        ucl = mean(x) - se.mean * t.val
        return(ucl)
    }
    else {
        x = checkData(x, method = "matrix", ... = ...)
        result = apply(x, 2, mean.UCL, ... = ...)
        result = matrix(result, nrow=1)
        colnames(result) = colnames(x)
        rownames(result) = "Upper Confidence Level"
        return(result)
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