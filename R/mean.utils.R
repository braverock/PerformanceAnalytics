`mean.geometric` <-
function (R)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the mean geometric return for a return series

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Returns the geometric return

    # FUNCTION:
    x = checkDataVector(R)
    mean.geometric = exp(mean(log(1+x)))-1
    mean.geometric

}

`mean.stderr` <-
function (R)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the standard error of the mean for a return series

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Returns the standard error of the mean for the return

    # FUNCTION:
    x = checkDataVector(R)
    stderr = sqrt(var(x)/length(x))
    stderr

}

`mean.LCL` <-
function (R, ci)
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
    x = checkDataVector(R)

    if (length(x) <= 1)
        return(NA)
    se.mean = sqrt(var(x)/n)
    t.val = qt((1 - ci)/2, n - 1)
    lcl = mean(x) + se.mean * t.val
    lcl
}

`mean.UCL` <-
function (R, ci)
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
    x = checkDataVector(R)

    if (length(x) <= 1)
        return(NA)
    se.mean = sqrt(var(x)/n)
    t.val = qt((1 - ci)/2, n - 1)
    ucl = mean(x) - se.mean * t.val
    ucl
}