# collection of mean-related statistics
#
# NOTE: we would prefer that the first argument be R for returns,
#       but the package check complains about mismatch of the
#       first parameter with R command mean()

`mean.geometric` <-
function (x)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the mean geometric return for a return series

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Returns the geometric return

    # FUNCTION:
    x = checkDataVector(x)
    mean.geometric = exp(mean(log(1+x)))-1
    mean.geometric

}

`mean.stderr` <-
function (x)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the standard error of the mean for a return series

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Returns the standard error of the mean for the return

    # FUNCTION:
    x = checkDataVector(x)
    stderr = sqrt(var(x)/length(x))
    stderr

}

`mean.LCL` <-
function (x, ci = 0.95)
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
    x = checkDataVector(x)
    n = length(x)
    if (n <= 1)
        return(NA)
    se.mean = sqrt(var(x)/n)
    t.val = qt((1 - ci)/2, n - 1)
    lcl = mean(x) + se.mean * t.val
    lcl
}

`mean.UCL` <-
function (x, ci = 0.95)
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
    x = checkDataVector(x)
    n = length(x)
    if (n <= 1)
        return(NA)
    se.mean = sqrt(var(x)/n)
    t.val = qt((1 - ci)/2, n - 1)
    ucl = mean(x) - se.mean * t.val
    ucl
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: mean.utils.R,v 1.5 2007-03-05 04:51:36 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
#
# revision 1.4
# date: 2007-03-04 22:48:18 -0600;  brian;
# - change first parameter to x for R generic method consistency
# - fix capitalization errors in documentation
#
# revision 1.3
# date: 2007-03-04 22:04:24 -0600;  peter;
# - fixed bug omitting length()
#
# revision 1.2
# date: 2007-03-04 21:25:04 -0600;  peter;
# - added defaults for ci
#
# revision 1.1
# date: 2007-03-04 20:54:35 -0600;  peter;
# - added set of minor utility functions
#
###############################################################################