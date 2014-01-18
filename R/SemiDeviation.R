#' @rdname DownsideDeviation
#' @export
SemiDeviation <-
function (R)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function is just a wrapper of DownsideDeviation with
    # MAR = mean(x)
    # see below

    # FUNCTION:

    if (is.vector(R)) {
        R = na.omit(R)
        return(DownsideDeviation(R, MAR=mean(R), method="full"))
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, 2, SemiDeviation)
        result = matrix(result, nrow=1)
        colnames(result) = colnames(R)
        rownames(result) = "Semi-Deviation"
        return(result)
    }
}

#' @rdname DownsideDeviation
#' @export
SemiVariance <-
function (R)
{
    if (is.vector(R)) {
        R = na.omit(R)
        return(DownsideDeviation(R, MAR=mean(R), method="subset"))
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, 2, SemiVariance)
        dim(result) = c(1,NCOL(R))
        colnames(result) = colnames(R)
        rownames(result) = "Semi-Variance"
        return(result)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################