#' calculate a function over a rolling window
#' 
#' Creates a results timeseries of a function applied over a rolling window.
#' 
#' Wrapper function for \code{\link[zoo]{rollapply}} to hide some of the
#' complexity of managing single-column zoo objects.
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param width number of periods to apply rolling function window over
#' @param gap numeric number of periods from start of series to use to train
#' risk calculation
#' @param trim TRUE/FALSE, whether to keep alignment caused by NA's
#' @param FUN any function that can be evaluated using a single set of returns
#' (e.g., rolling beta won't work, but \code{\link{Return.annualized}} will)
#' @param by calculate FUN for trailing width points at every by-th time point.
#' @param \dots any other passthru parameters
#' @return A timeseries in a zoo object of the calculation results
#' @author Peter Carl
#' @seealso \code{\link{apply}} \cr \code{\link[zoo]{rollapply}}
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' apply.rolling(managers[,1,drop=FALSE], FUN="mean", width=36)
#' 
#' @export
apply.rolling <- function (R, width, trim = TRUE, gap = 12, by = 1, FUN = "mean", ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # FUNCTION:
    R = checkData(R)
    R = na.omit(R)
    rows=NROW(R)
    result = xts(, order.by = time(R))
    dates=time(R)

    calcs = matrix()

    if(width == 0) { # from inception
        gap = gap
    }
    else
        gap = width
    steps = seq(from = rows, to = gap, by = -by)
    steps = steps[order(steps)]
    for(row in steps) {
        if (width == 0)  # from inception
            r = R[1:row,]
        else
            r = R[(row-width+1):row,]
        calc = apply(r, MARGIN = 2, FUN = FUN, ...=...)
        calcs = rbind(calcs, calc)
    }
    calcs = xts(calcs[-1],order.by=dates[steps])
    result = merge(result, calcs)
# print(result)
#     colnames(result)=colnames(R)
    result = reclass(result, R)
    return (result)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
