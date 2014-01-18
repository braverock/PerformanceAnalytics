#' calculate a function over an expanding window always starting from the
#' beginning of the series
#' 
#' A function to calculate a function over an expanding window from the start
#' of the timeseries.  This wrapper allows easy calculation of \dQuote{from
#' inception} statistics.
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param FUN any function that can be evaluated using a single set of returns
#' (e.g., rolling beta won't work, but \code{\link{Return.annualized}} will)
#' @param gap the number of data points from the beginning of the series
#' required to \dQuote{train} the calculation
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso \code{\link[zoo]{rollapply}}
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' apply.fromstart(managers[,1,drop=FALSE], FUN="mean", width=36)
#' 
#' @export
#' 
apply.fromstart <- function (R, FUN = "mean" , gap = 1, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A function to calculate a function from the start of the timeseries

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # FUN: any function that can be evaluated using a single set of returns
    #   (e.g., rolling beta won't work, but Return.annualizeds will)
    # gap: the number of data points required for the calculation to start

    # Outputs:
    # A timeseries in a zoo object of the calculation results

    # FUNCTION:

    # Coerce input data into a zoo object
    R = checkData(R, method = "zoo")

    # Get dimensions and labels
    columns = ncol(R)
    columnnames = colnames(R)

    # Calculate

    for(column in 1:columns) {
        # the drop=FALSE flag is essential for when the zoo object only has one column
        column.Return.calc=zoo(NA, order.by = as.Date(time(R)))
        for(i in gap:length(time(R))) {
            data.zoo = window(R[,column,drop=FALSE],start = start(R), end = time(R[i])) #rm as.Date
            column.Return.calc[i]=apply(as.matrix(data.zoo[,,drop=FALSE]), FUN = FUN, ..., MARGIN = 2)
        }
        if(column == 1)
            Return.calc = column.Return.calc
        else
            Return.calc = merge(Return.calc,column.Return.calc)
    }

    if(!is.null(ncol(Return.calc)))
        colnames(Return.calc) = columnnames

    return(Return.calc)

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
