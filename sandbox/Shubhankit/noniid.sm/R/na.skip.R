na.skip <- function (x, FUN=NULL, ...) # maybe add a trim capability?
{ # @author Brian Peterson

    # DESCRIPTION:

    # Time series data often contains NA's, either due to missing days, 
    # noncontiguous series, or merging multiple series,
    # 
    # Some Calulcations, such as return calculations, require data that 
    # looks like a vector, and needs the output of na.omit
    # 
    # It is often convenient to apply these vector-like functions, but 
    # you still need to keep track of the structure of the oridginal data.

    # Inputs
    # x		the time series to apply FUN too
    # FUN	function to apply
    # ...	any additonal parameters to FUN

    # Outputs:
    # An xts time series that has the same index and NA's as the data 
    # passed in, after applying FUN

    nx <- na.omit(x)
    fx <- FUN(nx, ... = ...)
    if (is.vector(fx)) {
        result <- .xts(fx, .index(x), .indexCLASS = indexClass(x))
    }
    else {
        result <- merge(fx, .xts(, .index(x)))
    }
    return(result)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: na.skip.R 1855 2012-01-15 12:57:58Z braverock $
#
###############################################################################