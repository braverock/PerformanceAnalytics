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
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: na.skip.R,v 1.2 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2009-08-31 20:51:27  brian
# - add new function na.skip to deal with non-contiguous NA's in data, may eventually go to xts
# - fix components of charts.PerformanceSummary to use na.skip
#
###############################################################################
