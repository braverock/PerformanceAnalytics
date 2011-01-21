Return.cumulative <-
function (R, geometric = TRUE)
{ # @author Peter Carl

    # This is a useful function for calculating cumulative return over a period
    # of time, say a calendar year.  Can produce simple or geometric return.

    if (is.vector(R)) {
        R = na.omit(R)
        if (!geometric)
            return(sum(R))
        else {
            return(prod(1+R)-1)
        }
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, 2, Return.cumulative, geometric = geometric)
        dim(result) = c(1,NCOL(R))
        colnames(result) = colnames(R)
        rownames(result) = "Cumulative Return"
        return(result)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2011 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################