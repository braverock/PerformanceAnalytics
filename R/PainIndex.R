PainIndex <-
function (R, ...) {
    
    # DESCRIPTION:
    # The pain index is the mean value of the drawdowns over the entire 
    # analysis period. The measure is similar to the Ulcer index except that 
    # the drawdowns are not squared.  Also, it's different than the average
    # drawdown, in that the numerator is the total number of observations 
    # rather than the number of drawdowns.
    # 
    # Visually, the pain index is the area of the region that is enclosed by 
    # the horizontal line at zero percent and the drawdown line in the 
    # Drawdown chart.
    # 
    # PI = sum[i=1,2,...,n](abs(D'_i)/n) where
    # D'_i = drawdown since previous peak in period i
    # 
    # Becker, Thomas (2006)? Zephyr Associates.

    R = checkData(R)

    pi <- function(R) {
        R = na.omit(R)
        result = sqrt(sum(abs(Drawdowns(R)))/length(R))
        return(result)
    }

    result = apply(R, MARGIN = 2, pi)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = "Pain Index"
    return (result)
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