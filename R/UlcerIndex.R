#' calculate the Ulcer Index
#' 
#' Developed by Peter G. Martin in 1987 (Martin and McCann, 1987) and named
#' for the worry caused to the portfolio manager or investor.  This is
#' similar to drawdown deviation except that the impact of the duration of 
#' drawdowns is incorporated by selecting the negative return for each 
#' period below the previous peak or high water mark.  The impact of long,
#' deep drawdowns will have significant impact because the underperformance
#' since the last peak is squared.
#' 
#' UI = sqrt(sum[i=1,2,...,n](D'_i^2/n)) where
#' D'_i = drawdown since previous peak in period i
#' 
#' DETAILS:
#' This approach is sensitive to the frequency of the time periods involved 
#' and penalizes managers that take time to recover to previous highs.
#' 
#' REFERENCES:
#' Martin, P. and McCann, B. (1989) The investor's Guide to Fidelity Funds: 
#' Winning Strategies for Mutual Fund Investors.  John Wiley & Sons, Inc.
#' Peter Martin's web page on UI: http://www.tangotools.com/ui/ui.htm
#' 
#' ## Test against spreadsheet at: 
#' http://www.tangotools.com/ui/UlcerIndex.xls
#' 
#' @param R a vector, matrix, data frame, timeSeries or zoo object of asset
#' returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel 
#' @export
UlcerIndex <-
function (R, ...) {
    
    R = checkData(R)

    ui <- function(R) {
        result = sqrt(sum(DrawdownPeak(R)^2))
        R = na.omit(R)
	result = result/sqrt(length(R))
        return(result)
    }

    result = apply(R, MARGIN = 2, ui)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = "Ulcer Index"
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