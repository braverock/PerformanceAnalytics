#' order list of drawdowns from worst to best
#' 
#' sortDrawdowns(findDrawdowns(R)) Gives the drawdowns in order of worst to
#' best
#' 
#' Returns a sorted list: \itemize{ 
#'  \item return depth of drawdown 
#'  \item from starting period 
#'  \item to ending period 
#'  \item length length in periods 
#' }
#' 
#' @param runs pass in runs array from findDrawdowns to be sorted
#' @author Peter Carl \cr modified with permission from prototype function by
#' Sankalp Upadhyay
#' @seealso \code{\link{DownsideDeviation}} \cr \code{\link{maxDrawdown}} \cr
#' \code{\link{findDrawdowns}} \cr \code{\link{sortDrawdowns}} \cr
#' \code{\link{chart.Drawdown}} \cr \code{\link{table.Drawdowns}} \cr
#' \code{\link{table.DownsideRisk}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 88 \cr
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' findDrawdowns(edhec[,"Funds of Funds", drop=FALSE])
#' sortDrawdowns(findDrawdowns(edhec[,"Funds of Funds", drop=FALSE]))
#' 
#' @export
sortDrawdowns <- function (runs) {
    # sortDrawdowns(findDrawdowns(returns))
    # gives the drawdowns in order of worst to best

    # original function modified with permission from function by Sankalp Upadhyay
    # <sankalp.upadhyay [at] gmail [dot] com>
    
    # this version provided by H. Felix Wittman < hfwittmann <at> googlemail <dot> com >
    
    index.sorted <- sort(runs$return, index=T)$ix # das kleinste zu erst
    runs.sorted <- lapply(runs, function(x) x <- x[index.sorted])
    
    return(runs.sorted)
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
