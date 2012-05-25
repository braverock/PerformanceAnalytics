#' computes contribution of the portfolio segments
#' 
#' @aliases contibution
#' 
#' Performs simple contribution analysis of returns. Used to uncover the sources 
#' of the return using returns and portfolio weights. 
#'
#' @aliases contribution
#' @param Rp vector of portfolio returns
#' @param wp vector of portfolio weights
#' @author Andrii Babii
#' @seealso 
#' @references Jon A. Christopherson, David R., Wayne E. Ferson 
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 2009.
#' @examples
#' 
#' data(portfolio)
#'     c <- contribution(portfolio[, 6], portfolio[, 7])
#'     data.frame(portfolio, c)
#'
#'
contribution <- 
function (Rp, wp)
{ # @author Andrii Babii

    # DESCRIPTION:
    # This is a wrapper for calculating contribution to returns.
    
    # Inputs:
    # Rp: a matrix, data frame, or timeSeries of returns
    # wp: a matrix, data frame, or timeSeries of weights
  
    # Outputs:
    # This function returns the vector of contribution to returns

    # FUNCTION:

    Rp = checkData(Rp, method = "zoo")
    wp = checkData(wp, method = "zoo")
    
    columns = ncol(Rp)
    rows = nrow(Rp)
    columnames = colnames(Rp)
    
    for(i in 1:columns){
    contr = Rp[, i] * wp[, i]
    if(i == 1){
        result.contr = contr
    } else{
        result.contr = merge(result.contr, contr)
    }
    }
    colnames(result.contr) = columnames
    return(result.contr)
}

#' @export 
#' @rdname contribution

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CalmarRatio.R 1905 2012-04-21 19:23:13Z braverock $
#
###############################################################################
