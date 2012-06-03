#' performs geometric attribution
#' 
#' @aliases attribution.geometric
#' 
#' Performs geometric attribution analysis of returns. Used to uncover the sources 
#' of portfolio return 
#'
#' @aliases attribution.geometric
#' @param Rp portfolio returns
#' @param wp portfolio weights
#' @param Rb benchmark returns
#' @param wb benchmark weights
#' @author Andrii Babii
#' @seealso 
#' @references Jon A. Christopherson, David R., Wayne E. Ferson 
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 2009.
#' @examples
#' 
#' 
#'
attribution.geometric <- 
function (Rp, wp, Rb, wb, method = c("top.down", "bottom.up", "simple"))
{ # @author Andrii Babii

    # DESCRIPTION:
    # This is a wrapper for attribution analysis.
    # TODO: extend to multiple periods, time-varying weights, multiple levels 

    # Inputs:
    # Rp: portfolio returns
    # wp: portfolio weights
    # Rb: benchmark returns
    # wb: benchmark weights
  
    # Outputs:
    # This function returns the
    # FUNCTION:
    
    Rb = checkData(Rb)
    Rp = checkData(Rp)
    wp = as.xts(matrix(rep(wp, ncol(Rp)), nrow(Rp), ncol(Rp)), index(Rp))
    wb = as.xts(matrix(rep(wb, ncol(Rb)), nrow(Rb), ncol(Rb)), index(Rb))
    colnames(wp) = colnames(Rp)
    colnames(wb) = colnames(Rb)

    allocation = (wp - wb) * (Rb - drop(Rb %*% t(wb)))
    selection = wb * (Rp - Rb)
    interaction = (wp - wb) * (Rp - Rb)
    total = allocation + selection + interaction

    k = (log(1 + Rp) - log(1 + Rb)) / (Rp - Rb)
    allocation = exp(allocation * k) - 1
    selection = exp(selection * k) - 1
    interaction = exp(interaction * k) - 1
    total = allocation + selection + interaction

    if(method == "top.down")
        result = data.frame(t(allocation), t(selection) + t(interaction), 
        t(total)) # Top-down attribution
    else
        if(method == "bottom.up")
            result = data.frame(t(allocation) + t(interaction), t(selection), 
            t(total)) # Bottom-up attribution
        else
            if(method == "simple")
                result = data.frame(t(allocation), t(selection), t(total))
            else
                stop(paste("Please select the correct method for the attribution output"))
    colnames(result) = c("Allocation", "Selection", "Total")
    sum = (t(as.matrix(colSums(result))))
    rownames(sum) = "Total"
    result = rbind(result, sum)
    return(result)
}
#EXAMPLE:
Rp <- matrix(c(0.0397, 0.0493, 0.0891, 0.0289), 1, 4)
colnames(Rp) <- c("Oil", "It", "Retail", "Energy")
rownames(Rp) <- "2011-01-06"
Rb <- Rp + 0.01
wp <- c(0.1, 0.4, 0.3, 0.2)
wb <- c(0.2, 0.1, 0.4, 0.3)
attribution.geometric(Rp, wp, Rb, wb, method = "top.down")
attribution.geometric(Rp, wp, Rb, wb, method = "bottom.up")
attribution.geometric(Rp, wp, Rb, wb, method = "simple")
attribution.geometric(Rp, wp, Rb, wb, method = "simpel")
#' @export 
#' @rdname attribution.geometric

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