#' performs attribution analysis
#' 
#' @aliases attribution.arithmetic
#' 
#' Performs attribution analysis of returns. Used to decompose uncover the sources 
#' of the return using returns and portfolio weights. 
#'
#' @aliases attribution.arithmetic
#' @param Rp vector of portfolio returns
#' @param wp vector of portfolio weights
#' @param Rb
#' @param wb
#' @author Andrii Babii
#' @seealso 
#' @references Jon A. Christopherson, David R., Wayne E. Ferson 
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 2009.
#' @examples
#' 
#' wp  <- rep(c(0.6, 0.1, 0.3), 3)
#' wb <- rep(c(0.55, 0.05, 0.4), 3)
#' Rp <- c(-0.037, -0.035, -0.001, 0.041, 0.037, 0.029, 0.035, 0.012, 0.01)
#' Rb <- c(-0.019, -0.053, -0.09, 0.021, 0.043, 0.024, 0.021, 0.014, 0.004)
#' dates <- c(rep("01/01/12", 3), rep("02/01/12", 3), rep("03/01/12", 3))
#' t <- as.Date(dates, "%m/%d/%y")
#' asset.class <- rep(c("large-cap equity", "small-cap equity", "fixed income"), 3)
#' data.frame(t, asset.class, attribution.arithmetic(Rp, wp, Rb, wb, "td")) #Top-down
#' data.frame(t, asset.class, attribution.arithmetic(Rp, wp, Rb, wb, "bu")) #Bottom-up
#'
#'
attribution.arithmetic <- 
function (Rp, wp, Rb, wb, FUN=c("td", "bu"))
{ # @author Andrii Babii

    # DESCRIPTION:
    # This is a wrapper for attribution analysis.
    
    # Inputs:
    # Rp: a matrix, data frame, or timeSeries of returns
    # wp: a matrix, data frame, or timeSeries of weights
    # Rb: 
    # wb:
  
    # Outputs:
    # This function returns the
    # FUNCTION:

    Rp = checkData(Rp)
    wp = checkData(wp)
    Rb = checkData(Rb)
    wb = checkData(wb)

    ae = (wp - wb) * Rb
    se = (wb) * (Rp - Rb)
    ie = (wp - wb) * (Rp - Rb)
    te = ae + se + ie
    result.contr.td = data.frame(ae, se + ie, te) # Top-down
    result.contr.bu = data.frame(ae + ie, se, te) # Bottom-up
    if(FUN == "td")
        return(result.contr.td)
    else
        return(result.contr.bu)
}

#' @export 
#' @rdname attribution.arithmetic

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
