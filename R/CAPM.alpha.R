#' calculate CAPM alpha
#' 
#' This is a wrapper for calculating a CAPM alpha.
#' 
#' "Alpha" purports to be a measure of a manager's skill by measuring the
#' portion of the managers returns that are not attributable to "Beta", or the
#' portion of performance attributable to a benchmark.
#' 
#' While the classical CAPM has been almost completely discredited by the 
#' literature, it is an example of a simple single factor model, 
#' comparing an asset to any arbitrary benchmark.
#'  
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @author Peter Carl
#' @seealso \code{\link{CAPM.beta}} \code{\link{CAPM.utils}}
#' @references Sharpe, W.F. Capital Asset Prices: A theory of market
#' equilibrium under conditions of risk. \emph{Journal of finance}, vol 19,
#' 1964, 425-442. \cr Ruppert, David. \emph{Statistics and Finance, an
#' Introduction}. Springer. 2004. \cr
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' # First we load the data
#'     data(managers)
#'     CAPM.alpha(managers[,1,drop=FALSE], 
#' 			managers[,8,drop=FALSE], 
#' 			Rf=.035/12) 
#'     CAPM.alpha(managers[,1,drop=FALSE], 
#' 			managers[,8,drop=FALSE], 
#' 			Rf = managers[,10,drop=FALSE])
#'     CAPM.alpha(managers[,1:6], 
#' 			managers[,8,drop=FALSE], 
#' 			Rf=.035/12)
#'     CAPM.alpha(managers[,1:6], 
#' 			managers[,8,drop=FALSE], 
#' 			Rf = managers[,10,drop=FALSE])
#'     CAPM.alpha(managers[,1:6], 
#' 			managers[,8:7,drop=FALSE], 
#' 			Rf=.035/12) 
#'     CAPM.alpha(managers[,1:6], 
#' 			managers[,8:7,drop=FALSE], 
#' 			Rf = managers[,10,drop=FALSE])
#' 
#' @export
CAPM.alpha <- function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a CAPM alpha.

    # Inputs:
    # R: vector of returns for the asset being tested
    # Rb: vector of returns for the benchmark the asset is being gauged against
    # R and Rb are assumed to be matching periods
    # Rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as R and y.

    # Output:
    # CAPM alpha

    # FUNCTION:
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra)	
    Rb.ncols = NCOL(Rb)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    alpha <-function (xRa, xRb)
    {
        merged = as.data.frame(na.omit(cbind(xRa, xRb)))
        model.lm = lm(merged[,1] ~ merged[,2], merged)
        alpha = coef(model.lm)[[1]]
        alpha
    }

    result = apply(pairs, 1, FUN = function(n, xRa, xRb) alpha(xRa[,n[1]], xRb[,n[2]]), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Alpha:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
