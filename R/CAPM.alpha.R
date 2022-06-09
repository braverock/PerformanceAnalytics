#' Calculate single factor model (CAPM) alpha
#' 
#' This is a wrapper for calculating a single factor model (CAPM) alpha.
#' 
#' "Alpha" purports to be a measure of a manager's skill by measuring the
#' portion of the managers returns that are not attributable to "Beta", or the
#' portion of performance attributable to a benchmark.
#' 
#' While the classical CAPM has been almost completely discredited by the 
#' literature, it is an example of a simple single factor model, 
#' comparing an asset to any arbitrary benchmark.
#'  
#' @aliases CAPM.alpha
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param \dots Other parameters like max.it or bb specific to lmrobdetMM regression.
#' @param method (Optional): string representing linear regression model, "LS" for Least Squares
#'                    and "Rob" for robust      
#' @param family (Optional): 
#'         If method == "Rob": 
#'           This is a string specifying the name of the family of loss function
#'           to be used (current valid options are "bisquare", "opt" and "mopt").
#'           Incomplete entries will be matched to the current valid options. 
#'           Defaults to "mopt".
#'         Else: the parameter is ignored
#' @author Dhairya Jain, Peter Carl
#' @seealso \code{\link{CAPM.beta}} \code{\link{CAPM.utils}}
#' @references Sharpe, W.F. Capital Asset Prices: A theory of market
#' equilibrium under conditions of risk. \emph{Journal of finance}, vol 19,
#' 1964, 425-442. \cr Ruppert, David. \emph{Statistics and Finance, an
#' Introduction}. Springer. 2004. \cr
###keywords ts multivariate distribution models
#' @examples
#' 
#' # First we load the data
#'     data(managers)
#'     SFM.alpha(managers[,1], 
#' 			managers[,8], 
#' 			Rf=.035/12) 
#'     SFM.alpha(managers[,1], 
#' 			managers[,8], 
#' 			Rf=.035/12, method="LS") 
#'     SFM.alpha(managers[,1], 
#' 			managers[,8], 
#' 			Rf = managers[,10])
#'     SFM.alpha(managers[,1], 
#' 			managers[,8], 
#' 			Rf = managers[,10],
#' 			method="Rob", family="mopt")
#'     SFM.alpha(managers[,1:6], 
#' 			managers[,8], 
#' 			Rf=.035/12)
#'     SFM.alpha(managers[,1:6], 
#' 			managers[,8], 
#' 			Rf=.035/12, method="Rob", 
#' 			family="bisquare", bb=0.25, max.it=200)
#'     SFM.alpha(managers[,1:6], 
#' 			managers[,8], 
#' 			Rf = managers[,10])
#'     SFM.alpha(managers[,1:6], 
#' 			managers[,8:9], 
#' 			Rf=.035/12) 
#'     SFM.alpha(managers[,1:6], 
#' 			managers[,8:9], 
#' 			Rf = managers[,10])
#'     SFM.alpha(managers[,1:6], 
#'               managers[,8:9], 
#'               Rf = managers[,10],method="Rob", 
#'               family="mopt", bb=0.25, max.it=200)
#'     SFM.alpha(managers[, "HAM2", drop=FALSE], 
#'               managers[, "SP500 TR", drop=FALSE], 
#'               Rf = managers[, "US 3m TR", drop=FALSE])
#'     SFM.alpha(managers[, "HAM2", drop=FALSE], 
#'               managers[, "SP500 TR", drop=FALSE], 
#'               Rf = managers[, "US 3m TR", drop=FALSE],
#'               method="Rob", family="opt",
#'               bb=0.25, max.it=200)
#' 	   
#' @rdname SFM.alpha
#' @export SFM.alpha CAPM.alpha
SFM.alpha <- CAPM.alpha <- function (Ra, Rb, Rf = 0,  ..., method="LS", family="mopt", warn=T){
    # @author Peter Carl, Dhairya Jain

    # DESCRIPTION:
    # This is a wrapper for calculating a SFM alpha.

    # Inputs:
    # R: vector of returns for the asset being tested
    # Rb: vector of returns for the benchmark the asset is being gauged against
    # R and Rb are assumed to be matching periods
    # Rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as R and y.
    # method (Optional): string representing linear regression model, "LS" for Least Squares
    #                    and "Rob" for robust. Defaults to "LS      
    # family (Optional): 
    #         If method == "Rob": 
    #           This is a string specifying the name of the family of loss function
    #           to be used (current valid options are "bisquare", "opt" and "mopt").
    #           Incomplete entries will be matched to the current valid options. 
    #           Defaults to "mopt".
    #         Else: the parameter is ignored
    
    # Output:
    # SFM alpha

    # FUNCTION:
    
    # .coefficients fails if method is "Both"
    if (warn && method == "Both"){
        warning("Using 'Both' while using SFM.beta will lead to ill-formatted output");
    }
    
    # Get the NCOL and colnames from Ra, and Rb
    Ra.ncols <- NCOL(Ra);
    Rb.ncols <- NCOL(Rb);
    Ra.colnames <- colnames(Ra);
    Rb.colnames <- colnames(Rb)
    
    # Get the excess returns of Ra, Rb over Rf
    xR <- excessReturns(Ra, Rb, Rf);
    xRa <- xR[[1]];
    xRb <- xR[[2]];
    
    # Get the result matrix
    result.all <- getResults(xRa=xRa, xRb=xRb, 
                             Ra.ncols=Ra.ncols, Rb.ncols=Rb.ncols, 
                             method = method, family = family, ...);
    
    # Process the results and return them
    return (processResults(result.all, "intercept", Ra.ncols, Rb.ncols, 
                           Ra.colnames, Rb.colnames, method, "Alpha"))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2020 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
