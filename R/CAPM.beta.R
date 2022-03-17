#' Calculate single factor model (CAPM) beta
#' 
#' The single factor model or CAPM Beta is the beta of an asset to the variance 
#' and covariance of an initial portfolio.  Used to determine diversification potential.
#' 
#' This function uses a linear intercept model to achieve the same results as
#' the symbolic model used by \code{\link{BetaCoVariance}}
#' 
#' \deqn{\beta_{a,b}=\frac{CoV_{a,b}}{\sigma_{b}}=\frac{\sum((R_{a}-\bar{R_{a}})(R_{b}-\bar{R_{b}}))}{\sum(R_{b}-\bar{R_{b}})^{2}}}{beta
#' = cov(Ra,Rb)/var(R)}
#' 
#' Ruppert(2004) reports that this equation will give the estimated slope of
#' the linear regression of \eqn{R_{a}}{Ra} on \eqn{R_{b}}{Rb} and that this
#' slope can be used to determine the risk premium or excess expected return
#' (see Eq. 7.9 and 7.10, p. 230-231).
#' 
#' Two other functions apply the same notion of best fit to positive and
#' negative market returns, separately.  The \code{SFM.beta.bull} is a
#' regression for only positive market returns, which can be used to understand
#' the behavior of the asset or portfolio in positive or 'bull' markets.
#' Alternatively, \code{SFM.beta.bear} provides the calculation on negative
#' market returns.
#' 
#' The \code{TimingRatio} may help assess whether the manager is a good timer
#' of asset allocation decisions.  The ratio, which is calculated as
#' \deqn{TimingRatio =\frac{\beta^{+}}{\beta^{-}}}{Timing Ratio = beta+/beta-}
#' is best when greater than one in a rising market and less than one in a
#' falling market.
#' 
#' While the classical CAPM has been almost completely discredited by the 
#' literature, it is an example of a simple single factor model, 
#' comparing an asset to any arbitrary benchmark.
#'  
#' @aliases CAPM.beta CAPM.beta.bull CAPM.beta.bear TimingRatio
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param \dots Other parameters like max.it or bb specific to lmrobdetMM regression.
#' @param method (Optional): string representing linear regression model, "LS" for Least Squares
#'                    and "Rob" for robust. Defaults to "LS"      
#' @param family (Optional): 
#'         If method == "Rob": 
#'           This is a string specifying the name of the family of loss function
#'           to be used (current valid options are "bisquare", "opt" and "mopt").
#'           Incomplete entries will be matched to the current valid options. 
#'           Defaults to "mopt".
#'         Else: the parameter is ignored
#' @author Dhairya Jain, Peter Carl 
#' @seealso \code{\link{BetaCoVariance}} \code{\link{SFM.alpha}}
#' \code{\link{CAPM.utils}}
#' @references Sharpe, W.F. Capital Asset Prices: A theory of market
#' equilibrium under conditions of risk. \emph{Journal of finance}, vol 19,
#' 1964, 425-442. \cr Ruppert, David. \emph{Statistics and Finance, an
#' Introduction}. Springer. 2004. \cr Bacon, Carl. \emph{Practical portfolio
#' performance measurement and attribution}. Wiley. 2004. \cr
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#'     SFM.beta(managers[,1], 
#' 			managers[,8], 
#' 			Rf=.035/12)
#'     SFM.beta(managers[,1], 
#' 			managers[,8], 
#' 			Rf=.035/12, method="LS") 
#'     SFM.beta(managers[,1], 
#' 			managers[,8], 
#' 			Rf = managers[,10],
#' 			method="Rob", family="mopt")
#'     SFM.beta(managers[,1:6], 
#' 			managers[,8], 
#' 			Rf=.035/12, method="Rob", 
#' 			family="opt", bb=0.25, max.it=200)
#'     SFM.beta(managers[,1:6], 
#' 			managers[,8], 
#' 			Rf = managers[,10])
#'     SFM.beta(managers[,1:6], 
#' 			managers[,8:7], 
#' 			Rf=.035/12, method="Rob", family="mopt") 
#'     SFM.beta(managers[,1:6], 
#' 			managers[,8:7], 
#' 			Rf = managers[,10])
#'     SFM.beta(managers[, "HAM2"], 
#' 			managers[, "SP500 TR"], 
#' 			Rf = managers[, "US 3m TR"])
#' 	   SFM.beta.bull(managers[, "HAM2"], 
#' 			managers[, "SP500 TR"], 
#' 			Rf = managers[, "US 3m TR"])
#'     SFM.beta.bull(managers[, "HAM2"], 
#' 			managers[, "SP500 TR"], 
#' 			Rf = managers[, "US 3m TR"],
#' 			method="Rob", family="opt",
#' 			bb=0.25, max.it=200)
#' 	   SFM.beta.bear(managers[, "HAM2"], 
#' 			managers[, "SP500 TR"], 
#' 			Rf = managers[, "US 3m TR"])
#'     SFM.beta.bear(managers[, "HAM2"], 
#' 			managers[, "SP500 TR"], 
#' 			Rf = managers[, "US 3m TR"],
#' 			method="Rob", family="bisquare",
#' 			bb=0.25, max.it=200)
#'     TimingRatio(managers[, "HAM2"], 
#' 			managers[, "SP500 TR"], 
#' 			Rf = managers[, "US 3m TR"])
#' 	   TimingRatio(managers[, "HAM2"], 
#' 			managers[, "SP500 TR"], 
#' 			Rf = managers[, "US 3m TR"],
#' 			method="Rob", family="opt",
#' 			bb=0.25, max.it=200)	
#'     chart.Regression(managers[, "HAM2"], 
#' 			managers[, "SP500 TR"], 
#' 			Rf = managers[, "US 3m TR"], 
#' 			fit="conditional", 
#' 			main="Conditional Beta")
#'   		
#' @rdname SFM.beta
#' @export SFM.beta CAPM.beta
SFM.beta <- CAPM.beta <- function (Ra, Rb, Rf = 0, ..., method="LS", family="mopt")
{ # @author Peter Carl, Dhairya Jain

    # DESCRIPTION:
    # This is a wrapper for calculating a CAPM beta.

    # Inputs:
    # Ra: vector of returns for the asset being tested
    # Rb: vector of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as x and y.
    # method (Optional): string representing linear regression model, "LS" for Least Squares
    #                    and "Rob" for robust. Defaults to "LS"      
    # family (Optional): 
    #         If method == "Rob": 
    #           This is a string specifying the name of the family of loss function
    #           to be used (current valid options are "bisquare", "opt" and "mopt").
    #           Incomplete entries will be matched to the current valid options. 
    #           Defaults to "mopt".
    #         Else: the parameter is ignored
    
    # Output:
    # 

    # FUNCTION:
    
    # .coefficients fails if method is "Both"
    if (method == "Both"){
        stop("Method can't be 'Both' while using SFM.beta")
    }
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    result.all = apply(pairs, 1, FUN = function(n, xRa, xRb, ..., method, family)
        CAPM.coefficients(xRa[,n[1]], xRb[,n[2]], ..., method = method, family = family), 
        xRa = xRa, xRb = xRb, ..., method = method, family = family)
    result = list()
    for (res in result.all) {
        result[[length(result)+1]] <- res$beta
    }
    if(length(result) ==1)
        return(result[[1]])
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

#' @rdname SFM.beta
#' @export SFM.beta.bull CAPM.beta.bull
SFM.beta.bull <- CAPM.beta.bull <-
function (Ra, Rb, Rf = 0, ..., method="LS", family="mopt")
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a conditional CAPM beta for up markets.

    # Inputs:
    # Ra: time series of returns for the asset being tested
    # Rb: time series of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a time series
    #     of the same length as x and y.
    # method (Optional): string representing linear regression model, "LS" for Least Squares
    #                    and "Rob" for robust. Defaults to "LS"      
    # family (Optional): 
    #         If method == "Rob": 
    #           This is a string specifying the name of the family of loss function
    #           to be used (current valid options are "bisquare", "opt" and "mopt").
    #           Incomplete entries will be matched to the current valid options. 
    #           Defaults to "mopt".
    #         Else: the parameter is ignored
    #
    # Output:
    # Bull market beta

    # FUNCTION:
    
    # .coefficients fails if method is "Both"
    if (method == "Both"){
        stop("Method can't be 'Both' while using SFM.beta")
    }
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
    
    # .coefficients fails if subset contains no positive values, all(xRb <= 0) is true
    if (all(xRb <= 0)) {
        message("Function SFM.beta.bull: Cannot perform lm. No positive Rb values (no bull periods).")
        return(NA)
    }
    
    result.all = apply(pairs, 1, FUN = function(n, xRa, xRb, ...)
        CAPM.coefficients(xRa[,n[1]], xRb[,n[2]], xRb[,n[2]] > 0, ...), 
        xRa = xRa, xRb = xRb, ...)
    
    result.all = apply(pairs, 1, FUN = function(n, xRa, xRb, ..., method, family)
        CAPM.coefficients(xRa[,n[1]], xRb[,n[2]], subset = xRb[,n[2]] > 0, ..., 
                          method = method, family = family), 
        xRa = xRa, xRb = xRb, ..., method = method, family = family)
    result = list()
    for (res in result.all) {
        result[[length(result)+1]] <- res$beta
    }
    
    if(length(result) ==1)
        return(result[[1]])
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Bull Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

#' @rdname SFM.beta
#' @export SFM.beta.bear CAPM.beta.bear
SFM.beta.bear <- CAPM.beta.bear <-
function (Ra, Rb, Rf = 0, ..., method="LS", family="mopt")
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a conditional CAPM beta for down markets 

    # Inputs:
    # Ra: time series of returns for the asset being tested
    # Rb: time series of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a time series
    #     of the same length as Ra and Rb.
    # method (Optional): string representing linear regression model, "LS" for Least Squares
    #                    and "Rob" for robust. Defaults to "LS"   
    # family (Optional): 
    #         If method == "Rob": 
    #           This is a string specifying the name of the family of loss function
    #           to be used (current valid options are "bisquare", "opt" and "mopt").
    #           Incomplete entries will be matched to the current valid options. 
    #           Defaults to "mopt".
    #         Else: the parameter is ignored
    
    # Output:
    # Bear market beta

    # FUNCTION:
    
    # .coefficients fails if method is "Both"
    if (method == "Both"){
        stop("Method can't be 'Both' while using SFM.beta")
    }
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
    
    # .coefficients fails if subset contains no negative values, all(xRb >= 0) is true
    if (all(xRb >= 0)) {
        message("Function SFM.beta.bear: Cannot perform lm. No negative Rb values (no bear periods).")
        return(NA)
    }
    
    result.all = apply(pairs, 1, FUN = function(n, xRa, xRb, ..., method, family)
        CAPM.coefficients(xRa[,n[1]], xRb[,n[2]], subset = xRb[,n[2]] < 0, ..., 
                          method = method, family = family), 
        xRa = xRa, xRb = xRb, ..., method = method, family = family)
    
    result = list()
    for (res in result.all) {
        result[[length(result)+1]] <- res$beta
    }
    if(length(result) ==1)
        return(result[[1]])
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Bear Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}


#' @rdname SFM.beta
#' @export
TimingRatio <-
function (Ra, Rb, Rf = 0, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function calculates the ratio of the two conditional CAPM betas (up and down).

    beta.bull = SFM.beta.bull(Ra, Rb, Rf = Rf, ...)
    beta.bear = SFM.beta.bear(Ra, Rb, Rf = Rf, ...)
    
    result = beta.bull/beta.bear

    if(length(result) ==1)
        return(result)
    else {
        names = colnames(Rb)
        rownames(result) = paste("Timing Ratio:", names)
        return(result)
    }
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
