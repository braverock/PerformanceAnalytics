#' calculate single factor model (CAPM) beta
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
#' negative market returns, separately.  The \code{CAPM.beta.bull} is a
#' regression for only positive market returns, which can be used to understand
#' the behavior of the asset or portfolio in positive or 'bull' markets.
#' Alternatively, \code{CAPM.beta.bear} provides the calculation on negative
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
#' @author Peter Carl
#' @seealso \code{\link{BetaCoVariance}} \code{\link{CAPM.alpha}}
#' \code{\link{CAPM.utils}}
#' @references Sharpe, W.F. Capital Asset Prices: A theory of market
#' equilibrium under conditions of risk. \emph{Journal of finance}, vol 19,
#' 1964, 425-442. \cr Ruppert, David. \emph{Statistics and Finance, an
#' Introduction}. Springer. 2004. \cr Bacon, Carl. \emph{Practical portfolio
#' performance measurement and attribution}. Wiley. 2004. \cr
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
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
#'     CAPM.beta(managers[, "HAM2", drop=FALSE], 
#' 			managers[, "SP500 TR", drop=FALSE], 
#' 			Rf = managers[, "US 3m TR", drop=FALSE])
#'     CAPM.beta.bull(managers[, "HAM2", drop=FALSE], 
#' 			managers[, "SP500 TR", drop=FALSE], 
#' 			Rf = managers[, "US 3m TR", drop=FALSE])
#'     CAPM.beta.bear(managers[, "HAM2", drop=FALSE], 
#' 			managers[, "SP500 TR", drop=FALSE], 
#' 			Rf = managers[, "US 3m TR", drop=FALSE])
#'     TimingRatio(managers[, "HAM2", drop=FALSE], 
#' 			managers[, "SP500 TR", drop=FALSE], 
#' 			Rf = managers[, "US 3m TR", drop=FALSE])
#'     chart.Regression(managers[, "HAM2", drop=FALSE], 
#' 			managers[, "SP500 TR", drop=FALSE], 
#' 			Rf = managers[, "US 3m TR", drop=FALSE], 
#' 			fit="conditional", 
#' 			main="Conditional Beta")
#'   		
#' @rdname CAPM.beta
#' @export CAPM.beta SFM.beta
CAPM.beta <- SFM.beta <- function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a CAPM beta.

    # Inputs:
    # Ra: vector of returns for the asset being tested
    # Rb: vector of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as x and y.

    # Output:
    # 

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

    result = apply(pairs, 1, FUN = function(n, xRa, xRb)
        .beta(xRa[,n[1]], xRb[,n[2]]), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

#' @rdname CAPM.beta
#' @export 
CAPM.beta.bull <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a conditional CAPM beta for up markets.

    # Inputs:
    # Ra: time series of returns for the asset being tested
    # Rb: time series of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a time series
    #     of the same length as x and y.

    # Output:
    # Bear market beta

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

    result = apply(pairs, 1, FUN = function(n, xRa, xRb)
        .beta(xRa[,n[1]], xRb[,n[2]], xRb[,n[2]] > 0), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Bull Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

#' @rdname CAPM.beta
#' @export 
CAPM.beta.bear <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a conditional CAPM beta for down markets 

    # Inputs:
    # Ra: time series of returns for the asset being tested
    # Rb: time series of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a time series
    #     of the same length as Ra and Rb.

    # Output:
    # Bear market beta

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

    result = apply(pairs, 1, FUN = function(n, xRa, xRb)
        .beta(xRa[,n[1]], xRb[,n[2]], xRb[,n[2]] < 0), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Bear Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}


#' @rdname CAPM.beta
#' @export 
TimingRatio <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function calculates the ratio of the two conditional CAPM betas (up and down).

    beta.bull = CAPM.beta.bull(Ra, Rb, Rf = Rf)
    beta.bear = CAPM.beta.bear(Ra, Rb, Rf = Rf)
    result = beta.bull/beta.bear

    if(length(result) ==1)
        return(result)
    else {
        names = colnames(Rb)
        rownames(result) = paste("Timing Ratio:", names)
        return(result)
    }
}

.beta <- function (xRa, xRb, subset) {
    # subset is assumed to be a logical vector
    if(missing(subset))
        subset <- TRUE
    # check columns
    if(NCOL(xRa)!=1L || NCOL(xRb)!=1L || NCOL(subset)!=1L)
        stop("all arguments must have only one column")
    # merge, drop NA
    merged <- as.data.frame(na.omit(cbind(xRa, xRb, subset)))
    # return NA if no non-NA values
    if(NROW(merged)==0)
        return(NA)
    # add column names and convert subset back to logical
    colnames(merged) <- c("xRa","xRb","subset")
    merged$subset <- as.logical(merged$subset)
    # calculate beta
    model.lm = lm(xRa ~ xRb, data=merged, subset=merged$subset)
    beta = coef(model.lm)[[2]]
    beta
}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
