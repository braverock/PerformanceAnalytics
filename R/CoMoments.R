# Compute co-moment matrices



#' calculate centered Returns
#' 
#' the \eqn{n}-th centered moment is calculated as \deqn{ }{moment^n(R) =
#' E[R-E(R)^n]}\deqn{ \mu^{(n)}(R) = E\lbrack(R-E(R))^n\rbrack }{moment^n(R) =
#' E[R-E(R)^n]}
#' 
#' These functions are used internally by PerformanceAnalytics to calculate
#' centered moments for a multivariate distribution as well as the standardized
#' moments of a portfolio distribution.  They are exposed here for users who
#' wish to use them directly, and we'll get more documentation written when we
#' can.
#' 
#' These functions were first utilized in Boudt, Peterson, and Croux (2008),
#' and have been subsequently used in our other research.
#' 
#' ~~ Additional Details will be added to documentation as soon as we have time
#' to write them. Documentation Patches Welcome. ~~
#' 
#' @aliases centeredcomoment centeredmoment Return.centered
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb an xts, vector, matrix, data frame, timeSeries or zoo object of
#' index, benchmark, portfolio, or secondary asset returns to compare against
#' @param power power or moment to calculate
#' @param p1 first power of the comoment
#' @param p2 second power of the comoment
#' @param normalize whether to standardize the calculation to agree with common
#' usage, or leave the default mathematical meaning
#' @param \dots any other passthru parameters
#' @author Kris Boudt and Brian Peterson
#' @references Boudt, Kris, Brian G. Peterson, and Christophe Croux. 2008.
#' Estimation and Decomposition of Downside Risk for Portfolios with Non-Normal
#' Returns. Journal of Risk. Winter.
#' 
#' Martellini, Lionel, and Volker Ziemann. 2007. Improved Forecasts of
#' Higher-Order Comoments and Implications for Portfolio Selection. EDHEC Risk
#' and Asset Management Research Centre working paper.
#' 
#' Ranaldo, Angelo, and Laurent Favre Sr. 2005. How to Price Hedge Funds: From
#' Two- to Four-Moment CAPM. SSRN eLibrary.
#' 
#' Scott, Robert C., and Philip A. Horvath. 1980. On the Direction of
#' Preference for Moments of Higher Order than the Variance. Journal of Finance
#' 35(4):915-919.
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' 
#' data(managers)
#' Return.centered(managers[,1:3,drop=FALSE])
#' 
#' @rdname centeredmoments 
#' @export 
Return.centered <-
function (R,...)
{ # @author Peter Carl and Kris Boudt

    # DESCRIPTION:
    # Calculates the returns less the mean return of the asset

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns

    # Outputs:
    # A timeseries of the calculated series

    # FUNCTION:

    # Transform input data to a timeseries (zoo) object
    R = checkData(R, method="zoo")

    # Get dimensions and labels
    columns.a = ncol(R)
    rows.a = nrow(R)

    if(columns.a==1){
       R.centered = zoo(NA);
       R.mean = zoo(NA);
       R.mean = mean(R[, drop=FALSE])
       R.centered = R[ , drop=FALSE] - R.mean
    }else{
       R.mean = apply(R,2,'mean', na.rm=TRUE)
       # returns a vector holding the mean return for each asset

       R.centered = R - matrix( rep(R.mean,rows.a), ncol= columns.a, byrow=TRUE)
       # return the matrix of centered returns
   }


   # RESULTS:
    return(R.centered)
}

###############################################################################

CoSkewnessMatrix <-
function (R, ...)
{ # @author Kris Boudt
    return(M3.MM(R))
}

###############################################################################

CoKurtosisMatrix <-
function (R, ...)
{ # @author Kris Boudt
    return(M4.MM(R))
}

###############################################################################


#' @rdname centeredmoments
centeredmoment = function(R,power)
{# @author Kris Boudt, Peter Carl
    R = checkData(R)
    out =  apply(Return.centered(R)^power,2,FUN=mean, na.rm=TRUE)
    return(out);
}

###############################################################################

#' @rdname centeredmoments
centeredcomoment = function(Ra,Rb,p1,p2,normalize=FALSE)
{# @author Kris Boudt, Peter Carl, and Brian G. Peterson

    Ra = checkData(Ra); Rb = checkData(Rb);

    out = mean( na.omit( Return.centered(Ra)^p1 * Return.centered(Rb)^p2))

    if(normalize) {
        out=out/ as.numeric(centeredmoment(Rb,power=(p1+p2))) #
    }
    return(out);
}


###############################################################################

#' Functions for calculating comoments of financial time series
#' 
#' calculates coskewness and cokurtosis as the skewness and kurtosis of two
#' assets with reference to one another.
#' 
#' Ranaldo and Favre (2005) define coskewness and cokurtosis as the skewness
#' and kurtosis of a given asset analysed with the skewness and kurtosis of the
#' reference asset or portfolio.  Adding an asset to a portfolio, such as a
#' hedge fund with a significant level of coskewness to the portfolio, can
#' increase or decrease the resulting portfolio's skewness. Similarly, adding a
#' hedge fund with a positive cokurtosis coefficient will add kurtosis to the
#' portfolio.
#' 
#' The co-moments are useful for measuring the marginal contribution of each
#' asset to the portfolio's resulting risk.  As such, comoments of asset return
#' distribution should be useful as inputs for portfolio optimization in
#' addition to the covariance matrix.  Martellini and Ziemann (2007) point out
#' that the problem of portfolio selection becomes one of selecting tangency
#' points in four dimensions, incorporating expected return, second, third and
#' fourth centered moments of asset returns.
#' 
#' Even outside of the optimization problem, measuring the co-moments should be
#' a useful tool for evaluating whether or not an asset is likely to provide
#' diversification potential to a portfolio, not only in terms of normal risk
#' (i.e. volatility) but also the risk of assymetry (skewness) and extreme
#' events (kurtosis).
#' @name CoMoments
#' @concept co-moments
#' @concept moments
#' @aliases CoMoments CoVariance CoSkewness CoKurtosis
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb an xts, vector, matrix, data frame, timeSeries or zoo object of
#' index, benchmark, portfolio, or secondary asset returns to compare against
#' @author Kris Boudt, Peter Carl, Brian Peterson
#' @seealso \code{\link{BetaCoSkewness}} \cr \code{\link{BetaCoKurtosis}} \cr
#' \code{\link{BetaCoMoments}} \cr % \code{\link{MultivariateMoments}}
#' @references Boudt, Kris, Brian G. Peterson, and Christophe Croux. 2008.
#' Estimation and Decomposition of Downside Risk for Portfolios with Non-Normal
#' Returns. Journal of Risk. Winter.
#' 
#' Martellini, Lionel, and Volker Ziemann. 2007. Improved Forecasts of
#' Higher-Order Comoments and Implications for Portfolio Selection. EDHEC Risk
#' and Asset Management Research Centre working paper.
#' 
#' Ranaldo, Angelo, and Laurent Favre Sr. 2005. How to Price Hedge Funds: From
#' Two- to Four-Moment CAPM. SSRN eLibrary.
#' 
#' Scott, Robert C., and Philip A. Horvath. 1980. On the Direction of
#' Preference for Moments of Higher Order than the Variance. Journal of Finance
#' 35(4):915-919.
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' CoVariance(managers[, "HAM2", drop=FALSE], managers[, "SP500 TR", drop=FALSE])
#' CoSkewness(managers[, "HAM2", drop=FALSE], managers[, "SP500 TR", drop=FALSE])
#' CoKurtosis(managers[, "HAM2", drop=FALSE], managers[, "SP500 TR", drop=FALSE])
#' 
#' 
CoVariance<- function(Ra,Rb)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    covar <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        return(centeredcomoment(R[,1],R[,2],p1=1,p2=1,normalize=FALSE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) covar(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Covariance:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

BetaCoVariance <- function(Ra,Rb)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    bcovar <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        return(centeredcomoment(R[,1],R[,2],p1=1,p2=1,normalize=TRUE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) bcovar(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Beta Covariance:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

#' @rdname CoMoments
CoSkewness <- function(Ra,Rb)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    cosk <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        return(centeredcomoment(R[,1],R[,2],p1=1,p2=2,normalize=FALSE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) cosk(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Coskewness:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

BetaCoSkewness <- function(Ra, Rb, test=FALSE)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    bcosk <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        skew = skewness(Rb)
        # Kris notes: the output should be conditional on the value of the market skewness. 
        if(skew > -0.05 && skew < 0.05 ){
            warning("skewness is close to zero. The classical definition of the coskewness statistic is not applicable and one should normalize using the comoment without standardization.")
        }
        if(test==TRUE){
#             if(skew < 0)
#                 multiplier = -1
#             else
#                 multiplier = 1
            stop("Not implemented yet")
        }
        else
            multiplier = 1

        return(multiplier * centeredcomoment(R[,1],R[,2],p1=1,p2=2,normalize=TRUE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) bcosk(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Beta Coskewness:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

#' @rdname CoMoments
CoKurtosis <- function(Ra,Rb)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    cokurt <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        return(centeredcomoment(R[,1],R[,2],p1=1,p2=3,normalize=FALSE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) cokurt(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Cokurtosis:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

BetaCoKurtosis <- function(Ra,Rb)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    bcosk <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        return(centeredcomoment(R[,1],R[,2],p1=1,p2=3,normalize=TRUE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) bcosk(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Beta Cokurtosis:", colnames(Rb))
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
