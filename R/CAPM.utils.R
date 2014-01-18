# This file exists to contain several related and small CAPM utility functions.
# CAPM.alpha and CAPM.beta could probably have gone in here too, but they're already in separate files

#' @rdname CAPM.RiskPremium
#' @export
CAPM.CML.slope <- function (Rb, Rf = 0 )
{ #author Brian G. Peterson

    #the Capital Market Line slope is a wrapper for the Sharpe Ratio on the benchmark asset
    #
    # Rb = Return vector of the benchmark or market portfolio
    result = SharpeRatio(Rb,Rf,FUN="StdDev")
    names = colnames(Rb)
    rownames(result) = paste("Capital Market Line Slope:", names)
    return(result) 
}

#' @rdname CAPM.RiskPremium
#' @export
CAPM.CML <- function (Ra, Rb, Rf = 0)
{ #@author Brian G. Peterson

    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    cml <-function (Ra, Rb, Rf)
    {
        CML = mean(Rf, na.rm=TRUE) + CAPM.CML.slope(Rb, Rf)*mean(Ra, na.rm=TRUE)
        return(CML)
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb, Rf) cml(Ra[,n[1]], Rb[,n[2]], Rf), Ra = Ra, Rb = Rb, Rf = Rf)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Capital Market Line:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}



#' utility functions for CAPM CML, SML, and RiskPremium
#' 
#' The Capital Asset Pricing Model, from which the popular
#' \code{\link{SharpeRatio}} is derived, is a theory of market equilibrium.
#' These utility functions provide values for various measures proposed in the
#' CAPM.
#' 
#' The CAPM provides a justification for passive or index investing by positing
#' that assets that are not on the efficient frontier will either rise or lower
#' in price until they are on the efficient frontier of the market portfolio.
#' 
#' The CAPM Risk Premium on an investment is the measure of how much the
#' asset's performance differs from the risk free rate.  Negative Risk Premium
#' generally indicates that the investment is a bad investment, and the money
#' should be allocated to the risk free asset or to a different asset with a
#' higher risk premium.
#' 
#' The Capital Market Line relates the excess expected return on an efficient
#' market portfolio to it's Risk.  The slope of the CML is the Sharpe Ratio for
#' the market portfolio. The Security Market line is constructed by calculating
#' the line of Risk Premium over \code{\link{CAPM.beta}}.  For the benchmark
#' asset this will be 1 over the risk premium of the benchmark asset.  The CML
#' also describes the only path allowed by the CAPM to a portfolio that
#' outperforms the efficient frontier: it describes the line of reward/risk
#' that a leveraged portfolio will occupy.  So, according to CAPM, no portfolio
#' constructed of the same assets can lie above the CML.
#' 
#' Probably the most complete criticism of CAPM in actual practice (as opposed
#' to structural or theory critiques) is that it posits a market equilibrium,
#' but is most often used only in a partial equilibrium setting, for example by
#' using the S\&P 500 as the benchmark asset.  A better method of using and
#' testing the CAPM would be to use a general equilibrium model that took
#' global assets from all asset classes into consideration.
#' 
#' Chapter 7 of Ruppert(2004) gives an extensive overview of CAPM, its
#' assumptions and deficiencies.
#' 
#' \code{CAPM.RiskPremium} is the premium returned to the investor over the
#' risk free asset
#' 
#' \deqn{\overline{(R_{a}-R_{f})}}{mean(Ra-Rf=0)}
#' 
#' \code{CAPM.CML} calculates the expected return of the asset against the
#' benchmark Capital Market Line
#' 
#' \code{CAPM.CML.slope} calculates the slope of the Capital Market Line for
#' looking at how a particular asset compares to the CML
#' 
#' \code{CAPM.SML.slope} calculates the slope of the Security Market Line for
#' looking at how a particular asset compares to the SML created by the
#' benchmark
#' 
#' @aliases CAPM.utils CAPM.RiskPremium CAPM.CML CAPM.CML.slope CAPM.SML.slope
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @author Brian G. Peterson
#' @seealso \code{\link{CAPM.beta}} \code{\link{CAPM.alpha}}
#' \code{\link{SharpeRatio}} \code{\link{InformationRatio}}
#' \code{\link{TrackingError}} \code{\link{ActivePremium}}
#' @references Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio
#' Management},Fall 1994, 49-58. \cr Sharpe, W.F. Capital Asset Prices: A
#' theory of market equilibrium under conditions of risk. \emph{Journal of
#' finance}, vol 19, 1964, 425-442. \cr Ruppert, David. \emph{Statistics and
#' Finance, an Introduction}. Springer. 2004. \cr
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' CAPM.CML.slope(managers[,"SP500 TR",drop=FALSE], managers[,10,drop=FALSE])
#' CAPM.CML(managers[,"HAM1",drop=FALSE], managers[,"SP500 TR",drop=FALSE], Rf=0)
#' CAPM.RiskPremium(managers[,"SP500 TR",drop=FALSE], Rf=0)
#' CAPM.RiskPremium(managers[,"HAM1",drop=FALSE], Rf=0)
#' CAPM.SML.slope(managers[,"SP500 TR",drop=FALSE], Rf=0)
#' # should create plots like in Ruppert 7.1 7.2
#' @export
CAPM.RiskPremium <- function (Ra, Rf = 0)
{ #@author Brian G. Peterson

    Ra = checkData(Ra)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    xRa = Return.excess(Ra, Rf)
    result = apply(xRa, 2, mean, na.rm=TRUE)
    dim(result) = c(1,NCOL(Ra))
    colnames(result) = colnames(Ra)
    rownames(result) = paste("Risk Premium (Rf=", round(mean(Rf)*100,1),"%)", sep="")
    return (result)
}

#' @rdname CAPM.RiskPremium
#' @export
CAPM.SML.slope <- function (Rb, Rf = 0)
{ #@author Brian G. Peterson

    result = 1/CAPM.RiskPremium(Rb, Rf)
    names = colnames(Rb)
    rownames(result) = paste("Security Market Line:", names)
    return(result)
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
