###############################################################################
# $Id$
###############################################################################





#' calculate various Value at Risk (VaR) measures
#' 
#' Calculates Value-at-Risk(VaR) for univariate, component, and marginal cases
#' using a variety of analytical methods.
#' 
#' 
#' @aliases VaR VaR.CornishFisher
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param p confidence level for calculation, default p=.95
#' @param method one of "modified","gaussian","historical", "kernel", see
#' Details.
#' @param clean method for data cleaning through \code{\link{Return.clean}}.
#' Current options are "none", "boudt", or "geltner".
#' @param portfolio_method one of "single","component","marginal" defining
#' whether to do univariate, component, or marginal calc, see Details.
#' @param weights portfolio weighting vector, default NULL, see Details
#' @param mu If univariate, mu is the mean of the series. Otherwise mu is the
#' vector of means of the return series , default NULL, , see Details
#' @param sigma If univariate, sigma is the variance of the series. Otherwise
#' sigma is the covariance matrix of the return series , default NULL, see
#' Details
#' @param m3 If univariate, m3 is the skewness of the series. Otherwise m3 is
#' the coskewness matrix of the returns series, default NULL, see Details
#' @param m4 If univariate, m4 is the excess kurtosis of the series. Otherwise
#' m4 is the cokurtosis matrix of the return series, default NULL, see Details
#' @param invert TRUE/FALSE whether to invert the VaR measure.  see Details.
#' @param \dots any other passthru parameters
#' @note The option to \code{invert} the VaR measure should appease both
#' academics and practitioners.  The mathematical definition of VaR as the
#' negative value of a quantile will (usually) produce a positive number.
#' Practitioners will argue that VaR denotes a loss, and should be internally
#' consistent with the quantile (a negative number).  For tables and charts,
#' different preferences may apply for clarity and compactness.  As such, we
#' provide the option, and set the default to TRUE to keep the return
#' consistent with prior versions of PerformanceAnalytics, but make no value
#' judgment on which approach is preferable.
#' 
#' The prototype of the univariate Cornish Fisher VaR function was completed by
#' Prof. Diethelm Wuertz.  All corrections to the calculation and error
#' handling are the fault of Brian Peterson.
#' @section Background: This function provides several estimation methods for
#' the Value at Risk (typically written as VaR) of a return series and the
#' Component VaR of a portfolio. Take care to capitalize VaR in the commonly
#' accepted manner, to avoid confusion with var (variance) and VAR (vector
#' auto-regression).  VaR is an industry standard for measuring downside risk.
#' For a return series, VaR is defined as the high quantile (e.g. ~a 95% or 99%
#' quantile) of the negative value of the returns. This quantile needs to be
#' estimated.  With a sufficiently large data set, you may choose to utilize
#' the empirical quantile calculated using \code{\link{quantile}}.  More
#' efficient estimates of VaR are obtained if a (correct) assumption is made on
#' the return distribution, such as the normal distribution.  If your return
#' series is skewed and/or has excess kurtosis, Cornish-Fisher estimates of VaR
#' can be more appropriate.  For the VaR of a portfolio, it is also of interest
#' to decompose total portfolio VaR into the risk contributions of each of the
#' portfolio components.  For the above mentioned VaR estimators, such a
#' decomposition is possible in a financially meaningful way.
#' @author Brian G. Peterson and Kris Boudt
#' @seealso \code{\link{SharpeRatio.modified}} \cr
#' \code{\link{chart.VaRSensitivity}} \cr
#' \code{\link{Return.clean}} 
#' @references Boudt, Kris, Peterson, Brian, and Christophe Croux. 2008.
#' Estimation and decomposition of downside risk for portfolios with non-normal
#' returns. 2008. The Journal of Risk, vol. 11, 79-103.
#' 
#' Cont, Rama, Deguest, Romain and Giacomo Scandolo. Robustness and sensitivity
#' analysis of risk measurement procedures. Financial Engineering Report No.
#' 2007-06, Columbia University Center for Financial Engineering.
#' 
#' Denton M. and Jayaraman, J.D. Incremental, Marginal, and Component VaR.
#' Sunguard. 2004.
#' 
#' Epperlein, E., Smillie, A. Cracking VaR with kernels. RISK, 2006, vol.  19,
#' 70-74.
#' 
#' Gourieroux, Christian, Laurent, Jean-Paul and Olivier Scaillet.  Sensitivity
#' analysis of value at risk. Journal of Empirical Finance, 2000, Vol. 7,
#' 225-245.
#' 
#' Keel, Simon and Ardia, David. Generalized marginal risk. Aeris CAPITAL
#' discussion paper.
#' 
#' Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk
#' Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002,
#' v 5.
#' 
#' Martellini, Lionel, and Volker Ziemann.  Improved Forecasts of Higher-Order
#' Comoments and Implications for Portfolio Selection. 2007. EDHEC Risk and
#' Asset Management Research Centre working paper.
#' 
#' Return to RiskMetrics: Evolution of a Standard
#' \url{http://www.riskmetrics.com/publications/techdocs/r2rovv.html}
#' 
#' Zangari, Peter. A VaR Methodology for Portfolios that include Options. 1996.
#' RiskMetrics Monitor, First Quarter, 4-12.
#' 
#' Rockafellar, Terry and Uryasev, Stanislav. Optimization of Conditional VaR.
#' The Journal of Risk, 2000, vol. 2, 21-41.
#' 
#' Dowd, Kevin. Measuring Market Risk, John Wiley and Sons, 2010.
#' 
#' Jorian, Phillippe. Value at Risk, the new benchmark for managing financial risk.
#' 3rd Edition, McGraw Hill, 2006.
#' 
#' Hallerback, John. "Decomposing Portfolio Value-at-Risk: A General Analysis",
#' 2003. The Journal of Risk vol 5/2.
#' 
#' Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and 
#'    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization",
#'    Bank of Japan.
#'      
#' @keywords ts multivariate distribution models
#' @examples
#' 
#'     data(edhec)
#' 
#'     # first do normal VaR calc
#'     VaR(edhec, p=.95, method="historical")
#' 
#'     # now use Gaussian
#'     VaR(edhec, p=.95, method="gaussian")
#' 
#'     # now use modified Cornish Fisher calc to take non-normal distribution into account
#'     VaR(edhec, p=.95, method="modified")
#' 
#'     # now use p=.99
#'     VaR(edhec, p=.99)
#'     # or the equivalent alpha=.01
#'     VaR(edhec, p=.01)
#' 
#'     # now with outliers squished
#'     VaR(edhec, clean="boudt")
#' 
#'     # add Component VaR for the equal weighted portfolio
#'     VaR(edhec, clean="boudt", portfolio_method="component")
#' 
#' @export
VaR <-
function (R=NULL , p=0.95, ..., method=c("modified","gaussian","historical", "kernel"), clean=c("none","boudt","geltner"),  portfolio_method=c("single","component","marginal"), weights=NULL, mu=NULL, sigma=NULL, m3=NULL, m4=NULL, invert=TRUE)
{ # @author Brian G. Peterson

    # Descripion:

    # wrapper for univariate and multivariate VaR functions.

    # Setup:
    #if(exists(modified)({if( modified == TRUE) { method="modified" }}
    #if(method == TRUE or is.null(method) ) { method="modified" }
    clean = clean[1]
    method = method[1]
    portfolio_method = portfolio_method[1]
    if (is.null(weights) & portfolio_method != "single"){
        message("no weights passed in, assuming equal weighted portfolio")
        weights=t(rep(1/dim(R)[[2]], dim(R)[[2]]))
    }
    if(!is.null(R)){
        R <- checkData(R, method="xts", ...)
        columns=colnames(R)
        if (!is.null(weights) & portfolio_method != "single") {
            if ( length(weights) != ncol(R)) {
                stop("number of items in weights not equal to number of columns in R")
            }
        }
        # weights = checkData(weights, method="matrix", ...) #is this necessary?
        # TODO check for date overlap with R and weights
        if(clean!="none" & is.null(mu)){ # the assumption here is that if you've passed in any moments, we'll leave R alone
            R = as.matrix(Return.clean(R, method=clean))
        }
        if(portfolio_method != "single"){
            # get the moments ready
            if (is.null(mu)) { mu =  apply(R,2,'mean' ) }
            if (is.null(sigma)) { sigma = cov(R) }
            if(method=="modified"){
                if (is.null(m3)) {m3 = M3.MM(R)}
                if (is.null(m4)) {m4 = M4.MM(R)}
            }
        } 
    } else { 
        #R is null, check for moments
        if(is.null(mu)) stop("Nothing to do! You must pass either R or the moments mu, sigma, etc.")
        if ( length(weights) != length(mu)) {
            stop("number of items in weights not equal to number of items in the mean vector")
        }
    }
    
    if (!is.null(R)){
    }
    
    switch(portfolio_method,
        single = {
            if(is.null(weights)){
                switch(method,
                    modified = { rVaR = VaR.CornishFisher(R=R,p=p) }, # mu=mu, sigma=sigma, skew=skew, exkurt=exkurt))},
                    gaussian = { rVaR = VaR.Gaussian(R=R,p=p) },
                    historical = { rVaR = -1* t(apply(R, 2, quantile, probs=1-p, na.rm=TRUE )) },
                    kernel = { stop("no kernel method defined for non-component VaR")}
                ) # end single switch calc
                # convert from vector to columns
                rVaR=as.matrix(rVaR)
                colnames(rVaR)=columns
            } else { # we have weights, so we should use the .MM calc
                weights=as.vector(weights)
                switch(method,
                        modified = { rVaR=mVaR.MM(w=weights, mu=mu, sigma=sigma, M3=m3 , M4=m4 , p=p) }, 
                        gaussian = { rVaR=GVaR.MM(w=weights, mu=mu, sigma=sigma, p=p) },
                        historical = { rVaR = VaR.historical(R=R,p=p) %*% weights } # note that this is weighting the univariate calc by the weights
                ) # end multivariate method
            }
            columns<-ncol(rVaR)
            for(column in 1:columns) {
                tmp=rVaR[,column]
                if (!is.finite(tmp)) # skip reasonableness check if tmp is NA, NaN, +/-Inf, etc
                    next()
                if (eval(tmp < 0)) { #eval added previously to get around Sweave bitching
                    message(c("VaR calculation produces unreliable result (inverse risk) for column: ",column," : ",rVaR[,column]))
                    # set VaR to NA, since inverse risk is unreasonable
                    rVaR[,column] <- NA
                } else
                if (eval(1 < tmp)) { #eval added previously to get around Sweave bitching
                    message(c("VaR calculation produces unreliable result (risk over 100%) for column: ",column," : ",rVaR[,column]))
                    # set VaR to 1, since greater than 100% is unreasonable
                    rVaR[,column] <- 1
                }
            } # end reasonableness checks
            if(invert) rVaR <- -rVaR
            rownames(rVaR)<-"VaR"
            return(rVaR)
        }, # end single portfolio switch
        component = {
            # @todo need to add another loop here for subsetting, I think, when weights is a timeseries
            #if (mu=NULL or sigma=NULL) {
            #     pfolioret = Return.portfolio(R, weights, wealth.index = FALSE, contribution=FALSE, method = c("simple"))
            #}
            # for now, use as.vector
            weights=as.vector(weights)
    	    names(weights)<-colnames(R)

            switch(method,
                modified = { return(VaR.CornishFisher.portfolio(p,weights,mu,sigma,m3,m4))},
                gaussian = { return(VaR.Gaussian.portfolio(p,weights,mu,sigma)) },
                historical = { return(VaR.historical.portfolio(R, p,weights)) },
                kernel = { return(VaR.kernel.portfolio(R, p,weights)) }
            )

        }, # end component portfolio switch
        marginal = {
    	    return(VaR.Marginal(R,p,method,as.vector(weights)))
	    },  # end marginal portfolio switch
    )

} # end VaR wrapper function

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
