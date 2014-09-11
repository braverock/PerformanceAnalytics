###############################################################################
# $Id$
###############################################################################

#' calculates Expected Shortfall(ES) (or Conditional Value-at-Risk(CVaR) for
#' univariate and component, using a variety of analytical methods.
#' 
#' Calculates Expected Shortfall(ES) (also known as) Conditional Value at
#' Risk(CVaR) or Expected Tail Loss (ETL) for univariate, component, 
#' and marginal cases using a variety of analytical methods.
#' 
#' 
#' @export
#' @aliases ES CVaR ETL
#' @rdname ES
#' @param R a vector, matrix, data frame, timeSeries or zoo object of asset
#' returns
#' @param p confidence level for calculation, default p=.95
#' @param method one of "modified","gaussian","historical", see
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
#' @param operational TRUE/FALSE, default TRUE, see Details.
#' @param \dots any other passthru parameters
#' @note The option to \code{invert} the ES measure should appease both
#' academics and practitioners.  The mathematical definition of ES as the
#' negative value of extreme losses will (usually) produce a positive number.
#' Practitioners will argue that ES denotes a loss, and should be internally
#' consistent with the quantile (a negative number).  For tables and charts,
#' different preferences may apply for clarity and compactness.  As such, we
#' provide the option, and set the default to TRUE to keep the return
#' consistent with prior versions of PerformanceAnalytics, but make no value
#' judgement on which approach is preferable.
#' @section Background: This function provides several estimation methods for
#' the Expected Shortfall (ES) (also called Expected Tail Loss (ETL)
#' or Conditional Value at Risk (CVaR)) of a return series and the Component ES
#' (ETL/CVaR) of a portfolio.
#' 
#' At a preset probability level denoted \eqn{c}, which typically is between 1
#' and 5 per cent, the ES of a return series is the negative value of the
#' expected value of the return when the return is less than its
#' \eqn{c}-quantile.  Unlike value-at-risk, conditional value-at-risk has all
#' the properties a risk measure should have to be coherent and is a convex
#' function of the portfolio weights (Pflug, 2000).  With a sufficiently large
#' data set, you may choose to estimate ES with the sample average of all
#' returns that are below the \eqn{c} empirical quantile. More efficient
#' estimates of VaR are obtained if a (correct) assumption is made on the
#' return distribution, such as the normal distribution. If your return series
#' is skewed and/or has excess kurtosis, Cornish-Fisher estimates of ES can be
#' more appropriate. For the ES of a portfolio, it is also of interest to
#' decompose total portfolio ES into the risk contributions of each of the
#' portfolio components. For the above mentioned ES estimators, such a
#' decomposition is possible in a financially meaningful way.
#' @author Brian G. Peterson and Kris Boudt
#' @seealso \code{\link{VaR}} \cr \code{\link{SharpeRatio.modified}} \cr
#' \code{\link{chart.VaRSensitivity}} \cr \code{\link{Return.clean}}
#' 
#' @references Boudt, Kris, Peterson, Brian, and Christophe Croux. 2008.
#' Estimation and decomposition of downside risk for portfolios with non-normal
#' returns. 2008. The Journal of Risk, vol. 11, 79-103.
#' 
#' Cont, Rama, Deguest, Romain and Giacomo Scandolo. Robustness and sensitivity
#' analysis of risk measurement procedures. Financial Engineering Report No.
#' 2007-06, Columbia University Center for Financial Engineering.
#' 
#' Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk
#' Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002,
#' v 5.
#' 
#' Martellini, Lionel, and Volker Ziemann.  Improved Forecasts of Higher-Order
#' Comoments and Implications for Portfolio Selection. 2007. EDHEC Risk and
#' Asset Management Research Centre working paper.
#' 
#' Pflug, G. Ch.  Some remarks on the value-at-risk and the conditional
#' value-at-risk. In S. Uryasev, ed., Probabilistic Constrained Optimization:
#' Methodology and Applications, Dordrecht: Kluwer, 2000, 272-281.
#' 
#' Scaillet, Olivier. Nonparametric estimation and sensitivity analysis of
#' expected shortfall. Mathematical Finance, 2002, vol. 14, 74-86.
###keywords ts multivariate distribution models
#' @examples
#' 
#'     data(edhec)
#' 
#'     # first do normal ES calc
#'     ES(edhec, p=.95, method="historical")
#' 
#'     # now use Gaussian
#'     ES(edhec, p=.95, method="gaussian")
#' 
#'     # now use modified Cornish Fisher calc to take non-normal distribution into account
#'     ES(edhec, p=.95, method="modified")
#' 
#'     # now use p=.99
#'     ES(edhec, p=.99)
#'     # or the equivalent alpha=.01
#'     ES(edhec, p=.01)
#' 
#'     # now with outliers squished
#'     ES(edhec, clean="boudt")
#' 
#'     # add Component ES for the equal weighted portfolio
#'     ES(edhec, clean="boudt", portfolio_method="component")
#' @export ETL CVaR ES
ETL <- CVaR <- ES <- function (R=NULL , p=0.95, ..., 
        method=c("modified","gaussian","historical"), 
        clean=c("none","boudt", "geltner"),  
        portfolio_method=c("single","component"), 
        weights=NULL, mu=NULL, sigma=NULL, m3=NULL, m4=NULL, 
        invert=TRUE, operational=TRUE)
{ # @author Brian G. Peterson

    # Descripion:

    # wrapper for univariate and multivariate ES functions.

    # Setup:
    #if(exists(modified)({if( modified == TRUE) { method="modified" }}
    #if(method == TRUE or is.null(method) ) { method="modified" }
    method = method[1]
    clean = clean[1]
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
                if (is.null(m3)) {m3 = M3.MM(R,mu=mu)}
                if (is.null(m4)) {m4 = M4.MM(R,mu=mu)}
            }
        } 
    } else { 
        #R is null, check for moments
        if(is.null(mu)) stop("Nothing to do! You must pass either R or the moments mu, sigma, etc.")
        if ( length(weights) != length(mu)) {
            stop("number of items in weights not equal to number of items in the mean vector")
        }
    }
    
    switch(portfolio_method,
        single = {
            if(is.null(weights)){
                columns=colnames(R)
                switch(method,
                    modified = { if (operational) rES =  operES.CornishFisher(R=R,p=p)
    			     else rES = ES.CornishFisher(R=R,p=p) 
    			   }, # mu=mu, sigma=sigma, skew=skew, exkurt=exkurt))},)
                    gaussian = { rES = ES.Gaussian(R=R,p=p) },
                    historical = { rES = ES.historical(R=R,p=p) }
                ) # end single method switch calc
                
    	        # convert from vector to columns
                rES=as.matrix(rES)
                colnames(rES)=columns
            } else { # we have weights, so we should use the .MM calc
                weights=as.vector(weights)
                switch(method,
                        modified = { rES=mES.MM(w=weights, mu=mu, sigma=sigma, M3=m3 , M4=m4 , p=p) }, 
                        gaussian = { rES=GES.MM(w=weights, mu=mu, sigma=sigma, p=p) },
                        historical = { rES = ES.historical(R=R,p=p) %*% weights }, # note that this is not tested for weighting the univariate calc by the weights,
                ) # end multivariate method
            }
	        # check for unreasonable results
            columns<-ncol(rES)
            for(column in 1:columns) {
                tmp=rES[,column]
                if (eval(0 > tmp)) { #eval added previously to get around Sweave bitching
                    message(c("ES calculation produces unreliable result (inverse risk) for column: ",column," : ",rES[,column]))
                    # set ES to NA, since inverse risk is unreasonable
                    rES[,column] <- NA
                } else
                if (eval(1 < tmp)) { #eval added previously to get around Sweave bitching
                    message(c("ES calculation produces unreliable result (risk over 100%) for column: ",column," : ",rES[,column]))
                    # set ES to 1, since greater than 100% is unreasonable
                    rES[,column] <- 1
                }
            } # end reasonableness checks
            if(invert) rES <- -rES
            rownames(rES) <- "ES"
            return(rES)

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
                modified = { if (operational) return(operES.CornishFisher.portfolio(p,weights,mu,sigma,m3,m4))
			     else return(ES.CornishFisher.portfolio(p,weights,mu,sigma,m3,m4))
			   },
                gaussian = { return(ES.Gaussian.portfolio(p,weights,mu,sigma)) },
                historical = { return(ES.historical.portfolio(R, p,weights)) },
                kernel = { return(ES.kernel.portfolio(R=R,p=p,w=weights)) }
            )

        } # end component portfolio switch
    )

} # end ES wrapper function

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