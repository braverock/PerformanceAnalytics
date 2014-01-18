###############################################################################
# $Id$
###############################################################################





#' calculates Standard Deviation for univariate and multivariate series, also
#' calculates component contribution to standard deviation of a portfolio
#' 
#' calculates Standard Deviation for univariate and multivariate series, also
#' calculates component contribution to standard deviation of a portfolio
#' 
#' TODO add more details
#' 
#' This wrapper function provides fast matrix calculations for univariate,
#' multivariate, and component contributions to Standard Deviation.
#' 
#' It is likely that the only one that requires much description is the
#' component decomposition.  This provides a weighted decomposition of the
#' contribution each portfolio element makes to the univariate standard
#' deviation of the whole portfolio.
#' 
#' Formally, this is the partial derivative of each univariate standard
#' deviation with respect to the weights.
#' 
#' As with \code{\link{VaR}}, this contribution is presented in two forms, both
#' a scalar form that adds up to the univariate standard deviation of the
#' portfolio, and a percentage contribution, which adds up to 100%.  Note that
#' as with any contribution calculation, contribution can be negative.  This
#' indicates that the asset in question is a diversified to the overall
#' standard deviation of the portfolio, and increasing its weight in relation
#' to the rest of the portfolio would decrease the overall portfolio standard
#' deviation.
#' 
#' @param R a vector, matrix, data frame, timeSeries or zoo object of asset
#' returns
#' @param \dots any other passthru parameters
#' @param clean method for data cleaning through \code{\link{Return.clean}}.
#' Current options are "none", "boudt", or "geltner".
#' @param portfolio_method one of "single","component" defining whether to do
#' univariate/multivariate or component calc, see Details.
#' @param weights portfolio weighting vector, default NULL, see Details
#' @param mu If univariate, mu is the mean of the series. Otherwise mu is the
#' vector of means of the return series , default NULL, , see Details
#' @param sigma If univariate, sigma is the variance of the series. Otherwise
#' sigma is the covariance matrix of the return series , default NULL, see
#' Details
#' @param use an optional character string giving a method for computing
#' covariances in the presence of missing values.  This must be (an
#' abbreviation of) one of the strings \code{"everything"}, \code{"all.obs"},
#' \code{"complete.obs"}, \code{"na.or.complete"}, or
#' \code{"pairwise.complete.obs"}.
#' @param method a character string indicating which correlation coefficient
#' (or covariance) is to be computed.  One of \code{"pearson"} (default),
#' \code{"kendall"}, or \code{"spearman"}, can be abbreviated.
#' @author Brian G. Peterson and Kris Boudt
#' @seealso \code{\link{Return.clean}} \code{sd}
#' @keywords ts multivariate distribution models
#' @examples
#' 
#'     data(edhec)
#' 
#'     # first do normal StdDev calc
#'     StdDev(edhec)
#'     # or the equivalent
#'     StdDev(edhec, portfolio_method="single")
#' 
#'     # now with outliers squished
#'     StdDev(edhec, clean="boudt")
#' 
#'     # add Component StdDev for the equal weighted portfolio
#'     StdDev(edhec, clean="boudt", portfolio_method="component")
#' 
#' 
#' @export
StdDev <- function (R , ..., clean=c("none","boudt","geltner"),  portfolio_method=c("single","component"), weights=NULL, mu=NULL, sigma=NULL, use="everything", method=c("pearson", "kendall", "spearman"))
{ # @author Brian G. Peterson
    
    # Descripion:
    
    # wrapper for univariate and multivariate standard deviation functions.
    
    # Setup:
    portfolio_method = portfolio_method[1]
    clean = clean[1]
    R <- checkData(R, method="xts", ...)
    columns=colnames(R)

    if (is.null(weights) & portfolio_method != "single"){
        message("no weights passed in, assuming equal weighted portfolio")
        weights=t(rep(1/dim(R)[[2]], dim(R)[[2]]))
    }
    
    # check weights options
    if (!is.null(weights)) {
        if (is.vector(weights)){
            # message("weights are a vector, will use same weights for entire time series") # remove this warning if you call function recursively
            if (length (weights)!=ncol(R)) {
                stop("number of items in weighting vector not equal to number of columns in R")
            }
        } else {
            weights = checkData(weights, method="matrix", ...)
            if (ncol(weights) != ncol(R)) {
                stop("number of columns in weighting timeseries not equal to number of columns in R")
            }
            #@todo: check for date overlap with R and weights
        }
    } # end weight checks
    
    if(clean!="none"){
        R = as.matrix(Return.clean(R, method=clean))
    }
    
    switch(portfolio_method,
            single = {
                if (is.null(weights)) {
                    tsd=t(sd.xts(R, na.rm=TRUE))
                    rownames(tsd)<-"StdDev"
                } else {
                    #do the multivariate calc with weights
                    if(!hasArg(sigma)|is.null(sigma)) sigma=cov(R, use=use, method=method[1])
                    tsd<-StdDev.MM(w=weights,sigma=sigma)
                }
                return(tsd)
            }, # end single portfolio switch
            component = {
                # @TODO: need to add another loop here for subsetting, I think, when weights is a timeseries
                #if (mu=NULL or sigma=NULL) {
                #     pfolioret = Return.portfolio(R, weights, wealth.index = FALSE, contribution=FALSE, method = c("simple"))
                #}
                # for now, use as.vector
                weights=as.vector(weights)
                names(weights)<-colnames(R)
                if (is.null(sigma)) { sigma = cov(R, use=use, method=method[1]) }
                
                return(Portsd(w=weights,sigma))
            } # end component portfolio switch           
    )
    
} # end StdDev wrapper function

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
