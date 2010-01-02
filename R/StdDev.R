###############################################################################
# $Id$
###############################################################################

StdDev <- function (R , clean=c("none","boudt","geltner"),  portfolio_method=c("single","component"), weights=NULL, mu=NULL, sigma=NULL, ...)
{ # @author Brian G. Peterson
    
    # Descripion:
    
    # wrapper for univariate and multivariate standard deviation functions.
    
    # Setup:
    #if(exists(modified)({if( modified == TRUE) { method="modified" }}
    #if(method == TRUE or is.null(method) ) { method="modified" }
    method = method[1]
    portfolio_method = portfolio_method[1]
    R <- checkData(R, method="xts", ...)
    columns=colnames(R)
    
    # check weights options
    if (is.null(weights) & portfolio_method != "single"){
        message("no weights passed in, assuming equal weighted portfolio")
        weights=t(rep(1/dim(R)[[2]], dim(R)[[2]]))
    }
    if (!is.null(weights)) {
        if (portfolio_method == "single") {
            message("weights passed as parameter, but portfolio_method set to 'single', using multivariate moment calc")
        }
        if (is.vector(weights)){
            #message("weights are a vector, will use same weights for entire time series") # remove this warning if you call function recursively
            if (length (weights)!=ncol(R)) {
                stop("number of items in weighting vector not equal to number of columns in R")
            }
        } else {
            weights = checkData(weights, method="matrix", ...)
            if (ncol(weights) != ncol(R)) {
                stop("number of columns in weighting timeseries not equal to number of columns in R")
            }
            #TODO: check for date overlap with R and weights
        }
    } # end weight checks
    
    if(clean[1]!="none"){
        R = as.matrix(Return.clean(R, method=clean))
    }
    
    switch(portfolio_method,
            single = {
                if (is.null(weights)) {
                    tsd<-matrix(nrow=1,ncol=ncol(R))
                    for(column in 1:ncol(R)) {
                        tsd[,column]=sd(R[,column])
                    } # end column support
                colnames(tsd)<-colnames(R)    
                rownames(tsd)<-"StdDev"
                } else {
                    #do the multivariate calc with weights
                    if(!hasArg(sigma)|is.null(sigma)) sigma=cov(R)
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
                if (is.null(sigma)) { sigma = cov(R) }
                
                return(Portsd(w=weights,sigma))
            } # end component portfolio switch           
    )
    
} # end StdDev wrapper function

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################