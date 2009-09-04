###############################################################################
# $Id: ES.R,v 1.5 2009-09-04 20:45:49 brian Exp $
###############################################################################

ES <-
function (R , p=0.99, method=c("modified","gaussian","historical", "kernel"), clean=c("none","boudt"),  portfolio_method=c("single","component"), weights=NULL, mu=NULL, sigma=NULL, skew=NULL, exkurt=NULL, m1=NULL, m2=NULL, m3=NULL, m4=NULL, invert=TRUE, operational=TRUE, ...)
{ # @author Brian G. Peterson

    # Descripion:

    # wrapper for univariate and multivariate ES functions.

    # Setup:
    #if(exists(modified)({if( modified == TRUE) { method="modified" }}
    #if(method == TRUE or is.null(method) ) { method="modified" }
    method = method[1]
    portfolio_method = portfolio_method[1]
    R <- checkData(R, method="xts", ...)

    # check weights options
    if (is.null(weights) & portfolio_method != "single"){
        warning("no weights passed in, assuming equal weighted portfolio")
        weights=t(rep(1/dim(R)[[2]], dim(R)[[2]]))
    }
    if (!is.null(weights)) {
        if (portfolio_method == "single") {
            warning("weights passed as parameter, but portfolio_method set to 'single', assuming 'component'")
            portfolio_method="component"
        }
        if (is.vector(weights)){
            warning("weights are a vector, will use same weights for entire time series") # remove this warning if you call function recursively
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

    if(clean[1]!="none"){
        R = as.matrix(Return.clean(R, method=clean))
    }
    
    switch(portfolio_method,
        single = {
            columns=colnames(R)
            switch(method,
                modified = { if (operational) rES =  operES.CornishFisher(R=R,p=p)
			     else rES = ES.CornishFisher(R=R,p=p) 
			   }, # mu=mu, sigma=sigma, skew=skew, exkurt=exkurt))},)
                gaussian = { rES = ES.Gaussian(R=R,p=p) },
                historical = { rES = ES.historical(R=R,p=p) }
            ) # end sigle switch calc
            
	    # convert from vector to columns
            rES=as.matrix(rES)
            colnames(rES)=columns
	    
	    # check for unreasonable results
            columns<-ncol(rES)
            for(column in 1:columns) {
                tmp=rES[,column]
                if (eval(0 > tmp)) { #eval added previously to get around Sweave bitching
                    warning(c("ES calculation produces unreliable result (inverse risk) for column: ",column," : ",rES[,column]))
                    # set ES to NA, since inverse risk is unreasonable
                    rES[,column] <- NA
                } else
                if (eval(1 < tmp)) { #eval added previously to get around Sweave bitching
                    warning(c("ES calculation produces unreliable result (risk over 100%) for column: ",column," : ",rES[,column]))
                    # set ES to 1, since greater than 100% is unreasonable
                    rES[,column] <- 1
                }
            } # end reasonableness checks
	    if(invert) rES <- -rES
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
            if (is.null(mu)) { mu =  apply(R,2,'mean' ) }
            if (is.null(sigma)) { sigma = cov(R) }
            if (is.null(m1)) {m1 = multivariate_mean(weights, mu)}
            if (is.null(m2)) {m2 = StdDev.MM(weights, sigma)}
            if (is.null(m3)) {m3 = M3.MM(R)}
            if (is.null(m4)) {m4 = M4.MM(R)}
            if (is.null(skew)) { skew = skewness.MM(weights,sigma,m3) }
            if (is.null(exkurt)) { exkurt = kurtosis.MM(weights,sigma,m4) - 3 }

            switch(method,
                modified = { if (operational) return(operES.CornishFisher.portfolio(p,weights,mu,sigma,m3,m4))
			     else return(ES.CornishFisher.portfolio(p,weights,mu,sigma,m3,m4))
			   },
                gaussian = { return(ES.Gaussian.portfolio(p,weights,mu,sigma)) },
                historical = { return(ES.historical.portfolio(R, p,weights)) }
            )

        }, # end component portfolio switch
    )

} # end ES wrapper function

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: ES.R,v 1.5 2009-09-04 20:45:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2009-08-25 15:29:46  brian
# - clean up labeling, warnings, and returns for Expected Shortfall
#
# Revision 1.3  2009-08-24 22:08:52  brian
# - adjust to handle p values for correct results
# - adjust ES to correctly handle probability
# - add invert argument with default TRUE to match older behavior
# - make sure all VaR/ES functions handle columns correctly
#
# Revision 1.2  2009-06-26 20:47:16  brian
# - clean up naming confusion/standardization between VaR/ES wrappers
#
# Revision 1.1  2009-06-25 16:10:03  brian
# - initial revision of ES wrapper function to call underlying Es functions for univariate and multivariate series
#
###############################################################################
