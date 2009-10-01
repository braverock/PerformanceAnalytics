###############################################################################
# $Id: VaR.R,v 1.14 2009-10-01 19:10:40 brian Exp $
###############################################################################

VaR <-
function (R , p=0.99, method=c("modified","gaussian","historical", "kernel"), clean=c("none","boudt"),  portfolio_method=c("single","component","marginal"), weights=NULL, mu=NULL, sigma=NULL, m3=NULL, m4=NULL, invert=TRUE, ...)
{ # @author Brian G. Peterson

    # Descripion:

    # wrapper for univariate and multivariate VaR functions.

    # Setup:
    #if(exists(modified)({if( modified == TRUE) { method="modified" }}
    #if(method == TRUE or is.null(method) ) { method="modified" }
    method = method[1]
    portfolio_method = portfolio_method[1]
    R <- checkData(R, method="xts", ...)
    columns=colnames(R)

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
            switch(method,
                modified = { rVaR = VaR.CornishFisher(R=R,p=p) }, # mu=mu, sigma=sigma, skew=skew, exkurt=exkurt))},
                gaussian = { rVaR = VaR.Gaussian(R=R,p=p) },
                historical = { rVaR = -1* t(apply(R, 2, quantile, probs=1-p, na.rm=TRUE )) },
                kernel = { stop("no kernel method defined for non-component VaR")}
            ) # end sigle switch calc
            # convert from vector to columns
            rVaR=as.matrix(rVaR)
            colnames(rVaR)=columns
            #rVaR=t(rVaR) #transform so it has real rows and columns
            # check for unreasonable results
            columns<-ncol(rVaR)
            for(column in 1:columns) {
                tmp=rVaR[,column]
                if (eval(tmp < 0)) { #eval added previously to get around Sweave bitching
                    warning(c("VaR calculation produces unreliable result (inverse risk) for column: ",column," : ",rVaR[,column]))
                    # set VaR to NA, since inverse risk is unreasonable
                    rVaR[,column] <- NA
                } else
                if (eval(1 < tmp)) { #eval added previously to get around Sweave bitching
                    warning(c("VaR calculation produces unreliable result (risk over 100%) for column: ",column," : ",rVaR[,column]))
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
            if (is.null(mu)) { mu =  apply(R,2,'mean' ) }
            if (is.null(sigma)) { sigma = cov(R) }
            # if (is.null(m1)) {m1 = multivariate_mean(weights, mu)}
            # if (is.null(m2)) {m2 = StdDev.MM(weights, sigma)}
            if (is.null(m3)) {m3 = M3.MM(R)}
            if (is.null(m4)) {m4 = M4.MM(R)}
            # if (is.null(skew)) { skew = skewness.MM(weights,sigma,m3) }
            # if (is.null(exkurt)) { exkurt = kurtosis.MM(weights,sigma,m4) - 3 }

            switch(method,
                modified = { return(VaR.CornishFisher.portfolio(p,weights,mu,sigma,m3,m4))},
                gaussian = { return(VaR.Gaussian.portfolio(p,weights,mu,sigma)) },
                historical = { return(VaR.historical.portfolio(R, p,weights)) },
                kernel = { return(VaR.kernel.portfolio(R, p,weights)) }
            )

        }, # end component portfolio switch
        marginal = {
#             weights=as.vector(weights)
# 	    names(weights)<-colnames(R)
	    return(VaR.Marginal(R,p,method,as.vector(weights)))
	},  # end marginal portfolio switch
    )

} # end VaR wrapper function

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: VaR.R,v 1.14 2009-10-01 19:10:40 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.13  2009-09-24 16:01:58  brian
# - remove unneeded function params
# - remove deprecated function
#
# Revision 1.12  2009-08-25 17:43:37  brian
# - updates to support Marginal VaR
# - use reclass() in Return.portfolio to return xts object
#
# Revision 1.11  2009-08-25 15:21:07  brian
# - eliminate deprecated multi-argument return
#
# Revision 1.10  2009-08-25 14:48:33  brian
# - add additional warnings for unreasonable VaR results
#
# Revision 1.9  2009-08-25 14:38:06  brian
# - update display logic and names in list return for Component VaR, test more cases
#
# Revision 1.8  2009-08-24 22:08:52  brian
# - adjust to handle p values for correct results
# - adjust ES to correctly handle probability
# - add invert argument with default TRUE to match older behavior
# - make sure all VaR/ES functions handle columns correctly
#
# Revision 1.7  2009-07-02 14:01:23  peter
# - forced returned value into matrix for naming
# - made VaR.CornishFisher results negative
#
# Revision 1.6  2009-06-26 20:47:16  brian
# - clean up naming confusion/standardization between VaR/ES wrappers
#
# Revision 1.4  2009-06-22 16:35:11  brian
# - correct apply for historical VaR to pass na.rm=TRUE
#
# Revision 1.2  2009-06-19 20:59:35  brian
# - worked out more of the switch logic,
# - NOTE: still looping too many times
#
# Revision 1.1  2009-04-17 15:14:00  brian
# - Initial revision of VaR wrapper and portfolio risk functions
#
###############################################################################
