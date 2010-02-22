###############################################################################
# $Id$
###############################################################################

VaR <-
function (R , p=0.95, ..., method=c("modified","gaussian","historical", "kernel"), clean=c("none","boudt","geltner"),  portfolio_method=c("single","component","marginal"), weights=NULL, mu=NULL, sigma=NULL, m3=NULL, m4=NULL, invert=TRUE)
{ # @author Brian G. Peterson

    # Descripion:

    # wrapper for univariate and multivariate VaR functions.

    # Setup:
    #if(exists(modified)({if( modified == TRUE) { method="modified" }}
    #if(method == TRUE or is.null(method) ) { method="modified" }
    clean = clean[1]
    method = method[1]
    portfolio_method = portfolio_method[1]
    R <- checkData(R, method="xts", ...)
    columns=colnames(R)

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
    }
    
    if(clean!="none" & is.null(mu)){ # the assumption here is that if you've passed in any moments, we'll leave R alone
        R = as.matrix(Return.clean(R, method=clean))
    }
    
    if (is.null(weights) & portfolio_method != "single"){
        message("no weights passed in, assuming equal weighted portfolio")
        weights=t(rep(1/dim(R)[[2]], dim(R)[[2]]))
    } else {
        # we have weights, so get the moments ready
        if (is.null(mu)) { mu =  apply(R,2,'mean' ) }
        if (is.null(sigma)) { sigma = cov(R) }
        if(method=="modified"){
            if (is.null(m3)) {m3 = M3.MM(R)}
            if (is.null(m4)) {m4 = M4.MM(R)}
        }
    } # end weight checks
      
    switch(portfolio_method,
        single = {
            if(is.null(weights)){
                switch(method,
                    modified = { rVaR = VaR.CornishFisher(R=R,p=p) }, # mu=mu, sigma=sigma, skew=skew, exkurt=exkurt))},
                    gaussian = { rVaR = VaR.Gaussian(R=R,p=p) },
                    historical = { rVaR = -1* t(apply(R, 2, quantile, probs=1-p, na.rm=TRUE )) },
                    kernel = { stop("no kernel method defined for non-component VaR")}
                ) # end sigle switch calc
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
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.16  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.15  2009-10-01 19:12:24  brian
# - update probability to 95% by default
#
# Revision 1.14  2009-10-01 19:10:40  brian
# - label rows
# - invert historical VaR for consistency with other univariate measures
#
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
