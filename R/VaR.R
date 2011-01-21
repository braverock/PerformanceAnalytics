###############################################################################
# $Id$
###############################################################################

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
# Copyright (c) 2004-2011 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################