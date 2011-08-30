###############################################################################
# $Id$
###############################################################################

ETL <- CVaR <- ES <- function (R=NULL , p=0.95, ..., 
        method=c("modified","gaussian","historical", "kernel"), 
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
                        historical = { rES = (ES.historical(R=R,p=p) %*% weights) } # note that this is not tested for weighting the univariate calc by the weights
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
                historical = { return(ES.historical.portfolio(R, p,weights)) }
            )

        } # end component portfolio switch
    )

} # end ES wrapper function

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