###############################################################################
# $Id: VaR.R,v 1.1 2009-04-17 15:14:00 brian Exp $
###############################################################################

VaR <-
function (R , p=0.99, method=c("modified","gaussian","historical", "kernel"), clean=c("none","boudt"),  portfolio_method=c("single","component","marginal"), weights=NULL, mu=NULL, sigma=NULL, skew=NULL, exkurt=NULL, m1=NULL, m2=NULL, m3=NULL, m4=NULL, ...)
{ # @author Brian G. Peterson

    # Descripion:

    # wrapper for univariate and multivariate VaR functions.

    # Setup:
    if(exists(modified) & modified == TRUE) { method="modified" }
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
            weights = checkData(weights, method="xts", ...)
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
                modified = { return(VaR.CornishFisher(R=R,p=p, mu=mu, sigma=sigma, skew=skew, exkurt=exkurt))},
                gaussian = { return(VaR.traditional(R=R,p=p)) },
                historical = { return(apply(R, 2, quantile, probs=1-p)) },
                kernel = {}
            )
        }, # end single portfolio switch
        component = {
            # @todo need to add another loop here for subsetting, I think, when weights is a timeseries
            #if (mu=NULL or sigma=NULL) {
            #     pfolioret = Return.portfolio(R, weights, wealth.index = FALSE, contribution=FALSE, method = c("simple"))
            #}
            if (is.null(mu)) { mu =  apply(R,2,'mean' ) }
            if (is.null(sigma)) { sigma = cov(R) }
            if (is.null(m1)) {m1 = multivariate_mean(weights, mu)}
            if (is.null(m2)) {m2 = StdDev.MM(weights, sigma)}
            if (is.null(m3)) {m3 = M3.MM(R)}
            if (is.null(m4)) {m4 = M4.MM(R)}
            if (is.null(skew)) { skew = skewness.MM(weights,sigma,M3) }
            if (is.null(exkurt)) { exkurt = kurtosis.MM(weights,sigma,M4) - 3 }

            switch(method,
                modified = { return(VaR.CornishFisher.portfolio(R=R,p=p))},
                gaussian = { return(VaR.Gaussian.portfolio(R=R,p=p)) },
                historical = { return(VaR.historical.portfolio(R=R,p=p)) }
            )

        }, # end component portfolio switch
        marginal = {
        },  # end marginal portfolio switch
    )

} # end VaR wrapper function

# `VaR.CornishFisher` <-
# function(R, p=0.99, modified = TRUE, clean=c("none", "boudt"))
# {   # @author Brian G. Peterson (completed/debugged fn)
#     # @author Diethelm Wuertz (prototype function)
#
#
#     # Description:
#
#     # The limitations of mean Value-at-Risk are well covered in the literature.
#     # Laurent Favre and Jose-Antonio Galeano published a paper in the
#     # Fall 2002, volume 5 of the Journal of Alternative Investment,
#     # "Mean-Modified Value-at-Risk optimization With Hedge Funds",
#     # that proposed a modified VaR calculation that takes the higher moments
#     # of non-normal distributions (skewness, kurtosis) into account, and
#     # collapses to standard (traditional) mean-VaR if the return stream follows a
#     # standard distribution.
#     # This measure is now widely cited and used in the literature,
#     # and is usually referred to as "Modified VaR" or "Modified Cornish-Fisher VaR"
#
#     # Diethelm Wuertz's original function was called monthlyVaR, but did not
#     # contain the required modifications to get to a monthly or an annualized number.
#     # I have converted it to VaR.CornishFisher, and made the assumption of p=0.99, with an option for p=0.95 and
#     # a collapse to normal mean VaR.
#
#     # FUNCTION:
#
#     # compute zc for the probability we want
#     if ( p >= 0.51 ) {
#         # looks like p was a percent like .99
#         p = 1-p
#     }
#     zc = qnorm(p)
#
#     R = checkData(R, method = "zoo")
#     columns = ncol(R)
#     columnnames=colnames(R)
#     if(clean[1]!="none"){
#         R = as.matrix(Return.clean(R, method=clean))
#     }
#
#     # FUNCTION:
#     for(column in 1:columns) {
#         r = as.vector(na.omit(R[,column]))
#         if (!is.numeric(r)) stop("The selected column is not numeric")
#
#         if (modified) {
#             s = skewness(r) #skewness of the distribution
#             k = kurtosis(r) #(excess) kurtosis
#             Zcf = zc + (((zc^2-1)*s)/6) + (((zc^3-3*zc)*k)/24) - ((((2*zc^3)-5*zc)*s^2)/36)
#             VaR = (-mean(r) - (Zcf * sd(r)))
#         } else {
#             VaR = (-mean(r) - (zc * sd(r)))
#         }
#         # check for unreasonable results
#         if (eval(VaR<0)){ #eval added to get around Sweave bitching
#             warning(c("VaR calculation produces unreliable result (inverse risk) for column: ",column," : ",VaR))
#             # set VaR to 0, since inverse risk is unreasonable
#             VaR=0
#         }
#         if (eval(VaR>1)){ #eval added to get around Sweave bitching
#             warning(c("VaR calculation produces unreliable result (risk over 100%) for column: ",column," : ",VaR))
#             # set VaR to 1, since greater than 100% is unreasonable
#             VaR=1
#         }
#
#         VaR=array(VaR)
#         if (column==1) {
#             #create data.frame
#             result=data.frame(VaR=VaR)
#         } else {
#             VaR=data.frame(VaR=VaR)
#             result=cbind(result,VaR)
#         }
#     } #end columns loop
#
#     if(ncol(result) == 1) {
#         # some backflips to name the single column zoo object
#         result = as.numeric(result)
#     }
#     else
#         colnames(result) = columnnames
#
#     # Return Value:
#     result
# }
#
# ###############################################################################
#
# `modifiedVaR` <-
# function(R, p=0.99)
# {   # @author Brian G. Peterson
#
#     # Description:
#
#     # This is a wrapper function for VaR.CornishFisher,
#     # because this measure is often referred to as modifiedVaR
#
#     # FUNCTION:
#     VaR.CornishFisher(R = R, p = p, modified=TRUE)
#
# }
#
# ###############################################################################
#
# `VaR.mean` <-
# function(R, p=0.95)
# {   # @author Brian G. Peterson
#
#     # Description:
#
#     # This is a wrapper function for modified VaR which assumes a normal
#     # distribution by discounting influence from skewness or kurtosis.
#
#     # Wrapper should be used with metrics related to VaR, such as Beyond VaR.
#
#     # FUNCTION:
#     VaR.CornishFisher(R = R, p = p, modified=FALSE)
#
# }
#
# ###############################################################################
#
# `VaR.traditional` <-
# function(R, p=0.95)
# {   # @author Brian G. Peterson
#
#     # Description:
#
#     # This is a wrapper function for modified VaR which assumes a normal
#     # distribution by discounting influence from skewness or kurtosis.
#
#     # Wrapper should be used with metrics related to VaR, such as Beyond VaR.
#
#     # FUNCTION:
#     VaR.CornishFisher(R = R, p = p, modified=FALSE)
#
# }

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: VaR.R,v 1.1 2009-04-17 15:14:00 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################