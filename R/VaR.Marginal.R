VaR.Marginal <-
function(R, p = 0.95, method=c("modified","gaussian","historical"), weightingvector=NULL)
{   # @author Brian G. Peterson

    # Description:

    # Function to implement Marginal VaR
    #
    # R                 Returns of your components
    # p                 probability to calculate VaR over
    # modified          use modified VaR or traditional VaR
    # weightingvector   to calculate portfolio returns with
    #
    # @returns data frame with total VaR of the portfolio plus Marginal VaR for each component

    R = checkData(R, method = "xts")

    if (is.null(weightingvector)) {
        weightingvector = t(rep(1/dim(R)[[2]], dim(R)[[2]]))
    }
    
    if(method==TRUE) method = "modified"

    #if (ncol(weightingvector) != ncol(R)) stop ("The Weighting Vector and Return Collection do not have the same number of Columns.")

    columns = ncol(R)
    columnnames=c("PortfolioVaR",colnames(R))

    # Function

    # first, get the numbers for the whole portfolio
    portfolioR   = Return.portfolio(R,as.vector(weightingvector))
    portfolioVaR = VaR(portfolioR,p,method,portfolio_method="single")
    pVaR = array (portfolioVaR)
    result=data.frame(pVaR=pVaR)

    for(column in 1:columns) {
        # calculate a multiplication factor, because the results don't seem to make sense
        # unless the weighting vector always equals the same sum
        weightfactor = sum(weightingvector)/sum(t(weightingvector)[,-column])  # if we do need it
        # weightfactor = 1  # if we don't need it

        subportfolioR   = Return.portfolio(R[ ,-column],as.vector(t(weightingvector)[ ,-column]*weightfactor))
        subportfolioVaR = VaR(subportfolioR,p,method,portfolio_method="single")

        marginalVaR = subportfolioVaR - portfolioVaR

        mVaR = array(marginalVaR)
        mVaR = data.frame(mVaR=mVaR)

        result=cbind(result,mVaR)
    } #end columns loop

    # check our result
    # this check would be used for Incremental/Component VaR, not Marginal VaR
    # if (portfolioVaR != sum(result[,-1])) warning (paste("The VaR of the portfolio ",portfolioVaR," does not match the sum of VaR.Marginal's ",sum(result[,-1])))

    colnames(result)<-columnnames

    # Return Value:
    result
} # end function VaR.Marginal

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################