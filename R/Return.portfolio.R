Return.rebalancing <- function (R, weights, ...){

    if (is.vector(weights)){
        stop("Use Return.portfolio for single weighting vector.  This function is for building portfolios over rebalancing periods.")
    } 
    weights=checkData(weights,method="xts")
    R=checkData(R,method="xts")

    # loop:
    for (row in 1:nrow(weights)){
        from =as.Date(index(weights[row,]))
        if (row == nrow(weights)){
           to = as.Date(index(last(R)))
        } else {
           to = as.Date(index(weights[(row+1),]))-1
        }
        if(row==1){
            startingwealth=1
        }
        resultreturns=Return.portfolio(R[paste(from,to,sep="/"),],weights=weights[row,], startingwealth=startingwealth, ...=...)
        startingwealth=resultreturns[nrow(resultreturns),"portfolio.wealthindex"]
        # the [,-1] takes out the weighted returns, which you don't care
        # about for contribution, although you may care about it for
        # graphing, and want to pull it into another var
        if(row==1){
            result = resultreturns
        } else {
            result = rbind(result,resultreturns)
        }
    }
    result<-reclass(result, R)
    result
}

# ------------------------------------------------------------------------------
# Return.portfolio - replaces RMetrics pfolioReturn fn
# move this function and the pfolioReturn wrapper into Performanceanalytics and remove from this file

Return.portfolio <- function (R, weights=NULL, wealth.index = FALSE, contribution=FALSE, method = c("compound","simple"), startingwealth=1)
{   # @author Brian G. Peterson

    # Function to calculate weighted portfolio returns
    #
    # old function pfpolioReturn in RMetrics used continuous compunding, which isn't accurate.
    # new function lets weights float after initial period, and produces correct results.
    #
    # R                 data structure of component returns
    #
    # weights           usually a numeric vector which has the length of the number
    #                   of  assets. The weights measures the normalized weights of
    #                   the  individual assets. By default 'NULL', then an equally
    #                   weighted set of assets is assumed.
    #
    # method:           "simple", "compound"
    #
    # wealth.index      if wealth.index is TRUE, return a wealth index, if false, return a return vector for each period
    #
    # contribution      if contribution is TRUE, add the weighted return contributed by the asset in this period

    # Setup:
    R=checkData(R,method="xts")

    # take only the first method
    method = method[1]

    if (is.null(weights)){
        # set up an equal weighted portfolio
        weights = t(rep(1/ncol(R), ncol(R)))
        colnames(weights)<-colnames(R)
    } else{
        weights=checkData(weights,method="matrix") # do this to make sure we have columns, and not just a vector
        if (length(weights) != ncol(R)) stop ("The Weighting Vector and Return Collection do not have the same number of Columns.")
    }
    

    #Function:


        # construct the wealth index of unweighted assets
        wealthindex.assets=cumprod(startingwealth+R)

        wealthindex.weighted = matrix(nrow=nrow(R),ncol=ncol(R))
        colnames(wealthindex.weighted)=colnames(wealthindex.assets)
        rownames(wealthindex.weighted)=as.character(index(wealthindex.assets))
        # weight the results
        for (col in colnames(weights)){
            wealthindex.weighted[,col]=weights[,col]*wealthindex.assets[,col]
        }
        wealthindex=apply(wealthindex.weighted,1,sum)

        # weighted cumulative returns
        weightedcumcont=t(apply (wealthindex.assets,1, function(x,weights){ as.vector((x-startingwealth)* weights)},weights=weights))
        weightedreturns=diff(rbind(0,weightedcumcont))
        colnames(weightedreturns)=colnames(wealthindex.assets)
        #browser()
        wealthindex=matrix(cumprod(startingwealth + as.matrix(apply(weightedreturns,1, sum), ncol = 1)),ncol=1)
        wealthindex=reclass(wealthindex,match.to=R)

    if(method=="simple"){
        weightedreturns=Return.calculate(wealthindex,method="simple")
    }

        # uncomment this to test
        #browser()

    if (!wealth.index){
        result=as.matrix(apply(weightedreturns,1,sum),ncol=1)
        colnames(result)="portfolio.returns"
    } else {
        result=wealthindex
        colnames(result)="portfolio.wealthindex"
    }

    if (contribution==TRUE){
        # show the contribution to the returns in each period.
        result=cbind(weightedreturns,result)
    }
    rownames(result)<-index(R)
    result<-reclass(result, R)
    result
} # end function Return.portfolio

pfolioReturn <- function (x, weights=NULL, ...)
{   # @author Brian G. Peterson
    # pfolioReturn wrapper - replaces RMetrics pfolioReturn fn

    Return.portfolio(R=x, weights=weights, ...=...)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.portfolio.R,v 1.7 2009-10-15 12:26:37 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.6  2009-10-14 21:59:24  brian
# - add xts-based weights handling
# - handle column names out of order for assets and weights
#
# Revision 1.5  2009-10-10 12:40:08  brian
# - update copyright to 2004-2009
#
# Revision 1.4  2009-10-01 20:22:02  brian
# - add reclass on wealthindex so Return.calculate (now using xts internally) doesn't blow up
#
# Revision 1.3  2009-08-25 17:43:37  brian
# - updates to support Marginal VaR
# - use reclass() in Return.portfolio to return xts object
#
# Revision 1.2  2009-01-08 11:23:01  brian
# - remove obsolete comments
# - change yeargrid to rebalancegrid
# - add comment block
#