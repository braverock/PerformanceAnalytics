Return.rebalancing <- function (R, weights, ...)
{   # @author Brian G. Peterson

    if (is.vector(weights)){
        stop("Use Return.portfolio for single weighting vector.  This function is for building portfolios over rebalancing periods.")
    } 
    weights=checkData(weights,method="xts")
    R=checkData(R,method="xts")

    if(as.Date(first(index(R))) > (as.Date(index(weights[1,]))+1)) {
        warning(paste('data series starts on',as.Date(first(index(R))),', which is after the first rebalancing period',as.Date(first(index(weights)))+1)) 
    }
    if(as.Date(last(index(R))) < (as.Date(index(weights[1,]))+1)){
        stop(paste('last date in series',as.Date(last(index(R))),'occurs before beginning of first rebalancing period',as.Date(first(index(weights)))+1))
    }
    # loop:
    for (row in 1:nrow(weights)){
        from =as.Date(index(weights[row,]))+1
        if (row == nrow(weights)){
           to = as.Date(index(last(R))) # this is correct
        } else {
           to = as.Date(index(weights[(row+1),]))
        }
        if(row==1){
            startingwealth=1
        }
        tmpR<-R[paste(from,to,sep="/"),]
        if (nrow(tmpR)>=1){
            resultreturns=Return.portfolio(tmpR,weights=weights[row,], ...=...)
            if(row==1){
                result = resultreturns
            } else {
                result = rbind(result,resultreturns)
            }
        }
        startingwealth=last(cumprod(1+result)*startingwealth)
    }
    result<-reclass(result, R)
    result
}

# ------------------------------------------------------------------------------
# Return.portfolio
Return.portfolio <- function (R, weights=NULL, wealth.index = FALSE, contribution=FALSE,geometric=TRUE, ...)
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
    if(!nrow(R)>=1){
        warning("no data passed for R(eturns)")
        return(NULL)
    }
    # take only the first method
    if(hasArg(method) & !is.null(list(...)$method)) 
        method = list(...)$method[1]
    else if(!isTRUE(geometric)) 
        method='simple' 
    else method=FALSE

    if (is.null(weights)){
        # set up an equal weighted portfolio
        weights = t(rep(1/ncol(R), ncol(R)))
        warning("weighting vector is null, calulating an equal weighted portfolio")
        colnames(weights)<-colnames(R)
    } else{
        weights=checkData(weights,method="matrix") # do this to make sure we have columns, and not just a vector
    }
    if (nrow(weights)>1){
        if ((nrow(weights)==ncol(R) |nrow(weights)==ncol(R[,names(weights)])  ) & (ncol(weights)==1)) {
          weights = t(weights) #this was a vector that got transformed
        } else {
          stop("Use Return.rebalancing for multiple weighting periods.  This function is for portfolios with a single set of weights.")
        }
    }
    if (is.null(colnames(weights))) { colnames(weights)<-colnames(R) }

    #Function:


    # construct the wealth index
    if(method=="simple") {
      # weights=as.vector(weights)
      weightedreturns = R[,colnames(weights)] * as.vector(weights) # simple weighted returns
      returns = R[,colnames(weights)] %*% as.vector(weights) # simple compound returns
      if(wealth.index) {
        wealthindex = as.matrix(cumsum(returns),ncol=1) # simple wealth index
      } else {
        result = returns
      }
    } else {
      #things are a little more complicated for the geometric case

      # first construct an unweighted wealth index of the assets
      wealthindex.assets=cumprod(1+R[,colnames(weights)])

      wealthindex.weighted = matrix(nrow=nrow(R),ncol=ncol(R[,colnames(weights)]))
      colnames(wealthindex.weighted)=colnames(wealthindex.assets)
      rownames(wealthindex.weighted)=as.character(index(wealthindex.assets))
      # weight the results
      for (col in colnames(weights)){
          wealthindex.weighted[,col]=weights[,col]*wealthindex.assets[,col]
      }
      wealthindex=apply(wealthindex.weighted,1,sum)

      # weighted cumulative returns
      weightedcumcont=t(apply (wealthindex.assets,1, function(x,weights){ as.vector((x-1)* weights)},weights=weights))
      weightedreturns=diff(rbind(0,weightedcumcont)) # compound returns
      colnames(weightedreturns)=colnames(wealthindex.assets)
      if (!wealth.index){
        result=as.matrix(apply(weightedreturns,1,sum),ncol=1)
      } else {
        wealthindex=matrix(cumprod(1 + as.matrix(apply(weightedreturns,1, sum), ncol = 1)),ncol=1)
      }
    }


    if (!wealth.index){
        colnames(result)="portfolio.returns"
    } else {
        wealthindex=reclass(wealthindex,match.to=R)
        result=wealthindex
        colnames(result)="portfolio.wealthindex"
    }

    if (contribution==TRUE){
        # show the contribution to the returns in each period.
        result=cbind(weightedreturns,result)
    }
    rownames(result)<-NULL # avoid a weird problem with rbind, per Jeff
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
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################