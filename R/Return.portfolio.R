#' @rdname Return.portfolio
#' @export
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




#' Calculates weighted returns for a portfolio of assets
#' 
#' Calculates weighted returns for a portfolio of assets.  If you have a single
#' weighting vector, or want the equal weighted portfolio, use
#' \code{Return.portfolio}.  If you have a portfolio that is periodically
#' rebalanced, and multiple time periods with different weights, use
#' \code{Return.rebalancing}.  Both functions will subset the return series to
#' only include returns for assets for which \code{weight} is provided.
#' 
#' \code{Return.rebalancing} uses the date in the weights time series or matrix
#' for xts-style subsetting of rebalancing periods.  Rebalancing periods can be
#' thought of as taking effect immediately after the close of the bar.  So, a
#' March 31 rebalancing date will actually be in effect for April 1.  A
#' December 31 rebalancing date will be in effect on Jan 1, and so forth.  This
#' convention was chosen because it fits with common usage, and because it
#' simplifies xts Date subsetting via \code{endpoints}.
#' 
#' \code{Return.rebalancing} will rebalance only on daily or lower frequencies.
#' If you are rebalancing intraday, you should be using a trading/prices
#' framework, not a weights-based return framework.
#' 
#' @aliases Return.portfolio Return.rebalancing
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param weights a time series or single-row matrix/vector containing asset
#' weights, as percentages
#' @param wealth.index TRUE/FALSE whether to return a wealth index, default
#' FALSE
#' @param contribution if contribution is TRUE, add the weighted return
#' contributed by the asset in this period
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param \dots any other passthru parameters
#' @return returns a time series of returns weighted by the \code{weights}
#' parameter, possibly including contribution for each period
#' @author Brian G. Peterson
#' @seealso \code{\link{Return.calculate}} \cr
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. Chapter 2\cr
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' 
#' data(edhec)
#' data(weights)
#' 
#' # calculate an equal weighted portfolio return
#' round(Return.portfolio(edhec),4)
#' 
#' # now return the contribution too
#' round(Return.portfolio(edhec,contribution=TRUE),4)
#' 
#' # calculate a portfolio return with rebalancing
#' round(Return.rebalancing(edhec,weights),4)
#' 
#' @export
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
    if(method=="simple" | nrow(R) == 1) {
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
      wealthindex=as.xts(apply(wealthindex.weighted,1,sum))
      result = wealthindex
      result[2:length(result)] = result[2:length(result)] /
        lag(result)[2:length(result)] - 1
      #result[1] = result[1] - 1
      result[1] = result[1] / sum(abs(weights[1,])) #divide by the sum of the first weighting vector to account for possible leverage
	  w = matrix(rep(NA), ncol(wealthindex.assets) * nrow(wealthindex.assets), ncol = ncol(wealthindex.assets), nrow = nrow(wealthindex.assets))
      w[1, ] = weights
      w[2:length(wealthindex), ] = (wealthindex.weighted / rep(wealthindex, ncol(wealthindex.weighted)))[1:(length(wealthindex) - 1), ]
      weightedreturns = R[, colnames(weights)] * w
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
        result=cbind(weightedreturns, coredata(result))
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