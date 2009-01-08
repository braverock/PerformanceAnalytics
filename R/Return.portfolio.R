Return.portfolio.multiweight <- function (R, weights, yeargrid, ...){
    result=data.frame()

    weights=checkData(weights,method="matrix")

    # loop:
    for (row in 1:nrow(yeargrid)){
        from =yeargrid[row,1]
        to = yeargrid[row,2]
        if(row==1){ startingwealth=1 }
        resultreturns=Return.portfolio(R[from:to,],weights=t(weights[row,]), startingwealth=startingwealth, ...=...)
        startingwealth=resultreturns[nrow(resultreturns),"portfolio.wealthindex"]
        # the [,-1] takes out the weighted returns, which you don't care
        # about for contribution, although you may care about it for
        # graphing, and want to pull it into another var

        result=rbind(result,resultreturns)
    }
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
    R=checkData(R,method="zoo")

    # take only the first method
    method = method[1]

    if (is.null(weights)){
        # set up an equal weighted portfolio
        weights = t(rep(1/ncol(R), ncol(R)))
    }

    if (ncol(weights) != ncol(R)) stop ("The Weighting Vector and Return Collection do not have the same number of Columns.")

    #Function:


#    if(method=="compound") {
        # construct the wealth index of unweighted assets
        wealthindex.assets=cumprod(startingwealth+R)

        # I don't currently think that the weighted wealth index is the correct route
        # I'll uncomment this and leave it in here so that we can compare
        # build a structure for our weighted results
        wealthindex.weighted = matrix(nrow=nrow(R),ncol=ncol(R))
        colnames(wealthindex.weighted)=colnames(wealthindex.assets)
        rownames(wealthindex.weighted)=rownames(wealthindex.assets)

        # weight the results
        for (col in 1:ncol(weights)){
            wealthindex.weighted[,col]=weights[,col]*wealthindex.assets[,col]
        }
        wealthindex=apply(wealthindex.weighted,1,sum)

        # weighted cumulative returns
        weightedcumcont=t(apply (wealthindex.assets,1, function(x,weights){ as.vector((x-startingwealth)* weights)},weights=weights))
        weightedreturns=diff(rbind(0,weightedcumcont))
        colnames(weightedreturns)=colnames(wealthindex.assets)
        #browser()
        wealthindex=matrix(cumprod(startingwealth + as.matrix(apply(weightedreturns,1, sum), ncol = 1)),ncol=1)
        # or, the equivalent
        #wealthindex=matrix(startingwealth+apply(weightedcumcont,1,sum),ncol=1)
#   }

    if(method=="simple"){
        # stop("Calculating wealth index for simple returns not yet supported.")
        #weighted simple returns
        # probably need to add 1 to the column before doing this
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

    result
} # end function Return.portfolio

pfolioReturn <- function (x, weights=NULL, ...)
{   # @author Brian G. Peterson
    # pfolioReturn wrapper - replaces RMetrics pfolioReturn fn

    Return.portfolio(R=x, weights=weights, ...=...)
}