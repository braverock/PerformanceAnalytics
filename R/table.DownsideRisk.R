#' Downside Risk Summary: Statistics and Stylized Facts
#' 
#' Creates a table of estimates of downside risk measures for comparison across
#' multiple instruments or funds.
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param ci confidence interval, defaults to 95\%
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param Rf risk free rate, in same period as your returns
#' @param MAR Minimum Acceptable Return, in the same periodicity as your
#' returns
#' @param p confidence level for calculation, default p=.99
#' @param digits number of digits to round results to
#' @author Peter Carl
#' @seealso 
#' \code{\link{DownsideDeviation}} \cr 
#' \code{\link{maxDrawdown}} \cr
#' \code{\link{VaR}} \cr 
#' \code{\link{ES}} \cr
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' table.DownsideRisk(edhec, Rf=.04/12, MAR =.05/12, p=.95)
#' 
#' result=t(table.DownsideRisk(edhec, Rf=.04/12, MAR =.05/12, p=.95))
#' require("Hmisc")
#' textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, 
#'          cdec=rep(3,dim(result)[2])), rmar = 0.8, cmar = 1.5,  
#'          max.cex=.9, halign = "center", valign = "top", row.valign="center", 
#'          wrap.rownames=15, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
#' title(main="Downside Risk Statistics")
#'
#' @export 
table.DownsideRisk <-
function (R, ci = 0.95, scale = NA, Rf = 0, MAR = .1/12, p= 0.95, digits = 4)
{# @author Peter Carl

    # DESCRIPTION:
    # Downside Risk Summary: Statistics and Stylized Facts

    # Inputs:
    # R: a regular timeseries of returns (rather than prices)

    # Output:
    # A table of estimates of downside risk measures

    #FUNCTION:

    y = checkData(R, method = "zoo")
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf, method = "zoo")
    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)

    if(is.na(scale)) {
        freq = periodicity(y)
        switch(freq$scale,
            minute = {stop("Data periodicity too high")},
            hourly = {stop("Data periodicity too high")},
            daily = {scale = 252},
            weekly = {scale = 52},
            monthly = {scale = 12},
            quarterly = {scale = 4},
            yearly = {scale = 1}
        )
    }

    # for each column, do the following:
    for(column in 1:columns) {
        x = na.omit(y[,column,drop=FALSE])
        # for each column, make sure that R and Rf are for the same dates
        if(!is.null(dim(Rf))){ # if Rf is a column
            z = merge(x,Rf)
            zz = na.omit(z)
            x = zz[,1,drop=FALSE]
            Rf.subset = zz[,2,drop=FALSE]
        }
        else { # unless Rf is a single number
            Rf.subset = Rf
        }

        z = c(
                DownsideDeviation(x,MAR=mean(x)),
                sd.xts(subset(as.vector(x),as.vector(x)>0)),
                sd.xts(subset(as.vector(x),as.vector(x)<0)),
                DownsideDeviation(x,MAR=MAR),
                DownsideDeviation(x,MAR=Rf.subset),
                DownsideDeviation(x,MAR=0),
                maxDrawdown(x),
                VaR(x, p=p,method="historical"),
                ES(x, p=p,method="historical"),
                VaR(x, p=p),
                ES(x, p=p)
                )
        znames = c(
                "Semi Deviation",
                "Gain Deviation",
                "Loss Deviation",
                paste("Downside Deviation (MAR=",MAR*scale*100,"%)", sep=""),
                paste("Downside Deviation (Rf=",base::round(mean(Rf.subset*scale*100),2),"%)", sep=""),
                paste("Downside Deviation (0%)", sep=""),
                "Maximum Drawdown",
                paste("Historical VaR (",p*100,"%)",sep=""),
                paste("Historical ES (",p*100,"%)",sep=""),
                paste("Modified VaR (",p*100,"%)",sep=""),
                paste("Modified ES (",p*100,"%)",sep="")
                )
        if(column == 1) {
            resultingtable = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans

    #  For example:
    #     > table.DownsideRisk(monthlyReturns.ts,Rf=.04/12)
    #                                     Actual    S&P500TR
    #     Semi Deviation                0.020849116  0.02913679
    #     Gain Deviation                0.023009623  0.01975342
    #     Loss Deviation                0.007740678  0.01344530
    #     Downside Deviation (MAR=10%)  0.019826422  0.02944389
    #     Downside Deviation (Rf=4%)    0.016275404  0.02713448
    #     Downside Deviation (0%)       0.014248969  0.02642777
    #     Maximum Drawdown             -0.052021280 -0.04080700
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
