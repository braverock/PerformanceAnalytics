#' Annualized Returns Summary: Statistics and Stylized Facts
#' 
#' Table of Annualized Return, Annualized Std Dev, and Annualized Sharpe
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param Rf risk free rate, in same period as your returns
#' @param digits number of digits to round results to
#' @author Peter Carl
#' @seealso \code{\link{Return.annualized}} \cr \code{\link{StdDev.annualized}}
#' \cr \code{\link{SharpeRatio.annualized}}
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' table.AnnualizedReturns(managers[,1:8])
#' 
#' require("Hmisc")
#' result = t(table.AnnualizedReturns(managers[,1:8], Rf=.04/12))
#' 
#' textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1)), rmar = 0.8, cmar = 2,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=20, wrap.colnames=10, col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,3,0)+0.1)
#' title(main="Annualized Performance")
#' 
#' @export
table.AnnualizedReturns <-
function (R, scale = NA, Rf = 0, geometric = TRUE, digits = 4)
{# @author Peter Carl

    # DESCRIPTION:
    # Annualized Returns Summary: Statistics and Stylized Facts

    # Inputs:
    # R: a regular timeseries of returns (rather than prices)

    # Output:
    # A table of estimates of annualized returns and risk measures

    # FUNCTION:

    y = checkData(R)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    # Set up dimensions and labels
    columns = ncol(y)
    #rows = nrow(y)
    columnnames = colnames(y)
    #rownames = rownames(y)

    if(is.na(scale)) {
        freq = periodicity(R)
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
        z = c(Return.annualized(y[,column,drop=FALSE], scale = scale, geometric = geometric), StdDev.annualized(y[,column,drop=FALSE], scale = scale), SharpeRatio.annualized(y[,column,drop=FALSE], scale = scale, Rf = Rf, geometric=geometric))
        znames = c("Annualized Return", "Annualized Std Dev", paste("Annualized Sharpe (Rf=",base::round(mean(Rf)*scale,4)*100,"%)", sep="") )
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
