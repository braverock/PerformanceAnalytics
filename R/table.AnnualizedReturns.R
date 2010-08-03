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
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################