`table.AnnualizedReturns` <-
function (R, ci = 0.95, firstcolumn = 1, scale = 12, rf = 0, digits = 4)
{# @author Peter Carl

    # DESCRIPTION:
    # Annualized Returns Summary: Statistics and Stylized Facts

    # Inputs:
    # R: a regular timeseries of returns (rather than prices)

    # Output:
    # A table of estimates of annualized returns and risk measures

    # FUNCTION:

    y = checkDataMatrix(R)

    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)

    # for each column, do the following:
    for(column in firstcolumn:columns) {
    x = as.vector(y[,column])
    x.length = length(x)
    x = x[!is.na(x)]
    x.na = x.length - length(x)
    z = c(Return.annualized(x, scale = scale), StdDev.annualized(x, scale = scale), SharpeRatio.annualized(x, scale = scale, rf = rf))
    znames = c("Annualized Return", "Annualized Std Dev", paste("Annualized Sharpe (rf=",rf*scale*100,"%)", sep="") )
        if(column == 1) {
            resultingtable = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    ans = round(resultingtable, digits = digits)
    ans
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.AnnualizedReturns.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################