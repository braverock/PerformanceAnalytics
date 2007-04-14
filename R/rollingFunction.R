`rollingFunction` <-
function (R, n, trim = TRUE, na.rm = TRUE, digits = 4, rf = 0, FUN = "mean", ...)
{# @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for providing n-period trailing calculations for the
    # data and functions provided.

    # Inspired by rollFun() written by Diethelm Wurtz.
    # We've extended the idea to all the columns provided, and added data checks.

    # FUNCTION:

    data.mat = checkDataMatrix(R)
    columns=ncol(data.mat)
    # @todo: remove NA's before setting column names
    columnnames = colnames(data.mat)
    # @todo: handle empty column names
    if(is.null(columnnames))
        stop("Column names are empty.  If you are trying to pass in a timeSeries, use seriesData() rather than the explicit @Data slot.")
    rows = nrow(data.mat)
    rownames = rownames(data.mat)

    if(rows < n)
        stop("Data set is too short. Select a shorter evaluation period or provide a longer time period.  Also, check the data for NA's.")

    # for each column, do the following:
    for(column in 1:columns) {
        valueNames = vector('character', 0)
        values = vector('numeric', 0)
        x = checkDataVector(data.mat[,column])

        if(n == 0) {
            period = 0
            n = 12
        }
        else
            period = n

        for(row in n:rows) {
            subperiod = x[1:row]
            values = c(values,rollingStat(subperiod, period = period, FUN = FUN, ...))
            valueNames = c(valueNames,rownames[row])
        }
        if (!trim) {
            values = c(rep(NA, (n - 1)), values)
            valueNames = c(rownames[1:n-1],valueNames)
        }
        if(column == 1) {
            resultingtable = data.frame(Value = values, row.names = valueNames)
        }

        else {
            nextcolumn = data.frame(Value = values, row.names = valueNames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    result = base::round(resultingtable, digits)
    result

    # Examples:
    # > rollingFunction(gg.ts[,1],n=3,FUN="Return.annualized")
    #                     Manager
    # 2002-02-28           0.0306
    # 2002-03-31           0.0521
    # 2002-04-30           0.0387
    # ...
    # > rollingFunction(gg.ts[,1],n=3,trim=FALSE,FUN="Return.annualized")
    #                     Manager
    # 2001-12-31               NA
    # 2002-01-31               NA
    # 2002-02-28           0.0306
    # 2002-03-31           0.0521
    # 2002-04-30           0.0387
    # ...
    # > rollingFunction(gg.ts[,1],n=3,trim=FALSE,FUN="SharpeRatio.annualized")
    #                     Manager
    # 2001-12-31               NA
    # 2002-01-31               NA
    # 2002-02-28           1.5302
    # 2002-03-31           4.3768
    # 2002-04-30           6.9640
    # ...
    # > rollingFunction(gg.ts[,1],n=3,trim=FALSE,FUN="SharpeRatio.annualized",rf=.03/12)
    #                     Manager
    # 2001-12-31               NA
    # 2002-01-31               NA
    # 2002-02-28           0.0298
    # 2002-03-31           1.8587
    # 2002-04-30           1.5598
    # ...
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: rollingFunction.R,v 1.4 2007-04-14 13:08:43 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/02/25 18:23:40  brian
# - change call to round() to call base::round() to fix conflict with newest fCalendar
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################