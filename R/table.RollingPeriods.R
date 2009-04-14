`table.RollingPeriods` <-
function (R,  periods = subset(c(3,6,9,12,18,24,36,48), c(3,6,9,12,18,24,36,48)
< length(as.matrix(R[,1]))), FUNCS=c("mean","sd"), funcs.names = c("Average", "Std Dev"), digits = 4, ...)
{# @author Peter Carl

    # DESCRIPTION:
    #  Rolling Periods Summary: Statistics and Stylized Facts

    # Inputs:
    # Assumes an input of monthly returns

    # Output:
    # A table of estimates of rolling period return measures

    # FUNCTION:

    R = checkData(R, method="zoo") # must be zoo for now, until window works w xts

    # Set up dimensions and labels
    columns = ncol(R)
    columnnames = colnames(R)
    freq = periodicity(R)

    switch(freq$scale,
        minute = {freq.lab = "minute"},
        hourly = {freq.lab = "hour"},
        daily = {freq.lab = "day"},
        weekly = {freq.lab = "week"},
        monthly = {freq.lab = "month"},
        quarterly = {freq.lab = "quarter"},
        yearly = {freq.lab = "year"}
    )

    if(length(FUNCS) != length(funcs.names)) {
        warn("The length of the names vector is unequal to the length of the functions vector, so using FUNCS for naming.")
        funcs.names = NA
    }
    if(is.na(funcs.names[1]))
        funcs.names = FUNCS

    # for each column in the matrix, do the following:
    for(column in 1:columns) {
        valueNames = vector('character', 0)
        values = vector('numeric', 0)
        column.data = na.omit(R[,column,drop=FALSE])

        # Find the last data point in the column
        end.index = length(column.data)

        for(FUNC in FUNCS) {
            func.name = funcs.names[grep(FUNC, FUNCS)]
            for(period in periods) {
                window.data = zoo(NA)

                start.index = (length(column.data) - period + 1)

                window.data = window(column.data, start = time(column.data)[start.index], end = time(column.data)[end.index])

                values = c(values, apply(as.matrix(window.data), FUN = FUNC, ..., MARGIN = 2))

                valueNames = c(valueNames,paste("Last", period, freq.lab, func.name, sep=" "))
            }
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
    ans = base::round(resultingtable, digits)
    ans

    # An example:
    #     > table.RollingPeriods(monthlyReturns.ts,rf=.04/12,FUN=c("mean","sd","SharpeRatio"))
    #                                 Actual    S&P500TR
    #     Last 3 month mean         0.005833333 0.021533333
    #     Last 6 month mean         0.005816667 0.007816667
    #     Last 9 month mean         0.012533333 0.013033333
    #     Last 12 month mean        0.009450000 0.008466667
    #     Last 18 month mean        0.007516667 0.010100000
    #     Last 24 month mean        0.006045833 0.006870833
    #     Last 36 month mean        0.009169444 0.013061111
    #     Last 3 month sd           0.013250786 0.019237030
    #     Last 6 month sd           0.014797489 0.020937470
    #     Last 9 month sd           0.017013230 0.020696376
    #     Last 12 month sd          0.016328920 0.021764455
    #     Last 18 month sd          0.017866704 0.021683526
    #     Last 24 month sd          0.016994346 0.021892553
    #     Last 36 month sd          0.026235013 0.025764242
    #     Last 3 month SharpeRatio  1.574876283 4.371462171
    #     Last 6 month SharpeRatio  1.406103795 1.350340592
    #     Last 9 month SharpeRatio  2.735414338 2.344855957
    #     Last 12 month SharpeRatio 2.112323000 1.412139876
    #     Last 18 month SharpeRatio 1.519161292 1.706268724
    #     Last 24 month SharpeRatio 1.274189604 1.129225354
    #     Last 36 month SharpeRatio 1.273709661 1.887926464

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.RollingPeriods.R,v 1.7 2009-04-14 03:45:02 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.6  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.5  2007/03/22 21:53:00  peter
# - added checkData
# - using zoo for window() calcs in period lengths
#
# Revision 1.4  2007/02/26 22:04:36  brian
# - changes in functions to pass "R CMD check" for package
#
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
