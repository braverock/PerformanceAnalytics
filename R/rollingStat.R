`rollingStat` <-
function (R, period = 3, trim = TRUE, FUN, ...)
{ # @author Peter Carl

    # DESCRIPTION
    # Function to apply any time series function over a rolling period,
    # say the last three months.  Pass in the function name as FUN,
    # e.g., FUN = "mean".  Useful for passing in several periods and functions
    # for comparing different series.

    # Inputs:
    # period  the number of periods over which to calculate the statistic.
    #         Setting the period to 0 calculates the statistic "from inception" or
    #         using all of the data passed in.
    # FUN     function to apply over e.g., FUN = "mean" or FUN = "Return.cumulatives"

    # Output:

    # FUNCTION

    x = checkDataVector(R)

    if (length(x) < period){
    # @todo: instead of stopping, should this warn and return NAs for length(x)?
        warning("Data set is not long enough. Reduce the evaluation period or provide a longer time period of data. Also, check the data for NA's.")
#         answer=NA
    }
#     if (!is.na(answer)){
#     }
#     else {
        if (period == 0) {
            y = as.matrix(x[1:length(x)])
        }
        else {
            y = as.matrix(x[(length(x)- period + 1):length(x)])
        }
        answer=apply(y, MARGIN = 2, FUN = FUN, ...)
#        print("here")
#     }
#     else {
#         answer = NA
#     }
    return(answer)
    # Example includes how to pass in arguements to the function - must be in order:
    # > rollingStat(gg.ts@Data[,1],period=12,FUN="SharpeRatio.annualized",rf=.03/12)
    # [1] 1.476426
    # > rollingStat(gg.ts@Data[,1],period=3,FUN="SharpeRatio.annualized",rf=.03/12)
    # [1] 6.804358
    # > rollingStat(gg.ts@Data[,1],period=3,FUN="SharpeRatio.annualized",rf=0)
    # [1] 8.253612

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: rollingStat.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################