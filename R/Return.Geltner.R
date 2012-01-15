Return.Geltner <-
function (Ra, ...)
{ # @author Brian G. Peterson, Peter Carl

    # Description:
    # Geltner Returns came from real estate where they are used to uncover a
    # liquidity-adjusted return series.

    # Ra    return vector

    # Function:
    R = checkData(Ra, method="xts")
    # Get dimensions and labels
    columns.a = ncol(R)
    columnnames.a = colnames(R)

    clean.geltner <- function(column.R) {
        # compute the lagged return series
        lagR = lag(column.R, k=1)
        # compute the first order autocorrelation
        f_acf = as.numeric(acf(as.numeric(column.R), plot = FALSE)[1][[1]])
        # now calculate and return the Geltner series
        column.geltner = (column.R-(lagR*f_acf))/(1-f_acf)
    }

    for(column.a in 1:columns.a) { # for each asset passed in as R
        # clean the data and get rid of NAs
        column.geltner = na.skip(R[,column.a],clean.geltner)

        if(column.a == 1)  { geltner = column.geltner }
        else { geltner = cbind (geltner, column.geltner) }

    }

    colnames(geltner) = columnnames.a

    # RESULTS:
    return(reclass(geltner,match.to=Ra))

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