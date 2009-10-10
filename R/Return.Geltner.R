`Return.Geltner` <-
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
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.Geltner.R,v 1.8 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2009-09-02 12:13:29  brian
# - fix for positive lag of lag.xts
#
# Revision 1.6  2009-09-02 11:53:57  brian
# - add na.skip functionality
# - add reclass functionality to returned series
#
# Revision 1.5  2009-03-02 03:21:26  peter
# - fix acf call to pass in numeric
#
# Revision 1.4  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.3  2007/09/11 03:03:45  peter
# - fixed na.omit for column zoo object
#
# Revision 1.1  2007/09/01 16:29:55  brian
# - initial revision of Geltner Returns function
# - seems to only work on rectangular multicolumn inputs
# - may lose rownames
#
###############################################################################