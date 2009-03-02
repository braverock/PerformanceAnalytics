`Return.Geltner` <-
function (Ra, ...)
{ # @author Brian G. Peterson, Peter Carl

    # Description:
    # Geltner Returns came from real estate where they are used to uncover a
    # liquidity-adjusted return series.

    # Ra    return vector

    # Function:
    Ra = checkData(Ra, method="zoo")
    # Get dimensions and labels
    columns.a = ncol(Ra)
    columnnames.a = colnames(Ra)
    geltner = zoo(NULL)

    for(column.a in 1:columns.a) { # for each asset passed in as R
        column.Ra = zoo(NULL)
        # clean the data and get rid of NAs
        column.Ra = na.omit(checkData (Ra[, column.a, drop = FALSE], method = "zoo", ...=...))

        # compute the lagged return series
        lagRa = lag(column.Ra, k=-1)
        # compute the first order autocorrelation
        f_acf = as.numeric(acf(as.numeric(column.Ra), plot = FALSE)[1][[1]])
        # now calculate and return the Geltner series
        column.geltner = (column.Ra-(lagRa*f_acf))/(1-f_acf)

        if(column.a == 1) 
            geltner = column.geltner
        else 
	        geltner = merge (geltner, column.geltner)

    }

    colnames(geltner) = columnnames.a

    # RESULTS:
    return(geltner)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.Geltner.R,v 1.5 2009-03-02 03:21:26 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
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