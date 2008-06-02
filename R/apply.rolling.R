`apply.rolling` <-
function (R, width = 12, FUN = "mean", na.pad = TRUE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create timeseries object of rolling performance metrics 

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # FUN: any function that can be evaluated using a single set of returns
    #   (e.g., rolling beta won't work, but Return.annualizeds will)

    # Outputs:
    # A timeseries object of the calculated series

    # FUNCTION:

    # Transform input data to a matrix
    x = checkData(R, method = "zoo")

    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)

    # Calculate

    for(column in 1:columns) {
        # the drop=FALSE flag is essential for when the zoo object only has one column
        column.Return.calc = rollapply(na.omit(x[,column,drop=FALSE]), width = width, FUN = FUN, ..., na.pad = na.pad, align = "right")
        if(column == 1)
            Return.calc = column.Return.calc
        else
            Return.calc = merge(Return.calc,column.Return.calc)
    }

    if(ncol(Return.calc) == 1) {
        # some backflips to name the single column zoo object
        Return.calc = as.matrix(Return.calc)
        colnames(Return.calc) = columnnames
        Return.calc = zoo(Return.calc, order.by = rownames(Return.calc))
    }
    else
        colnames(Return.calc) = columnnames

    return(Return.calc)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: apply.rolling.R,v 1.3 2008-06-02 16:05:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.2  2007/04/04 02:45:04  peter
# - added some backflips to name the single column zoo object
#
# Revision 1.1  2007/03/20 03:28:20  peter
# - uses zoo functions to apply functions to rolling windows
#
###############################################################################
