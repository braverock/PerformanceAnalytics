`apply.fromstart` <-
function (R, FUN = "mean" , gap = 1, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A function to calculate a function from the start of the timeseries

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # FUN: any function that can be evaluated using a single set of returns
    #   (e.g., rolling beta won't work, but Return.annualizeds will)
    # gap: the number of data points required for the calculation to start

    # Outputs:
    # A timeseries in a zoo object of the calculation results

    # FUNCTION:

    # Coerce input data into a zoo object
    R = checkData(R, method = "zoo")

    # Get dimensions and labels
    columns = ncol(R)
    columnnames = colnames(R)

    # Calculate

    for(column in 1:columns) {
        # the drop=FALSE flag is essential for when the zoo object only has one column
        column.Return.calc=zoo(NA, order.by = time(R))
        for(i in gap:length(time(R))) {
            data.zoo = window(R,start = as.Date(start(R)), end = as.Date(time(R[i])))
            column.Return.calc[i]=apply(as.matrix(data.zoo[,,drop=FALSE]), FUN = FUN, ..., MARGIN = 2)
        }
        if(column == 1)
            Return.calc = column.Return.calc
        else
            Return.calc = merge(Return.calc,column.Return.calc)
    }

    if(!is.null(ncol(Return.calc)))
        colnames(Return.calc) = columnnames

    return(Return.calc)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: apply.fromstart.R,v 1.3 2007-03-20 10:48:29 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.2 2007-03-20 05:44:46  brian
# - change F to FALSE to pass R CMD check
#
# Revision 1.1  2007/03/20 03:28:56  peter
# - uses zoo functions to apply functions to expanding windows
###############################################################################