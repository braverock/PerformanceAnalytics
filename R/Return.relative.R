Return.relative <-
function (Ra, Rb, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculate relative returns through time

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # Rb: a matrix, data frame, or timeSeries of returns for a benchmark

    # Outputs:
    # A timeseries line chart of the calculated series

    # FUNCTION:

    # Transform input data to a matrix
    Ra = checkData(Ra, method="zoo")
    Rb = checkData(Rb, method = "zoo")

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        for(column.b in 1:columns.b) { # against each asset passed in as Rb
            merged.columns = merge(Ra[, column.a, drop = FALSE], Rb[, column.b, drop = FALSE])
            cumulative = cumprod(1+na.omit(merged.columns))
            column.calc = cumulative[,1,drop=FALSE]/cumulative[,2,drop=FALSE]
            colnames(column.calc) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = "/")
            if(column.a == 1 & column.b == 1)
                Result.calc = column.calc
            else
                Result.calc = merge(Result.calc,column.calc)
        }
    }
    columnnames = colnames(Result.calc)
    Result.calc = reclass(Result.calc, Ra)
    return(Result.calc)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2011 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################