`chart.RelativePerformance` <-
function (R, Rb, main = "Relative Performance", xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ylog = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # Rb: a matrix, data frame, or timeSeries of returns for a benchmark

    # Outputs:
    # A timeseries line chart of the calculated series

    # FUNCTION:

    # Transform input data to a matrix
    Ra = checkData(R, method="zoo")
    Rb = checkData(Rb, method = "zoo")

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        for(column.b in 1:columns.b) { # against each asset passed in as Rb
            cumulative.a = cumprod(1+na.omit(Ra[,column.a,drop=FALSE]))
            cumulative.b = cumprod(1+na.omit(Rb[,column.b,drop=FALSE]))
            column.calc = cumulative.a/cumulative.b
            colnames(column.calc) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = "/")
            if(column.a == 1 & column.b == 1)
                Result.calc = column.calc
            else
                Result.calc = merge(Result.calc,column.calc)
        }
    }

    # Removes the first color in the colorset to keep consistant with other graphics
    colorset = colorset[-1]

    chart.TimeSeries(Result.calc, xaxis = xaxis, main = main, legend.loc = legend.loc, col = colorset, ylog = ylog, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.RelativePerformance.R,v 1.4 2007-03-15 01:15:03 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/03/13 21:54:11  peter
# - multiple assets can be compared to multiple benchmarks
# - uses dataCheck function
# - allows uneven timeseries
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################