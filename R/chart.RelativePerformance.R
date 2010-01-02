`chart.RelativePerformance` <-
function (Ra, Rb, main = "Relative Performance", xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ylog = FALSE, elementcolor = "darkgray", lty = 1, cex.legend=.7, ...)
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
    Ra = checkData(Ra)
    Rb = checkData(Rb)

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
    chart.TimeSeries(Result.calc, xaxis = xaxis, main = main, col = colorset, ylog = ylog, lty = lty, ...)
    abline(h=1,col=elementcolor)
    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = cex.legend, border.col = elementcolor, lty = lty, lwd = 2, bg = "white", legend = columnnames)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.12  2009-03-20 03:22:53  peter
# - added xts
#
# Revision 1.11  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.10  2008/02/15 04:22:13  peter
# - added lty parameters to plot and legend
#
# Revision 1.9  2007/12/27 18:43:15  peter
# - added solid reference line
#
# Revision 1.8  2007/04/14 13:24:54  brian
# - standardize on Ra for returns of asset
#
# Revision 1.7  2007/03/20 10:44:46  brian
# - change F to FALSE to pass R CMD check
#
# Revision 1.6  2007/03/17 01:24:04  brian
# - drop=FALSE to pass R CMD check
#
# Revision 1.5  2007/03/17 00:25:16  peter
# - uses checkData
# - corrected calculation
#
# Revision 1.4  2007/03/15 01:15:03  brian
# - replace drop=F with drop=FALSE for R CMD check compatibility
#
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