`chart.RollingCorrelation` <-
function (Ra, Rb, width = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), na.pad = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of rolling correlation metrics in a line chart

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
            merged.assets = merge(Ra[,column.a,drop=FALSE], Rb[,column.b,drop=FALSE])
            column.calc = rollapply(na.omit(merged.assets[,,drop=FALSE]), width = width, FUN= function(x) cor(x[,1,drop=FALSE], x[,2,drop=FALSE]), by = 1, by.column = FALSE, na.pad = na.pad, align = "right")

            # some backflips to name the single column zoo object
            column.calc.tmp = xts(column.calc)
            colnames(column.calc.tmp) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
            column.calc = xts(column.calc.tmp, order.by = time(column.calc))

            if(column.a == 1 & column.b == 1)
                Result.calc = column.calc
            else
                Result.calc = merge(Result.calc, column.calc)
        }
    }

    chart.TimeSeries(Result.calc, xaxis = xaxis, col = colorset, legend.loc = legend.loc, ylim = c(-1,1), ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.RollingCorrelation.R,v 1.8 2009-03-20 03:22:53 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.6  2007/10/11 03:56:12  peter
# - fixed so that it will handle zoo objects with yearmon dates
#
# Revision 1.5  2007/04/14 15:42:35  brian
# - standardize Ra as first argument for asset returns
#
# Revision 1.4  2007/03/15 01:15:03  brian
# - replace drop=F with drop=FALSE for R CMD check compatibility
#
# Revision 1.3  2007/03/14 03:12:44  peter
# - uses checkData function
# - handles uneven lengths of timeseries data
# - handles multiple assets and benchmarks for cross correlation
# - uses zoo rollapply function
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################