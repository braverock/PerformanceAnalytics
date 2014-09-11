#' chart rolling correlation fo multiple assets
#' 
#' A wrapper to create a chart of rolling correlation metrics in a line chart
#' 
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param width number of periods to apply rolling function window over
#' @param xaxis if true, draws the x axis
#' @param legend.loc places a legend into one of nine locations on the chart:
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#' center.
#' @param colorset color palette to use, set by default to rational choices
#' @param \dots any other passthru parameters
#' @param fill a three-component vector or list (recycled otherwise) providing 
#' filling values at the left/within/to the right of the data range. See the 
#' fill argument of \code{\link{na.fill}} for details.
#' @details The previous parameter \code{na.pad} has been replaced with \code{fill}; use \code{fill = NA} instead of 
#' \code{na.pad = TRUE}, or \code{fill = NULL} instead of \code{na.pad = FALSE}.
#' @author Peter Carl
###keywords ts multivariate distribution models hplot
#' @examples
#' 
#' # First we get the data
#' data(managers)
#' chart.RollingCorrelation(managers[, 1:6, drop=FALSE], 
#' 		managers[, 8, drop=FALSE], 
#' 		colorset=rich8equal, legend.loc="bottomright", 
#' 		width=24, main = "Rolling 12-Month Correlation")
#' 
#' @export 
chart.RollingCorrelation <-
function (Ra, Rb, width = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), ..., fill=NA)
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
            column.calc = rollapply(na.omit(merged.assets[,,drop=FALSE]), width = width, FUN= function(x) cor(x[,1,drop=FALSE], x[,2,drop=FALSE]), by = 1, by.column = FALSE, fill = fill, align = "right")

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

    chart.TimeSeries(Result.calc, xaxis = xaxis, colorset = colorset, legend.loc = legend.loc, ylim = c(-1,1), ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
