#' Calculate and display a table of capture ratio and related statistics
#' 
#' Creates a table of capture ratios and similar metrics for a set of returns
#' against a benchmark.
#' 
#' This table will show statistics pertaining to an asset against a set of
#' benchmarks, or statistics for a set of assets against a benchmark.
#' \code{table.CaptureRatios} shows only the capture ratio;
#' \code{table.UpDownRatios} shows three: the capture ratio, the number ratio,
#' and the percentage ratio.
#' 
#' @param Ra a vector of returns to test, e.g., the asset to be examined
#' @param Rb a matrix, data.frame, or timeSeries of benchmark(s) to test the
#' asset against.
#' @param digits number of digits to round results to for presentation
#' @author Peter Carl
#' @seealso \code{\link{UpDownRatios}}, \code{\link{chart.CaptureRatios}}
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' table.CaptureRatios(managers[,1:6], managers[,7,drop=FALSE])
#' table.UpDownRatios(managers[,1:6], managers[,7,drop=FALSE])
#' 
#' result = t(table.UpDownRatios(managers[,1:6], managers[,7,drop=FALSE]))
#' colnames(result)=colnames(managers[,1:6])
#' textplot(result, rmar = 0.8, cmar = 1.5,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=15, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
#' title(main="Capture Ratios for EDHEC LS EQ")
#' 
#' @aliases table.CaptureRatios table.UpDownRatios
#' 
#' @export
table.CaptureRatios <-
function (Ra, Rb, digits = 4)
{# @author Peter Carl

    # FUNCTION:

    Ra = checkData(Ra)
    Rb = checkData(Rb)

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    result.df = data.frame(NULL)

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        # for(column.b in 1:columns.b) { # against each asset passed in as Rb
            row.df = data.frame(NULL)
            merged.assets = merge(Ra[,column.a,drop=FALSE], Rb[,1,drop=FALSE])
            merged.assets = na.omit(merged.assets) 

            UpCapture = UpDownRatios(Ra=merged.assets[,1], Rb=merged.assets[,2], method="Capture", side="Up")
            DnCapture = UpDownRatios(Ra=merged.assets[,1], Rb=merged.assets[,2], method="Capture", side="Down")

            row.df = cbind(UpCapture, DnCapture)
            rownames(row.df) = columnnames.a[column.a]
            result.df = rbind(result.df, row.df)
        # }
    }

    colnames(result.df) = c("Up Capture", "Down Capture")

    result.df = base::round(result.df, digits)
    result.df
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
