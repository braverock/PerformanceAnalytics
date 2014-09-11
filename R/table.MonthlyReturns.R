#' Returns Summary: Statistics and Stylized Facts
#' 
#' Returns a basic set of statistics that match the period of the data passed
#' in (e.g., monthly returns will get monthly statistics, daily will be daily
#' stats, and so on)
#' 
#' This was created as a way to display a set of related statistics together
#' for comparison across a set of instruments or funds.  Careful consideration
#' to missing data or unequal time series should be given when intepreting the
#' results.
#' 
#' @aliases table.Stats table.MonthlyReturns
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param ci confidence interval, defaults to 95\%
#' @param digits number of digits to round results to
#' @author Peter Carl
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' table.Stats(edhec[,1:3])
#' t(table.Stats(edhec))
#' 
#' result=t(table.Stats(edhec))
#' require("Hmisc")
#' textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(rep(1,2),rep(3,14))), 
#'          rmar = 0.8, cmar = 1.5,  max.cex=.9, halign = "center", valign = "top", 
#'          row.valign="center", wrap.rownames=10, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
#' title(main="Statistics for EDHEC Indexes")
#' 
#' @rdname table.MonthlyReturns
#' @export 
table.Stats <-
function (R, ci = 0.95, digits = 4)
{# @author Peter Carl

    # DESCRIPTION
    # Returns Summary: Statistics and Stylized Facts

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Returns a basic set of statistics that match the period of the data passed
    # in (e.g., monthly returns will get monthly statistics)

    # FUNCTION:

    y = checkData(R, method = "zoo")

    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)

    cl.vals = function(x, ci) {
            x = x[!is.na(x)]
            n = length(x)
            if (n <= 1)
            return(c(NA, NA))
            se.mean = sqrt(var(x)/n)
            t.val = qt((1 - ci)/2, n - 1)
            mn = mean(x)
            lcl = mn + se.mean * t.val
            ucl = mn - se.mean * t.val
            c(lcl, ucl)
    }
# for each column, do the following:
    for(column in 1:columns) {
        x = as.vector(y[,column])
        x.length = length(x)
        x = x[!is.na(x)]
        x.na = x.length - length(x)
        z = c(
            length(x), 
            x.na, min(x), 
            as.numeric(quantile(x, prob = 0.25, na.rm = TRUE)), 
            median(x), 
            mean(x), 
            exp(mean(log(1+x)))-1,
            as.numeric(quantile(x, prob = 0.75, na.rm = TRUE)), 
            max(x), 
            sqrt(var(x)/length(x)),
            cl.vals(x, ci)[1], 
            cl.vals(x, ci)[2], 
            var(x), 
            sqrt(var(x)),
            skewness(x), 
            kurtosis(x)
            )
        z = base::round(as.numeric(z),digits)
        znames = c(
            "Observations", 
            "NAs", 
            "Minimum", 
            "Quartile 1", 
            "Median", 
            "Arithmetic Mean",
            "Geometric Mean", 
            "Quartile 3", 
            "Maximum", 
            "SE Mean", 
            paste("LCL Mean (",ci,")",sep=""),
            paste("UCL Mean (",ci,")",sep=""), 
            "Variance", 
            "Stdev", 
            "Skewness", 
            "Kurtosis"
            )
        if(column == 1) {
            resultingtable = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    ans = resultingtable
    ans
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
