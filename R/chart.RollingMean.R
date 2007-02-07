`chart.RollingMean` <-
function (R, n=12, xaxis = TRUE, ylim = NULL, main = paste("Rolling ",n,"-Month Mean Return",sep=""), trim = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a rolling mean return chart with 95% confidence bands.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest.

    # Outputs:
    # A timeseries line charts of the rolling mean, with error bars

    # FUNCTION:
    x = checkDataMatrix(R)
    x = as.data.frame(x[,1]) # @todo: handle multiple columns?
    ncols = ncol(x)
    rows = nrow(x)

    x.mean = rollingFunction(x, n = n, trim = trim, na.rm = FALSE, firstcolumn = 1, digits = 8, FUN = "mean")
    x.stdev = rollingFunction(x, n = n, trim = trim, na.rm = FALSE, firstcolumn = 1, digits = 8, FUN = "stdev")

    # @todo: allow user to set confidence interval
    # @todo: add chart for StdDev w confidence bands: x.stdev +- 2* x.stdev/sqrt(2*n)
    lower.band = x.mean - 2 * x.stdev/sqrt(n)
    upper.band = x.mean + 2 * x.stdev/sqrt(n)

    result = as.matrix(cbind(x.mean,lower.band,upper.band))

    # Set ylim correctly to allow for confidence bands
    if(is.null(ylim[1]))
        ylim = range(result[!is.na(result)])

    # The first row is the annualized returns
    chart.TimeSeries(result, ylim = ylim, main = main, xaxis = xaxis, ylab = "Mean Return", lty = c(1,2,2), colorset = c("black","darkgray","darkgray"), ...)

    #lines(1:rows, as.matrix(lower.band), col = "darkgray", lwd = 1, lty = "dashed", type = "l")
    #lines(1:rows, as.matrix(upper.band), col = "darkgray", lwd = 1, lty = "dashed", type = "l")
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.RollingMean.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################