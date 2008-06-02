`chart.RollingMean` <-
function (R, width = 12, xaxis = TRUE, ylim = NULL, na.pad = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a rolling mean return chart with 95% confidence bands.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.

    # Outputs:
    # A timeseries line charts of the rolling mean, with error bars

    # FUNCTION:
    x = checkData(R, method = "zoo")

    # Get dimensions and labels
    columnnames = colnames(x)

    # Calculate

    x.mean = rollapply(na.omit(x[,1,drop=FALSE]), width = width, FUN = "mean", na.pad = na.pad, align = "right")
    x.stdev = rollapply(na.omit(x[,1,drop=FALSE]), width = width, FUN = "stdev", na.pad = na.pad, align = "right")

    # @todo: allow user to set confidence interval
    # @todo: add chart for StdDev w confidence bands: x.stdev +- 2* x.stdev/sqrt(2*n)
    lower.band = x.mean - 2 * x.stdev/sqrt(width)
    upper.band = x.mean + 2 * x.stdev/sqrt(width)

    result = merge(x.mean,lower.band,upper.band)

    # Set ylim correctly to allow for confidence bands
    if(is.null(ylim[1]))
        ylim = range(result)

    # The first row is the annualized returns
    chart.TimeSeries(result, ylim = ylim, xaxis = xaxis, ylab = "Return", lty = c(1,2,2), colorset = c("black","darkgray","darkgray"), main = paste(columnnames[1], " Rolling ",width,"-Month Mean Return",sep=""), ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.RollingMean.R,v 1.4 2008-06-02 16:05:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/03/22 13:46:50  peter
# - uses checkData
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################