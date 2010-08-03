chart.RollingMean <-
function (R, width = 12, xaxis = TRUE, ylim = NULL, na.pad = FALSE, lwd=c(2,1,1), ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a rolling mean return chart with 95% confidence bands.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.

    # Outputs:
    # A timeseries line charts of the rolling mean, with error bars

    # FUNCTION:
    x = checkData(R)

    # Get dimensions and labels
    columnnames = colnames(x)

    # Calculate

    x.mean = rollapply(na.omit(x[,1,drop=FALSE]), width = width, FUN = "mean", na.pad = na.pad, align = "right")
    x.stdev = rollapply(na.omit(x[,1,drop=FALSE]), width = width, FUN = "sd", na.pad = na.pad, align = "right")

    # @todo: allow user to set confidence interval
    # @todo: add chart for StdDev w confidence bands: x.stdev +- 2* x.stdev/sqrt(2*n)
    lower.band = x.mean - 2 * x.stdev/sqrt(width)
    upper.band = x.mean + 2 * x.stdev/sqrt(width)

    result = merge(x.mean,lower.band,upper.band)

    # Set ylim correctly to allow for confidence bands
    if(is.null(ylim[1]))
        ylim = range(result)


    freq = periodicity(R)

    switch(freq$scale,
        minute = {freq.lab = "minute"},
        hourly = {freq.lab = "hour"},
        daily = {freq.lab = "day"},
        weekly = {freq.lab = "week"},
        monthly = {freq.lab = "month"},
        quarterly = {freq.lab = "quarter"},
        yearly = {freq.lab = "year"}
    )

    main = paste(columnnames[1], " Rolling ",width,"-",freq.lab," Performance",sep="")

    # The first row is the annualized returns
    chart.TimeSeries(result, ylim = ylim, xaxis = xaxis, ylab = "Return", lty = c(1,2,2), colorset = c("black","darkgray","darkgray"), main=main , ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################