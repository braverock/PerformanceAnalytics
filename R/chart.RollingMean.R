#' chart the rolling mean return
#' 
#' A wrapper to create a rolling mean return chart with 95% confidence bands.
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param width number of periods to apply rolling function window over
#' @param xaxis if true, draws the x axis
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param lwd set the line width, same as in \code{\link{plot}}.  Specified in
#' order of the main line and the two confidence bands.
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
#' data(edhec)
#' chart.RollingMean(edhec[, 9, drop = FALSE])
#' 
#' @export 
chart.RollingMean <-
function (R, width = 12, xaxis = TRUE, ylim = NULL, lwd=c(2,1,1), ..., fill = NA)
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
    x.mean = rollapply(x[,1,drop=FALSE], width = width, FUN = "mean", fill=fill, align = "right")
    x.stdev = rollapply(x[,1,drop=FALSE], width = width, FUN = "sd.xts", fill=fill, align = "right")

    # @todo: allow user to set confidence interval
    # @todo: add chart for StdDev w confidence bands: x.stdev +- 2* x.stdev/sqrt(2*n)
    lower.band = x.mean - 2 * x.stdev/sqrt(width)
    upper.band = x.mean + 2 * x.stdev/sqrt(width)

    result = merge(x.mean,lower.band,upper.band)

    # Set ylim correctly to allow for confidence bands
    if(is.null(ylim[1]))
        ylim = range(result, na.rm=TRUE)

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
    chart.TimeSeries(result, ylim = ylim, xaxis = xaxis, ylab = "Return", lty = c(1,2,2), colorset = c("black","darkgray","darkgray"), main=main, ... = ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
