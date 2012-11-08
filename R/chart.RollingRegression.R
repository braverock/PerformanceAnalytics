#' A wrapper to create charts of relative regression performance through time
#' 
#' A wrapper to create a chart of relative regression performance through time
#' 
#' A group of charts in \code{charts.RollingRegression} displays alpha, beta,
#' and R-squared estimates in three aligned charts in a single device.
#' 
#' The attribute parameter is probably the most confusing.  In mathematical
#' terms, the different choices yield the following:
#' 
#' Alpha - shows the y-intercept\cr Beta - shows the slope of the regression
#' line\cr R-Squared - shows the degree of fit of the regression to the data\cr
#' 
#' \code{chart.RollingQuantileRegression} uses \code{\link[quantreg]{rq}}
#' rather than \code{\link[stats]{lm}} for the regression, and may be more
#' robust to outliers in the data.
#' 
#' @aliases chart.RollingRegression chart.RollingQuantileRegression
#' charts.RollingRegression
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param width number of periods to apply rolling function window over
#' @param attribute one of "Beta","Alpha","R-Squared" for which attribute to
#' show
#' @param main set the chart title, same as in \code{plot}
#' @param na.pad TRUE/FALSE If TRUE it adds any times that would not otherwise
#' have been in the result with a value of NA. If FALSE those times are
#' dropped.
#' @param legend.loc used to set the position of the legend
#' @param event.labels 	if not null and event.lines is not null, this will apply a 
#' list of text labels to the vertical lines drawn
#' @param \dots any other passthru parameters to \code{\link{chart.TimeSeries}}
#' @note Most inputs are the same as "\code{\link{plot}}" and are principally
#' included so that some sensible defaults could be set.
#' @author Peter Carl
#' @seealso \code{\link[stats]{lm}} \cr \code{\link[quantreg]{rq}} \cr
#' @keywords ts multivariate distribution models hplot
#' @examples
#' 
#' # First we load the data
#' data(managers)
#' chart.RollingRegression(managers[, 1, drop=FALSE], 
#' 		managers[, 8, drop=FALSE], Rf = .04/12)
#' charts.RollingRegression(managers[, 1:6], 
#' 		managers[, 8, drop=FALSE], Rf = .04/12, 
#' 		colorset = rich6equal, legend.loc="topleft")
#' dev.new()
#' chart.RollingQuantileRegression(managers[, 1, drop=FALSE], 
#' 		managers[, 8, drop=FALSE], Rf = .04/12)
#' # not implemented yet
#' #charts.RollingQuantileRegression(managers[, 1:6], 
#' #		managers[, 8, drop=FALSE], Rf = .04/12, 
#' #		colorset = rich6equal, legend.loc="topleft")
#' 
#' @export
chart.RollingRegression <-
function (Ra, Rb, width = 12, Rf = 0, attribute = c("Beta", "Alpha", "R-Squared"), main=NULL, na.pad = TRUE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # R-Squared could deliver adjusted R-Squared if we wanted

    # FUNCTION:

    # Transform input data to a data frame
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    #Rf = checkDataMatrix(Rf)
    attribute=attribute[1]

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    # @todo: make an excess return function and use it here
    Ra.excess = Return.excess(Ra, Rf)
    Rb.excess = Return.excess(Rb, Rf)

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        for(column.b in 1:columns.b) { # against each asset passed in as Rb
            merged.assets = merge(Ra.excess[,column.a,drop=FALSE], Rb.excess[,column.b,drop=FALSE])
            if(attribute == "Alpha")
                column.result = rollapply(na.omit(merged.assets), width = width, FUN= function(x) lm(x[,1,drop=FALSE]~x[,2,drop=FALSE])$coefficients[1], by = 1, by.column = FALSE, fill = na.pad, align = "right")
            if(attribute == "Beta")
                column.result = rollapply(na.omit(merged.assets), width = width, FUN= function(x) lm(x[,1,drop=FALSE]~x[,2,drop=FALSE])$coefficients[2], by = 1, by.column = FALSE, fill = na.pad, align = "right")
            if(attribute == "R-Squared")
                column.result = rollapply(na.omit(merged.assets), width = width, FUN= function(x) summary(lm(x[,1,drop=FALSE]~x[,2,drop=FALSE]))$r.squared, by = 1, by.column = FALSE, align = "right")

            # some backflips to name the single column zoo object
            column.result.tmp = xts(column.result)
            colnames(column.result.tmp) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
            column.result = xts(column.result.tmp, order.by = time(column.result))

            if(column.a == 1 & column.b == 1)
                Result.calc = column.result
            else
                Result.calc = merge(Result.calc, column.result)
        }
    }

    if(is.null(main)){
      freq = periodicity(Ra)

      switch(freq$scale,
          minute = {freq.lab = "minute"},
          hourly = {freq.lab = "hour"},
          daily = {freq.lab = "day"},
          weekly = {freq.lab = "week"},
          monthly = {freq.lab = "month"},
          quarterly = {freq.lab = "quarter"},
          yearly = {freq.lab = "year"}
      )

      main = paste("Rolling ",width,"-",freq.lab," ", attribute, sep="")
    }
    chart.TimeSeries(Result.calc, main = main, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
