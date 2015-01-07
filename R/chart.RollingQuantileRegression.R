#' @rdname chart.RollingRegression
#' @export 
chart.RollingQuantileRegression <-
function (Ra, Rb, width = 12, Rf = 0, attribute = c("Beta", "Alpha", "R-Squared"), main=NULL, na.pad = TRUE, ...)
{ # @author Peter Carl, Brian Peterson

    # Check to see if the required libraries are loaded
    if(!require("quantreg", quietly=TRUE))
        stop("package", sQuote("quantreg"), "is needed.  Stopping")

    # DESCRIPTION:
    # A wrapper to create a chart of rolling quantile regression through time

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
                column.result = rollapply(na.omit(merged.assets[,,drop=FALSE]), width = width, FUN= function(x) quantreg::rq(x[,1,drop=FALSE]~x[,2,drop=FALSE])$coefficients[1], by = 1, by.column = FALSE, fill = if(na.pad) NA, align = "right")
            if(attribute == "Beta")
                column.result = rollapply(na.omit(merged.assets[,,drop=FALSE]), width = width, FUN= function(x) quantreg::rq(x[,1,drop=FALSE]~x[,2,drop=FALSE])$coefficients[2], by = 1, by.column = FALSE, fill = if(na.pad) NA, align = "right")
            if(attribute == "R-Squared")
                column.result = rollapply(na.omit(merged.assets[,,drop=FALSE]), width = width, FUN= function(x) summary(quantreg::rq(x[,1,drop=FALSE]~x[,2,drop=FALSE]))$r.squared, by = 1, by.column = FALSE, fill = if(na.pad) NA, align = "right")

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

      main = paste("Rolling",freq.lab, attribute, sep=" ")
    }
    chart.TimeSeries(Result.calc, main = main, ...)

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