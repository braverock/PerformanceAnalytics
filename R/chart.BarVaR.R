#' Periodic returns in a bar chart with risk metric overlay
#' 
#' Plots the periodic returns as a bar chart overlayed with a risk metric
#' calculation.
#' 
#' Note that \code{StdDev} and \code{VaR} are symmetric calculations, so a high
#' and low measure will be plotted.  \code{ModifiedVaR}, on the other hand, is
#' assymetric and only a lower bound will be drawn.
#' 
#' Creates a plot of time on the x-axis and vertical lines for each period to
#' indicate value on the y-axis.  Overlays a line to indicate the value of a
#' risk metric calculated at that time period.
#' 
#' \code{charts.BarVaR} places multile bar charts in a single 
#' graphic, with associated risk measures 
#' 
#' @name chart.BarVaR
#' @aliases chart.BarVaR charts.BarVaR
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param width periods specified for rolling-period calculations.  Note that
#' VaR, ES, and Std Dev with width=0 are calculated from the start of the
#' timeseries
#' @param gap numeric number of periods from start of series to use to train
#' risk calculation
#' @param methods Used to select the risk parameter of trailing \code{width}
#' returns to use: May be any of: \itemize{ \item none - does not add a risk
#' line, \item ModifiedVaR - uses Cornish-Fisher modified VaR, \item
#' GaussianVaR - uses traditional Value at Risk, \item HistoricalVaR -
#' calculates historical Value at Risk, \item ModifiedES - uses Cornish-Fisher
#' modified Expected Shortfall, \item GaussianES - uses traditional Expected
#' Shortfall, \item HistoricalES - calculates historical Expected Shortfall,
#' \item StdDev - per-period standard deviation }
#' @param p confidence level for \code{VaR} or \code{ModifiedVaR} calculation,
#' default is .99
#' @param all if TRUE, calculates risk lines for each column given in R.  If
#' FALSE, only calculates the risk line for the first column
#' @param clean the method to use to clean outliers from return data prior to
#' risk metric estimation. See \code{\link{Return.clean}} and \code{\link{VaR}}
#' for more detail
#' @param show.clean if TRUE and a method for 'clean' is specified, overlays
#' the actual data with the "cleaned" data.  See \code{\link{Return.clean}} for
#' more detail
#' @param \dots any other passthru parameters to \code{\link{chart.TimeSeries}}
#' @param show.horizontal if TRUE, shows a line across the timeseries at the
#' value of the most recent VaR estimate, to help the reader evaluate the
#' number of exceptions thus far
#' @param show.symmetric if TRUE and the metric is symmetric, this will show
#' the metric's positive values as well as negative values, such as for method
#' "StdDev".
#' @param show.endvalue if TRUE, show the final (out of sample) value
#' @param show.greenredbars if TRUE, show the per-period returns using green
#' and red bars for positive and negative returns
#' @param show.yaxis one of "all", "firstonly", "alternating", or "none" to
#' control where y axis is plotted in multipanel charts
#' @param perpanel default NULL, controls column display
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param lwd set the line width, same as in \code{\link{plot}}
#' @param lty set the line type, same as in \code{\link{plot}}
#' @param legend.loc legend location, such as in \code{\link{chart.TimeSeries}}
#' @param ypad adds a numerical padding to the y-axis to keep the data away
#' when legend.loc="bottom".  See examples below.
#' @param legend.cex sets the legend text size, such as in
#' \code{\link{chart.TimeSeries}}
#' @param cex.legend sets the legend text size, such as in
#' \code{\link{chart.TimeSeries}}
#' @param main sets the title text, such as in \code{\link{chart.TimeSeries}}
#' @param colorset color palette to use, such as in
#' \code{\link{chart.TimeSeries}}
#' @author Peter Carl
#' @seealso \code{\link{chart.TimeSeries}} \cr \code{\link{plot}} \cr
#' \code{\link{ES}} \cr \code{\link{VaR}} \cr \code{\link{Return.clean}}
###keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(managers)
#' # plain
#' chart.BarVaR(managers[,1,drop=FALSE], main="Monthly Returns")
#' 
#' # with risk line
#' chart.BarVaR(managers[,1,drop=FALSE], 
#' 		methods="HistoricalVaR", 
#' 		main="... with Empirical VaR from Inception")
#' 
#' # with lines for all managers in the sample
#' 
#' chart.BarVaR(managers[,1:6], 
#' 		methods="GaussianVaR", 
#' 		all=TRUE, lty=1, lwd=2, 
#' 		colorset= c("red", rep("gray", 5)), 
#' 		main="... with Gaussian VaR and Estimates for Peers")
#' 
#' \dontrun{
#' # not run on CRAN because of example time
#'
#' # with multiple methods
#' chart.BarVaR(managers[,1,drop=FALSE],
#' 		methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"), 
#' 		main="... with Multiple Methods")
#' 
#' # cleaned up a bit
#' chart.BarVaR(managers[,1,drop=FALSE],
#' 		methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"), 
#' 		lwd=2, ypad=.01, 
#' 		main="... with Padding for Bottom Legend")
#' 
#' # with 'cleaned' data for VaR estimates
#' chart.BarVaR(managers[,1,drop=FALSE],
#' 		methods=c("HistoricalVaR", "ModifiedVaR"), 
#' 		lwd=2, ypad=.01, clean="boudt", 
#' 		main="... with Robust ModVaR Estimate")
#' 
#' # Cornish Fisher VaR estimated with cleaned data, 
#' # with horizontal line to show exceptions
#' chart.BarVaR(managers[,1,drop=FALSE],
#' 		methods="ModifiedVaR", 
#' 		lwd=2, ypad=.01, clean="boudt", 
#' 		show.horizontal=TRUE, lty=2, 
#' 		main="... with Robust ModVaR and Line for Identifying Exceptions")
#' }
#' 
#' @rdname chart.BarVaR
#' @export
chart.BarVaR <- function (R, width = 0, gap = 12, 
                            methods = c("none", "ModifiedVaR", "GaussianVaR", 
										"HistoricalVaR", "StdDev", "ModifiedES", 
										"GaussianES", "HistoricalES"), 
                            p=0.95, 
                            clean = c("none", "boudt","geltner"), 
                            all = FALSE, 
                            ..., 
                            show.clean = FALSE, 
                            show.horizontal = FALSE, 
                            show.symmetric = FALSE, 
                            show.endvalue = FALSE, 
                            show.greenredbars = FALSE, 
                            legend.loc="bottomleft", 
                            ylim = NA, 
                            lwd = 2, 
                            colorset = 1:12, 
                            lty = c(1,2,4,5,6), 
                            ypad=0, 
                            legend.cex = 0.8 )
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of monthly returns in a bar chart.  This is
    # a difficult enough graph to read that it doesn't get much use.  Still,
    # it is useful for viewing a single set of data.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # width: periods specified for rolling-period calculations

    # Outputs:
    # A timeseries bar chart of the data series
    # Metrics with width=0 are calculated from the start of the timeseries

    # FUNCTION:

    # Transform input data to a timeseries object
    x = checkData(R)

    # Set up dimensions and labels
    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = time(x)
    legend.txt = NULL
    symmetric = NULL
    risk.line=TRUE

    freq = periodicity(x)

    switch(freq$scale,
        minute = {freq.lab = "minute"},
        hourly = {freq.lab = "hour"},
        daily = {freq.lab = "day"},
        weekly = {freq.lab = "week"},
        monthly = {freq.lab = "month"},
        quarterly = {freq.lab = "quarter"},
        yearly = {freq.lab = "year"}
    )

    time(x) = as.Date(time(x)) # this is here because merge.zoo is not behaving as expected when date formats are not consistent
    if(methods[1]=="none"){
        methods=NULL
        risk.line=FALSE
    }

    colors = colorRamp(c(colorset[1],"white"))
    if(length(methods)>1){
        columns = 1 # if there's more than one method specified, then we'll ignore columns other than the first
        colorset = c(colorset[1], rgb(colors(.25),maxColorValue=255), rgb(colors(.5),maxColorValue=255), rgb(colors(.75),maxColorValue=255))
    }
    clean = clean[1]

    risk = xts(rep(NA,length(time(x))),order.by=time(x))
    column.risk = xts(rep(0,length(time(x))),order.by=time(x))

    if (!all)
        columns = 1

    bar.color = colorset[1]
    if (show.clean){
        bar.color = rgb(colors(.75),maxColorValue=255)
    }

    x.orig = x

    if(show.clean){
        x = na.skip(x, Return.clean, method=clean) 
    }

    if(risk.line){
        for(column in 1:columns) {
            for(method in methods) {
                switch(method,
                    StdDev = {
                        symmetric = c(symmetric, TRUE)
                        if(width > 0){
                            column.risk = -1 * apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "sd")
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-",freq.lab," Std Dev",sep=""))
                        }
                        else {
                            column.risk = -1 * apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "sd")
                            if(column==1)
                                legend.txt = c(legend.txt, "Std Dev")
                        }
                    },
                    GaussianVaR = {
                        symmetric = c(symmetric, TRUE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "VaR", p = p, method="gaussian", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab," Gaussian VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "VaR", p = p, method="gaussian", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Gaussian VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    },
                    ModifiedVaR = {
                        symmetric = c(symmetric, FALSE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "VaR", p = p, method="modified", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab, " Modified VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "VaR", p = p, method="modified", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Modified VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    },
                    HistoricalVaR = {
                        symmetric = c(symmetric, FALSE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "VaR", p = p, method="historical") #hVaR = quantile(x,probs=.01)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab," Historical VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "VaR", p = p, method="historical")
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Historical VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    },
                    GaussianES = {
                        symmetric = c(symmetric, TRUE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "ES", p = p, method="gaussian", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab," Gaussian ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "ES", p = p, method="gaussian", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Gaussian ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    },
                    ModifiedES = {
                        symmetric = c(symmetric, FALSE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "ES", p = p, method="modified", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab, " Modified ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "ES", p = p, method="modified", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Modified ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    },
                    HistoricalES = {
                        symmetric = c(symmetric, FALSE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "ES", p = p, method="historical") #hES = quantile(x,probs=.01)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab," Historical ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "ES", p = p, method="historical")
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Historical ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    }
                ) # end switch

                risk = merge(risk,column.risk)
            } # end method loop
        } # end column loop
    }
    else {
        risk = 0
        legend.txt = ""
    }
    if(is.na(ylim[1])){
        ylim = range(c(na.omit(as.vector(x.orig[,1])), na.omit(as.vector(risk)), -na.omit(as.vector(risk))))
        ylim = c(ylim[1]-ypad,ylim[2]) # pad the bottom of the chart for the legend
    }
    if(!show.greenredbars){
    	chart.TimeSeries(x.orig[,1, drop=FALSE], type = "h", colorset = bar.color, legend.loc = NULL, ylim = ylim, lwd = lwd, lend="butt", ...)
    }
    else {
        positives = x.orig[,1,drop=FALSE]
        for(row in 1:length(x.orig[,1,drop=FALSE])){ 
            positives[row,]=max(0,x.orig[row,1])
        }
        negatives = x.orig[,1,drop=FALSE]
        for(row in 1:length(x.orig[,1,drop=FALSE])){ 
            negatives[row,]=min(0,x.orig[row,1])
        }
        chart.TimeSeries(positives, type = "h", legend.loc = NULL, ylim = ylim, lwd = lwd, lend="butt", colorset="darkgreen", ...)
        lines(1:length(x.orig[,1]), negatives, type="h", lend="butt", col="darkred", lwd=lwd)
    }

    if(show.clean) {
        lines(1:rows, x[,1, drop=FALSE], type="h", col=colorset[1], lwd = lwd, lend="butt")
    }

#     symmetric = symmetric[-1]
# add risk.columns here, since we'll want multiple lines when we have multiple methods
# if we have multiple methods, we'll want multiple lty's

    if(risk.line){
        risk.columns = ncol(risk)
        if(length(lty)==1)
            lty = rep(lty, risk.columns)
        for(column in (risk.columns):2) {
            if (show.symmetric && symmetric[column-1]){
                lines(1:rows, -risk[,column], col = colorset[column-1], lwd = 1, type = "l", lty=lty[column-1])
            }
        }
        for(column in (risk.columns):2) {
            lines(1:rows, risk[,column], col = colorset[column-1], lwd = 1, type = "l", lty=lty[column-1])
            if(show.horizontal)
                lines(1:rows, rep(tail(risk[,2],1),rows), col = colorset[1], lwd=1, type="l", lty=1)
	    if(show.endvalue){
		points(rows, tail(risk[,2],1), col = colorset[1], pch=20, cex=.7)
		mtext(paste(round(100*tail(risk[,2],1),2),"%", sep=""), line=.5, side = 4, at=tail(risk[,2],1), adj=0, las=2, cex = 0.7, col = colorset[1])
	    }
        }
    }

    if(legend.txt[1] != "" & !is.null(legend.loc))
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = legend.cex, border.col = "grey", lwd = 1, lty=lty, bty = "n", legend = legend.txt, horiz=TRUE)

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
