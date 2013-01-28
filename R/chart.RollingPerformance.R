#' wrapper to create a chart of rolling performance metrics in a line chart
#' 
#' A wrapper to create a chart of rolling performance metrics in a line chart
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param width number of periods to apply rolling function window over
#' @param FUN any function that can be evaluated using a single set of returns
#' (e.g., rolling \code{\link{CAPM.beta}} won't work, but
#' \code{\link{Return.annualized}} will)
#' @param fill a three-component vector or list (recycled otherwise) providing 
#' filling values at the left/within/to the right of the data range. See the 
#' fill argument of \code{\link{na.fill}} for details.
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param \dots any other passthru parameters to \code{\link{plot}} or the
#' function specified
#' @details The parameter \code{na.pad} has been deprecated; use \code{fill = NA} instead of \code{na.pad = TRUE}, 
#' or \code{fill = NULL} instead of \code{na.pad = FALSE}.
#' @author Peter Carl
#' @seealso \code{\link{charts.RollingPerformance}},
#' \code{\link[zoo]{rollapply}}
#' @keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(edhec)
#' chart.RollingPerformance(edhec[, 1:3], width = 24)
#' chart.RollingPerformance(edhec[, 1:3], 
#' 		FUN = 'mean', width = 24, colorset = rich8equal, 
#' 		lwd = 2, legend.loc = "topleft", 
#' 		main = "Rolling 24-Month Mean Return")
#' chart.RollingPerformance(edhec[, 1:3], 
#' 		FUN = 'SharpeRatio.annualized', width = 24, 
#' 		colorset = rich8equal, lwd = 2, legend.loc = "topleft", 
#' 		main = "Rolling 24-Month Sharpe Ratio")
#' 
#' @export 
chart.RollingPerformance <- function (R, width = 12, FUN = "Return.annualized", ...,  ylim = NULL, main = NULL, fill = NA)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of rolling peRformance metrics in a line chart

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # FUN: any function that can be evaluated using a single set of returns
    #   (e.g., rolling beta won't work, but Return.annualizeds will)

    # Outputs:
    # A timeseries line chart of the calculated series

    # FUNCTION:

    # Transform input data to a matrix
    x = checkData(R)

    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)

    # Separate function args from plot args
    dotargs <-list(...)
    funargsmatch = pmatch(names(dotargs), names(formals(FUN)), nomatch = 0L)
	funargs = dotargs[funargsmatch>0L]
	if(is.null(funargs))funargs=list()
	funargs$...=NULL

	plotargs = dotargs[funargsmatch==0L]
    plotargs$...=NULL
	if (!length(plotargs)) plotargs=list()
	
	funargs$width=width
	funargs$FUN=FUN
	funargs$fill = fill
	funargs$align='right'

	# Calculate
    for(column in 1:columns) {
        # the drop=FALSE flag is essential for when the zoo object only has one column
		rollargs<-c(list(data=na.omit(x[,column,drop=FALSE])),funargs)
		column.Return.calc <- do.call(xts:::rollapply.xts,rollargs)
        if(column == 1)
            Return.calc = xts(column.Return.calc)
        else
            Return.calc = merge(Return.calc,column.Return.calc)
    }
    if(is.null(ylim)){
        ylim = c(min(0,min(Return.calc, na.rm=TRUE)),max(Return.calc, na.rm=TRUE))
    }    
    colnames(Return.calc) = columnnames

    if(is.null(main)){

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

        main = paste(columnnames[1], " Rolling ",width,"-",freq.lab," ", FUN,sep="")
    }

	
	plotargs$R=Return.calc
	plotargs$main=main
	plotargs$ylim=ylim
	do.call(chart.TimeSeries,plotargs)
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
