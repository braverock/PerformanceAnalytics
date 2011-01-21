chart.RollingPerformance <- function (R, width = 12, FUN = "Return.annualized", ..., na.pad = TRUE, ylim=NULL, main=NULL)
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
	funargs$na.pad=na.pad
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
# Copyright (c) 2004-2011 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################