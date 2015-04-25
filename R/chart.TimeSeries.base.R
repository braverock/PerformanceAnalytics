#' @rdname chart.TimeSeries
chart.TimeSeries.base <-
function (R, 
          auto.grid=TRUE, 
          xaxis = TRUE, 
          yaxis = TRUE, 
          yaxis.right = FALSE, 
          type = "l", 
          lty = 1, 
          lwd = 2, 
          las = par("las"),
          main = NULL, 
          ylab=NULL, 
          xlab="", 
          date.format.in="%Y-%m-%d", 
          date.format = NULL, 
          xlim = NULL, 
          ylim = NULL, 
          element.color="darkgray", 
          event.lines = NULL, 
          event.labels = NULL, 
          period.areas = NULL, 
          event.color = "darkgray", 
          period.color = "aliceblue", colorset = (1:12), 
          pch = (1:12), 
          legend.loc = NULL, 
          ylog = FALSE, 
          cex.axis=0.8, 
          cex.legend = 0.8, 
          cex.lab = 1, 
          cex.labels = 0.8, 
          cex.main = 1, 
          major.ticks='auto', 
          minor.ticks=TRUE, 
          grid.color="lightgray", 
          grid.lty="dotted", 
          xaxis.labels = NULL, ...)
{ # @author Peter Carl, Brian Peterson

    # DESCRIPTION:
    # Draws a line chart and labels the x-axis with the appropriate dates.
    # This is really a "primitive", since it constructs the elements of a plot
    # to provide lines for each column of data provided.  

    # Inputs:
    # R = assumes that data is a regular time series, not irregular.  Can take
    # any type of object, whether a matrix, data frame, or timeSeries.
    # date.format: allows passing of a date format for the xaxis
    # legend.loc = use this to locate the legend, e.g., "topright"
    # colorset = use the name of any of the palattes above
    # reference.grid = if true, draws a grid aligned with the points on the
    #    x and y axes.
    # xaxis = if true, draws the x axis.
    # event.lines = if not null, will draw vertical lines indicating that an
    #    event happened during that time period.  event.lines should be a list
    #    of dates (e.g., c("09/03","05/06")) formatted the same as date.format.
    #    This function matches the re-formatted row names (dates) with the
    #    events.list, so to get a match the formatting needs to be correct.
    # event.labels = if not null and event.lines is not null, this will apply
    #    labels to the vertical lines drawn.

    # All other inputs are the same as "plot" and are principally included
    # so that some sensible defaults could be set.

    # Output:
    # Draws a timeseries graph of type "line" with some sensible defaults.

    # FUNCTION:

    y = checkData(R)

    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)

    if (is.null(date.format)){
	freq = periodicity(y)
	yr_eq <- ifelse(format(index(first(y)),format="%Y")==format(index(last(y)),format="%Y"),TRUE,FALSE) 
	switch(freq$scale,
	    seconds = { date.format = "%H:%M"},
	    minute = { date.format = "%H:%M"},
	    hourly = {date.format = "%d %H"},
	    daily = {if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"},
	    weekly = {if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"},
	    monthly = {if (yr_eq) date.format = "%b" else date.format = "%b %y"},
	    quarterly = {if (yr_eq) date.format = "%b" else date.format = "%b %y"},
	    yearly = {date.format = "%Y"}
	)
    }
    # Needed for finding aligned dates for event lines and period areas
    rownames = as.Date(time(y))
    rownames = format(strptime(rownames,format = date.format.in), date.format)

    time.scale = periodicity(y)$scale
    ep = axTicksByTime(y,major.ticks, format.labels = date.format)

    # If the Y-axis is ln
    logaxis = ""
    if(ylog) {
        logaxis = "y"
    }

    plot.new()

    if(is.null(xlim[1])) # is.na or is.null?
        xlim = c(1,rows)
    if(is.null(ylim[1])){
        ylim = as.numeric(range(y, na.rm=TRUE))
    }
    plot.window(xlim, ylim, xaxs = "r", log = logaxis)

    # par("usr"): A vector of the form 'c(x1, x2, y1, y2)' giving the extremes
    #           of the user coordinates of the plotting region.  When a
    #           logarithmic scale is in use (i.e., 'par("xlog")' is true, see
    #           below), then the x-limits will be '10 ^ par("usr")[1:2]'.
    #           Similarly for the y-axis.

    if(is.null(ylab)) {
        if(ylog) 
            ylab = "ln(Value)"

        else 
            ylab = "Value"
    }

    if(ylog)
        dimensions=10^par("usr")
    else
        dimensions = par("usr")

    # Draw any areas in the background
    if(!is.null(period.areas)) {
        # build a list of specific dates to find from xts ranges given
        period.dat = lapply(period.areas,function(x,y) c(first(index(y[x])),last(index(y[x]))),y=y)
        period.ind = NULL
        for(period in 1:length(period.dat)){
          if(!is.na(period.dat[[period]][1])){ 
            period.ind = list(grep(period.dat[[period]][1], index(y)), grep(period.dat[[period]][2], index(y)))
            rect(period.ind[1], dimensions[3], period.ind[2], dimensions[4], col = period.color, border=NA)
          }
        }
    }

    # Draw the grid
    if(auto.grid) {
        abline(v=ep, col=grid.color, lty=grid.lty)
        grid(NA, NULL, col = grid.color)
    }

    # Draw a solid reference line at zero
    abline(h = 0, col = element.color)

    # Add event.lines before drawing the data
    # This only labels the dates it finds
    if(!is.null(event.lines)) {
        event.ind = NULL
        for(event in 1:length(event.lines)){
            event.ind = c(event.ind, grep(event.lines[event], rownames))
        }
        number.event.labels = ((length(event.labels)-length(event.ind) + 1):length(event.labels))

        abline(v = event.ind, col = event.color, lty = 2)
        if(!is.null(event.labels)) {
            text(x=event.ind,y=ylim[2], label = event.labels[number.event.labels], offset = .2, pos = 2, cex = cex.labels, srt=90, col = event.color)
        }
    }

    # Expand the attributes to #columns if fewer values are passed in
    # (e.g., only one), to allow the user to pass in line, type, or
    # symbol variations.
    if(length(lwd) < columns)
        lwd = rep(lwd,columns)
    if(length(lty) < columns)
        lty = rep(lty,columns)
    if(length(pch) < columns)
        pch = rep(pch,columns)

    for(column in columns:1) {
        lines(1:rows, y[,column], col = colorset[column], lwd = lwd[column], pch = pch[column], lty = lty[column], type = type, ...)
    }

    if (xaxis) {
        if(minor.ticks)
            axis(1, at=1:NROW(y), labels=FALSE, col='#BBBBBB', las=las)
        label.height = cex.axis *(.5 + apply(t(names(ep)),1, function(X) max(strheight(X, units="in")/par('cin')[2]) ))
        if(is.null(xaxis.labels))
            xaxis.labels = names(ep)
        else
            ep = 1:length(xaxis.labels)
        axis(1, at=ep, labels=xaxis.labels, las=1, lwd=1, mgp=c(3,label.height,0), cex.axis = cex.axis, las=las) 
#         axis(1, at=ep, labels=xaxis.labels, las=1, lwd=1, mgp=c(3,2,0), cex.axis = cex.axis) 
        #axis(1, at = lab.ind, lab=rownames[lab.ind], cex.axis = cex.axis, col = elementcolor)
        title(xlab = xlab, cex = cex.lab)
        # use axis(..., las=3) for vertical labels.
    }

    # set up y-axis
    if (yaxis)
        if(yaxis.right)
            axis(4, cex.axis = cex.axis, col=element.color, ylog=ylog, las=las)
        else
            axis(2, cex.axis = cex.axis, col=element.color, ylog=ylog, las=las)
    box(col = element.color)

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = cex.legend, border.col = element.color, lty = lty, lwd = 2, bg = "white", legend = columnnames, pch=pch)
    }

    # Add the other titles
    if(is.null(main))
        main=columnnames[1]
    title(ylab = ylab, cex.lab = cex.lab)
    title(main = main, cex.main = cex.main)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.TimeSeries.R 3579 2015-01-07 13:01:25Z braverock $
#
###############################################################################
