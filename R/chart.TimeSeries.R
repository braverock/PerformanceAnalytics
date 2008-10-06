`chart.TimeSeries` <-
function (R, reference.grid = TRUE, xaxis = TRUE, yaxis = TRUE, yaxis.right = FALSE, type = "l", lty = 1, lwd = 1, main = NULL, ylab=NULL, xlab="Date", date.format.in="%Y-%m-%d", date.format = "%m/%y", xlim = NULL, ylim = NULL, event.lines = NULL, event.labels = NULL, period.areas = NULL, event.color = "darkgray", period.color = "lightgray", colorset = (1:12), pch = (1:12), darken = FALSE , legend.loc = NULL, ylog = FALSE, cex.axis=0.8, cex.legend = 0.8, cex.labels = 0.8, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # Draws a line chart and labels the x-axis with the appropriate dates.
    # This is really a "primitive", since it constructs the elements of a plot
    # to provide lines for each column of data provided.  This function is
    # intended to be used in a wrapper that is written for a particular purpose.
    # This is just a handy way to standardize the formatting of multiple charts.

    # Inputs:
    # R = assumes that data is a regular time series, not irregular.  Can take
    # any type of object, whether a matrix, data frame, or timeSeries.
    # date.format: allows passing of a date format for the xaxis
    # legend.loc = use this to locate the legend, e.g., "topright"
    # colorset = use the name of any of the palattes above
    # reference.grid = if true, draws a grid aligned with the points on the
    #    x and y axes.
    # darken = if true, draws the chart elements in "darkgray" rather than
    #    "gray".  Makes it easier to print for some printers.
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

    # Make sure that we have a matrix to work with
    y = checkData(R, method = "zoo")

    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    #rownames = rownames(y)
    rownames = as.Date(time(y))

    # Re-format the dates for the xaxis
#     rownames = format(strptime(as.Date(rownames),format = date.format.in), date.format)
    rownames = format(strptime(rownames,format = date.format.in), date.format)

    # If the Y-axis is ln
    logaxis = ""
    if(ylog) {
        logaxis = "y"
    }

    # Set color for key elements, easy to darken for the printer
    if(darken)
        elementcolor = "darkgray" #better for the printer
    else
        elementcolor = "lightgray" #better for the screen

    plot.new()

    if(is.null(xlim[1])) # is.na or is.null?
        xlim = c(1,rows)
    if(is.null(ylim[1])){
        ylim = range(y, na.rm=TRUE)
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
        period.ind = NULL
        for(period in 1:length(period.areas)){
            period.ind = list(grep(period.areas[[period]][1], rownames), grep(period.areas[[period]][2], rownames))
            rect(period.ind[1], dimensions[3], period.ind[2], dimensions[4], col = period.color, border=NA)
        }
    }

    # The default label and grid placement is ok, but not great.  We set up
    # indexes for each to improve placement.
    # @todo: measuring the length of data set and setting sensible ticks needs improvement

    if(xlim[2]>=200)
        tickspace=24
    if(xlim[2]>=100)
        tickspace=12
    if(xlim[2]>=50)
        tickspace=6
    else
        tickspace=4

    lab.ind = seq(1, rows, by = tickspace/2)
    grid.ind = seq(1, rows, by = tickspace)
    # lab.ind = seq(1,rows,length=rows/divisor)

    # Draw the grid
    if (reference.grid) {
        grid(nx = NA, ny = NULL ,col = elementcolor)
        #grid(col="darkgray")
        abline(v=grid.ind, col = elementcolor, lty = "dotted")
    }

    # Draw a solid reference line at zero
    abline(h = 0, col = elementcolor)

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
        axis(1, at = lab.ind, lab=rownames[lab.ind], cex.axis = cex.axis, col = elementcolor)
        title(xlab = xlab)
        # use axis(..., las=3) for vertical labels.
    }

    # set up y-axis
    if (yaxis)
        if(yaxis.right)
            axis(4, cex.axis = cex.axis, col=elementcolor, ylog=ylog)
        else
            axis(2, cex.axis = cex.axis, col=elementcolor, ylog=ylog)
    box(col = elementcolor)

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = cex.legend, border.col = elementcolor, lty = lty, lwd = 2, bg = "white", legend = columnnames)
    }

    # Add the other titles
    if(is.null(main))
        main=columnnames[1]
    title(ylab = ylab)
    title(main = main)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.TimeSeries.R,v 1.13 2008-10-06 19:08:50 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.12  2008-08-16 03:42:26  peter
# - added yaxis.right parameter
#
# Revision 1.11  2008-06-28 13:55:25  peter
# - added cex.labels attribute
#
# Revision 1.10  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.9  2008/02/15 04:21:20  peter
# - added parameters for legend management
#
# Revision 1.8  2007/11/19 03:42:14  peter
# - title will no longer be set to default text, will use column name instead
#
# Revision 1.7  2007/11/08 05:04:10  peter
# - added support for other time formats
#
# Revision 1.6  2007/08/20 21:06:11  peter
# - using range function's na.rm flag to get correct NA behavior
#
# Revision 1.5  2007/08/14 23:43:50  peter
# - now uses zoo internally and handles yearmon and yearqtr formatting
#
# Revision 1.4  2007/03/13 04:01:40  peter
# - added new checkData function
#
# Revision 1.3  2007/03/09 03:08:48  peter
# - fixed y-axis so that ylog could be passed in as a parameter
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################