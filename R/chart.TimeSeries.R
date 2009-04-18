`chart.TimeSeries` <-
function (R, auto.grid=TRUE, xaxis = TRUE, yaxis = TRUE, yaxis.right = FALSE, type = "l", lty = 1, lwd = 2, main = NULL, ylab=NULL, xlab="Date", date.format.in="%Y-%m-%d", date.format = "%m/%y", xlim = NULL, ylim = NULL, element.color="darkgray", event.lines = NULL, event.labels = NULL, period.areas = NULL, event.color = "darkgray", period.color = "aliceblue", colorset = (1:12), pch = (1:12), legend.loc = NULL, ylog = FALSE, cex.axis=0.8, cex.legend = 0.8, cex.lab = 1, cex.labels = 0.8, cex.main = 1, major.ticks='auto', minor.ticks=TRUE, grid.color="lightgray", grid.lty="dotted", ...)
{ # @author Peter Carl

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
            axis(1, at=1:NROW(y), labels=FALSE, col='#BBBBBB')
        label.height = .25 + cex.axis * apply(t(names(ep)),1, function(X) max(strheight(X, units="in")/par('cin')[2]) )

        axis(1, at=ep, labels=names(ep), las=1, lwd=1, mgp=c(3,label.height,0), cex.axis = cex.axis) 
        #axis(1, at = lab.ind, lab=rownames[lab.ind], cex.axis = cex.axis, col = elementcolor)
        title(xlab = xlab, cex = cex.lab)
        # use axis(..., las=3) for vertical labels.
    }

    # set up y-axis
    if (yaxis)
        if(yaxis.right)
            axis(4, cex.axis = cex.axis, col=element.color, ylog=ylog)
        else
            axis(2, cex.axis = cex.axis, col=element.color, ylog=ylog)
    box(col = element.color)

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = cex.legend, border.col = element.color, lty = lty, lwd = 2, bg = "white", legend = columnnames)
    }

    # Add the other titles
    if(is.null(main))
        main=columnnames[1]
    title(ylab = ylab, cex = cex.lab)
    title(main = main, cex = cex.main)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.TimeSeries.R,v 1.19 2009-04-18 02:56:53 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.18  2009-04-17 04:11:25  peter
# - removed commented code
# - parameter cleanup
#
# Revision 1.17  2009-04-14 02:49:41  peter
# - restored date formatting needed for matching event lines and areas
#
# Revision 1.16  2009-04-07 22:30:52  peter
# - added cex.* attributes for sizing text elements
#
# Revision 1.15  2009-03-20 03:22:53  peter
# - added xts
#
# Revision 1.14  2009-03-04 05:14:05  peter
# - added axTicksByTime from xts for nice xaxis
#
# Revision 1.13  2008-10-06 19:08:50  peter
# - fixed so that it will plot backgrounds when ylog=T
#
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