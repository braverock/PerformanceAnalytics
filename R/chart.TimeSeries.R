#' Creates a time series chart with some extensions.
#' 
#' Draws a line chart and labels the x-axis with the appropriate dates.  This
#' is really a "primitive", since it extends the base \code{\link{plot}} and
#' standardizes the elements of a chart.  Adds attributes for shading areas of
#' the timeline or aligning vertical lines along the timeline. This function is
#' intended to be used inside other charting functions.
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param auto.grid if true, draws a grid aligned with the points on the x and
#' y axes
#' @param grid.color sets the color for the reference grid
#' @param grid.lty defines the line type for the grid
#' @param xaxis if true, draws the x axis
#' @param yaxis if true, draws the y axis
#' @param yaxis.right if true, draws the y axis on the right-hand side of the
#' plot
#' @param type set the chart type, same as in \code{\link{plot}}
#' @param lty set the line type, same as in \code{\link{plot}}
#' @param lwd set the line width, same as in \code{\link{plot}}
#' @param las set the axis label rotation, same as in \code{\link{plot}}
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param ylab set the y-axis label, same as in \code{\link{plot}}
#' @param xlab set the x-axis label, same as in \code{\link{plot}}
#' @param date.format re-format the dates for the xaxis; the default is "\%m/\%y"
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param event.lines If not null, vertical lines will be drawn to indicate
#' that an event happened during that time period.  \code{event.lines} should
#' be a list of dates (e.g., \code{c("09/03","05/06"))} formatted the same as
#' date.format.  This function matches the re-formatted row names (dates) with
#' the events.list, so to get a match the formatting needs to be correct.
#' @param event.labels if not null and event.lines is not null, this will apply
#' a list of text labels (e.g., \code{c("This Event", "That Event")} to the
#' vertical lines drawn.  See the example below.
#' @param period.areas these are shaded areas described by start and end dates
#' in a vector of xts date rangees, e.g.,
#' \code{c("1926-10::1927-11","1929-08::1933-03")} See the examples below.
#' @param event.color draws the event described in \code{event.labels} in the
#' color specified
#' @param period.color draws the shaded region described by \code{period.areas}
#' in the color specified
#' @param colorset color palette to use, set by default to rational choices
#' @param pch symbols to use, see also \code{\link{plot}}
#' @param element.color provides the color for drawing chart elements, such as
#' the box lines, axis lines, etc. Default is "darkgray"
#' @param legend.loc places a legend into one of nine locations on the chart:
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#' center.
#' @param ylog TRUE/FALSE set the y-axis to logarithmic scale, similar to
#' \code{\link{plot}}, default FALSE
#' @param date.format.in allows specification of other date formats in the data
#' object, defaults to "\%Y-\%m-\%d"
#' @param cex.axis The magnification to be used for axis annotation relative to
#' the current setting of 'cex', same as in \code{\link{plot}}.
#' @param cex.legend The magnification to be used for sizing the legend
#' relative to the current setting of 'cex'.
#' @param cex.labels The magnification to be used for event line labels
#' relative to the current setting of 'cex'.
#' @param cex.lab The magnification to be used for x- and y-axis labels
#' relative to the current setting of 'cex'.
#' @param cex.main The magnification to be used for the chart title relative to
#' the current setting of 'cex'.
#' @param major.ticks Should major tickmarks be drawn and labeled, default
#' 'auto'
#' @param minor.ticks Should minor tickmarks be drawn, default TRUE
#' @param xaxis.labels Allows for non-date labeling of date axes, default is
#' NULL
#' @param space default 0
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso \code{\link{plot}}, \code{\link{par}},
#' \code{\link[xts]{axTicksByTime}}
#' @keywords ts multivariate distribution models hplot
#' @examples
#' 
#' 
#' # These are start and end dates, formatted as xts ranges.
#' ## http://www.nber.org-cycles.html
#' cycles.dates<-c("1857-06/1858-12",
#'                 "1860-10/1861-06",
#'                 "1865-04/1867-12",
#'                 "1869-06/1870-12",
#'                 "1873-10/1879-03",
#'                 "1882-03/1885-05",
#'                 "1887-03/1888-04",
#'                 "1890-07/1891-05",
#'                 "1893-01/1894-06",
#'                 "1895-12/1897-06",
#'                 "1899-06/1900-12",
#'                 "1902-09/1904-08",
#'                 "1907-05/1908-06",
#'                 "1910-01/1912-01",
#'                 "1913-01/1914-12",
#'                 "1918-08/1919-03",
#'                 "1920-01/1921-07",
#'                 "1923-05/1924-07",
#'                 "1926-10/1927-11",
#'                 "1929-08/1933-03",
#'                 "1937-05/1938-06",
#'                 "1945-02/1945-10",
#'                 "1948-11/1949-10",
#'                 "1953-07/1954-05",
#'                 "1957-08/1958-04",
#'                 "1960-04/1961-02",
#'                 "1969-12/1970-11",
#'                 "1973-11/1975-03",
#'                 "1980-01/1980-07",
#'                 "1981-07/1982-11",
#'                 "1990-07/1991-03",
#'                 "2001-03/2001-11",
#'                 "2007-12/2009-06"
#'                 )
#' # Event lists - FOR BEST RESULTS, KEEP THESE DATES IN ORDER
#' risk.dates = c(
#'     "Oct 87",
#'     "Feb 94",
#'     "Jul 97",
#'     "Aug 98",
#'     "Oct 98",
#'     "Jul 00",
#'     "Sep 01")
#' risk.labels = c(
#'     "Black Monday",
#'     "Bond Crash",
#'     "Asian Crisis",
#'     "Russian Crisis",
#'     "LTCM",
#'     "Tech Bubble",
#'     "Sept 11")
#' data(edhec)
#' 
#' R=edhec[,"Funds of Funds",drop=FALSE]
#' Return.cumulative = cumprod(1+R) - 1
#' chart.TimeSeries(Return.cumulative)
#' chart.TimeSeries(Return.cumulative, colorset = "darkblue", legend.loc = "bottomright", period.areas = cycles.dates, period.color = "lightblue", event.lines = risk.dates, event.labels = risk.labels, event.color = "red", lwd = 2)
#' 
#' @export 
chart.TimeSeries <-
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
    rownames = as.Date(xts:::time.xts(y))
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
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
