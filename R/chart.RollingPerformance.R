`chart.RollingPerformance` <-
function (R, width = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), FUN = "Return.annualized", na.pad = TRUE, type = "l", pch = NULL, lty = 1, bg = NULL, cex = 1, lwd = 1, xlim = NULL, ylim = NULL, log = "", main = paste(width,"-Month Rolling Performance", sep=""), sub = NULL, xlab = NULL, ylab = NULL, ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL, panel.last = NULL, asp = NA, ylog = FALSE, event.lines = NULL, event.labels = NULL, period.areas = NULL, event.color = "darkgray", period.color = "lightgray", darken = FALSE , date.format = "%m/%y", ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of rolling performance metrics in a line chart

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # FUN: any function that can be evaluated using a single set of returns
    #   (e.g., rolling beta won't work, but Return.annualizeds will)

    # Outputs:
    # A timeseries line chart of the calculated series

    # FUNCTION:

    # Transform input data to a matrix
    x = checkData(R, method = "zoo")

    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)

    # Calculate

    for(column in 1:columns) {
        column.Return.calc = rollapply(na.omit(x[,column]), width = width, FUN = FUN, ..., na.pad = na.pad, align = "right")
        if(column == 1)
            Return.calc = column.Return.calc
        else
            Return.calc = merge(Return.calc,column.Return.calc)
    }
    if(columns == 1) 
        Return.calc = as.matrix(Return.calc)
    colnames(Return.calc) = columnnames

    chart.TimeSeries(Return.calc, xaxis = xaxis, colorset = colorset, legend.loc = legend.loc, type = type, pch = pch, lty = lty, bg = bg, cex = cex, lwd = lwd, xlim = xlim, ylim = ylim, main = main, sub = sub, xlab = xlab, ylab = ylab, ann = ann, panel.first = panel.first, panel.last = panel.last, asp = asp, ylog = ylog, event.lines = event.lines, event.labels = event.labels, period.areas = period.areas, event.color = event.color, period.color = period.color, darken = darken, date.format = date.format )

# rollapply(data, width, FUN, ..., by = 1, ascending = TRUE, by.column = TRUE,
#       na.pad = FALSE, align = c("center", "left", "right"))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.RollingPerformance.R,v 1.4 2007-03-13 04:21:55 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/03/13 04:06:47  peter
# - modified to deal with unequal time periods
# - uses new checkData function
# - passes attributes to function rather than chart
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################