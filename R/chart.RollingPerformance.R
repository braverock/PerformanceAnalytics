`chart.RollingPerformance` <-
function (R, width = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), FUN = "Return.annualized", na.pad = TRUE, type = "l", pch = NULL, lty = 1, bg = NULL, cex.axis=0.8, cex.legend = 0.8, cex.labels = 0.7, lwd = 2, xlim = NULL, ylim = NULL, log = "", main=NULL, sub = NULL, xlab = "Date", ylab = NULL, ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL, panel.last = NULL, asp = NA, ylog = FALSE, event.lines = NULL, event.labels = NULL, period.areas = NULL, event.color = "darkgray", period.color = "lightgray", element.color = "darkgray", major.ticks='auto', minor.ticks=TRUE, grid.color="lightgray", grid.lty="dotted", ...)
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

    # Calculate
#     Return.calc = xts:::rollapply.xts(x, width = width, FUN = function(x, FUNCT=FUNCT, ... = ...) { if(class(na.action(na.omit(x)))=="omit") return(NA) else {FUN <- match.fun(FUNCT); FUN}}, FUNCT=FUNCT, ...=..., na.pad = na.pad, align = "right")
#     print(Return.calc)
    for(column in 1:columns) {
        # the drop=FALSE flag is essential for when the zoo object only has one column
        column.Return.calc = xts:::rollapply.xts(na.omit(x[,column,drop=FALSE]), width = width, FUN = FUN, ..., na.pad = na.pad, align = "right")
        if(column == 1)
            Return.calc = xts(column.Return.calc)
        else
            Return.calc = merge(Return.calc,column.Return.calc)
    }

    ylim = c(min(0,min(Return.calc, na.rm=TRUE)),max(Return.calc, na.rm=TRUE))
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

        main = paste(columnnames[1], " Rolling ",width,"-",freq.lab," Performance",sep="")
    }

    chart.TimeSeries(Return.calc, xaxis = xaxis, colorset = colorset, legend.loc = legend.loc, type = type, pch = pch, lty = lty, bg = bg, cex.axis=cex.axis, cex.legend = cex.legend, cex.labels = cex.labels, lwd = lwd, xlim = xlim, ylim = ylim, main = main, sub = sub, xlab = xlab, ylab = ylab, ann = ann, panel.first = panel.first, panel.last = panel.last, asp = asp, ylog = ylog, event.lines = event.lines, event.labels = event.labels, period.areas = period.areas, event.color = event.color, period.color = period.color, element.color = element.color, major.ticks=major.ticks, minor.ticks=minor.ticks, grid.color=grid.color, grid.lty=grid.lty  )

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.16  2009-10-10 12:40:08  brian
# - update copyright to 2004-2009
#
# Revision 1.15  2009-10-08 19:47:02  peter
# - added the new xts rollapply function
#
# Revision 1.14  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.13  2009-10-02 19:04:48  peter
# - uses apply.rolling rather than rollapply
#
# Revision 1.12  2009-09-17 03:02:38  peter
# - added new attributes
#
# Revision 1.11  2009-03-20 03:22:53  peter
# - added xts
#
# Revision 1.10  2008-06-23 02:41:35  peter
# - added ylimit to include zero
#
# Revision 1.9  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.8  2008-04-18 03:47:34  peter
# - added cex attributes for formatting
#
# Revision 1.7  2007/03/15 01:15:03  brian
# - replace drop=F with drop=FALSE for R CMD check compatibility
#
# Revision 1.6  2007/03/14 00:54:06  brian
# - updates to parameters for standardization
#
# Revision 1.5  2007/03/13 18:08:20  peter
# - now handles single column zoo objects when naming columns using drop=FALSE
#
# Revision 1.4  2007/03/13 04:21:55  peter
# - adjusted parameter inputs
#
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
