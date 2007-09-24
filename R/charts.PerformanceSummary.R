`charts.PerformanceSummary` <-
function (R, rf = 0, main = NULL, method = c("ModifiedVaR","VaR","StdDev"), width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, gap = 12, begin=c("first","axis"), legend.loc="topleft", ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a wealth index chart, bars for monthly performance,
    # and underwater chart for drawdown.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest, the next
    #   columns are assumed to be relevant benchmarks for comparison.
    # rf: this is the risk free rate.  Remember to set this to the same
    #   periodicity as the data being passed in.
    # method: Used to select the risk parameter to use in the chart.BarVaR.  May
    #   be any of:
    #     modVaR - uses CF modified VaR
    #     VaR - uses traditional Value at Risk
    #     StdDev - monthly standard deviation of trailing 12 month returns
    #

    # Outputs:
    # A stack of three related timeseries line charts

    # FUNCTION:
    begin = begin[1]
    x = checkData(R, method = "zoo")
    colnames = colnames(x)
    ncols = ncol(x)

# This repeats a bit of code from chart.CumReturns, but it's intended
# to align the start dates of all three charts.  Basically, it assumes
# that the first column in the list is the column of interest, and 
# starts everything from that start date

    length.column.one = length(x[,1])
# find the row number of the last NA in the first column
    start.row = 1
    start.index = 0
    while(is.na(x[start.row,1])){
        start.row = start.row + 1
    }
    x = x[start.row:length.column.one,]

    if(ncols > 1)
        legend.loc = legend.loc
    else
        legend.loc = NULL

    if(is.null(main))
        main = paste(colnames[1],"Performance", sep=" ")

    if(ylog)
        wealth.index = TRUE

    # First, we lay out the graphic as a three row, one column format
#    plot.new()
    layout(matrix(c(1,2,3)),height=c(2,1,1.3),width=1)
    # to see the resulting layout, use layout.show(3)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first row is the cumulative returns line plot
    par(mar=c(1,4,4,2))
    chart.CumReturns(x, main = main, xaxis = FALSE, ylab = NULL, legend.loc = legend.loc, event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, begin = begin, ...)

    # The second row is the monthly returns bar plot
    par(mar=c(1,4,0,2))
#    chart.BarVaR(as.matrix(R[,1]), main = "", xaxis = FALSE, ylab = "Monthly Return", method = method)
    chart.BarVaR(x, main = "", xaxis = FALSE, width = width, ylab = "Monthly Return", method = method, event.labels = NULL, ylog=FALSE, gap = gap, ...)

    # The third row is the underwater plot
    par(mar=c(5,4,0,2))
    chart.Drawdown(x, main = "", ylab = "From Peak", event.labels = NULL, ylog=FALSE, ...)

    # If we wanted to add a fourth row with the table of monthly returns
    # Unfortunately, the textplot function doesn't provide a lot of control over
    # formatting.  Also, it requires the gplots package.
    #par(mar=c(0,0,0,0))
    #textplot(table.Returns(as.matrix(R)),cex=.7,cmar=1.5,rmar=0.5,halign="center", valign="center")
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: charts.PerformanceSummary.R,v 1.15 2007-09-24 02:50:28 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.14  2007/08/23 02:12:04  peter
# - added legend.loc as parameter so that legend can be shut off or moved
# in top chart
#
# Revision 1.13  2007/08/15 00:04:44  peter
# - aligns the three charts along the start date of the first column of
# data
#
# Revision 1.12  2007/06/29 15:52:25  peter
# - removed plot.new() that was causing two page pdf files
#
# Revision 1.11  2007/06/18 03:35:22  brian
# - make method argument a list
#
# Revision 1.10  2007/04/30 12:56:05  peter
# - changed 'method' to 'begin'
#
# Revision 1.9  2007/04/09 12:31:27  brian
# - syntax and usage changes to pass R CMD check
#
# Revision 1.8  2007/04/04 02:46:34  peter
# - added gap parameter for chart.BarVaR
#
# Revision 1.7  2007/03/22 13:48:11  peter
# - removed yaxis label in favor of default
#
# Revision 1.6  2007/03/21 21:46:54  peter
# - passing in wealth.index to top chart
#
# Revision 1.5  2007/03/21 21:44:21  peter
# - fixed conditional test
#
# Revision 1.4  2007/03/21 21:40:48  peter
# - added error handling for ylog passing in top chart
#
# Revision 1.3  2007/03/20 13:48:07  peter
# - changed "n" attribute to "width" in chart.BarVaR call
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################
