`charts.PerformanceSummary` <-
function (R, rf = 0, main = NULL, method = "ModifiedVaR", width = 0, event.labels = NULL, ylog = FALSE, wealth.index = F, ...)
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
    x = checkData(R, method = "zoo")
    colnames = colnames(x)
    ncols = ncol(x)

    if(ncols > 1)
        legend.loc = "topleft"
    else
        legend.loc = NULL

    if(is.null(main))
        main = paste(colnames[1]," Performance")

    if(ylog)
        wealth.index = TRUE

    # First, we lay out the graphic as a three row, one column format
    plot.new()
    layout(matrix(c(1,2,3)),height=c(2,1,1.3),width=1)
    # to see the resulting layout, use layout.show(3)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first row is the cumulative returns line plot
    par(mar=c(1,4,4,2))
    chart.CumReturns(x, main = main, xaxis = FALSE, ylab = NULL, legend.loc = legend.loc, event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, ...)

    # The second row is the monthly returns bar plot
    par(mar=c(1,4,0,2))
#    chart.BarVaR(as.matrix(R[,1]), main = "", xaxis = FALSE, ylab = "Monthly Return", method = method)
    chart.BarVaR(x, main = "", xaxis = FALSE, width = width, ylab = "Monthly Return", method = method, event.labels = NULL, ylog = FALSE,...)

    # The third row is the underwater plot
    par(mar=c(5,4,0,2))
    chart.Drawdown(x, main = "", ylab = "From Peak", event.labels = NULL, ylog = FALSE, ...)

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
# $Id: charts.PerformanceSummary.R,v 1.7 2007-03-22 13:48:11 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
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