`charts.RollingPerformance` <-
function (R, width = 12, Rf = 0, main = NULL, trim = TRUE, event.labels = NULL, legend.loc=NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a rolling annualized returns chart, rolling annualized
    # standard deviation chart, and a rolling annualized sharpe ratio chart.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest, the next
    #   columns are assumed to be relevant benchmarks for comparison.
    # Rf: this is the risk free rate.  Remember to set this to the same
    #   periodicity as the data being passed in.
    #

    # Outputs:
    # A stack of three related timeseries line charts

    # FUNCTION:
    x = checkData(R)
    colnames = colnames(x)
    ncols = ncol(x)

    if(is.null(main))
        main = paste(colnames[1]," Rolling ",width,"-Month Performance",sep="")

    op <- par(no.readonly=TRUE)

    # First, we lay out the graphic as a three row, one column format
#    plot.new()
    layout(matrix(c(1,2,3)),height=c(1,0.75,1),width=1)
    # to see the resulting layout, use layout.show(3)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    par(mar=c(1,4,4,2))

    # The first row is the annualized returns
    chart.RollingPerformance(R, width = width, main = main, xaxis = FALSE, ylab = "Annualized Return", FUN = "Return.annualized", legend.loc = legend.loc, event.labels = event.labels, ...)

    # The second row is the annualized standard deviation
    par(mar=c(1,4,0,2))
    chart.RollingPerformance(R, width = width, main = "", xaxis = FALSE, ylab = "Annualized Standard Deviation", FUN = "StdDev.annualized", event.labels= NULL, ...)

    # The third row is the annualized SR
    par(mar=c(5,4,0,2))
    chart.RollingPerformance(R, width = width, main = "", ylab = "Annualized Sharpe Ratio", Rf = Rf, FUN = "SharpeRatio.annualized", event.labels= NULL, ...)

    par(op)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: charts.RollingPerformance.R,v 1.10 2009-10-02 18:57:47 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.9  2009-03-20 03:22:53  peter
# - added xts
#
# Revision 1.8  2008-10-14 14:37:29  brian
# - convert from matrix or data.frame to zoo in checkData call
#
# Revision 1.7  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.6  2008-04-18 03:52:44  peter
# - added par to reset layout to default
#
# Revision 1.5  2007/10/03 02:44:46  peter
# - legend will be hidden by default
# - legend location can be passed in through legend.loc parameter
#
# Revision 1.4  2007/06/29 15:53:16  peter
# - removed plot.new() that was causing two page pdfs
#
# Revision 1.3  2007/03/13 04:23:04  peter
# - changed to checkData function
# - modified parameters to fit RollingPerformance changes
# - now takes unequal time periods
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################
