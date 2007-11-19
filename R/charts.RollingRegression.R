charts.RollingRegression = function (Ra, Rb, width = 12, rf = 0, darken = FALSE, main = NULL, legend.loc = NULL, event.labels=NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a panel of RollingRegression charts that demonstrates
    # how the attributes change through time.

    # Inputs:
    # Ra: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest, the next
    #   columns are assumed to be relevant benchmarks for comparison.
    # Rb: a matrix, data frame, or timeSeries that is a set of returns of the
    #   same scale and periodicity as R.
    # rf: the risk free rate.  Remember to set this to the same periodicity
    #   as the data being passed in.
    # attribute: Used to select the regression parameter to use in the chart  May
    #   be any of:
    #     Alpha - shows the y-intercept
    #     Beta - shows the slope of the regression line
    #     R-Squared - shows the fit of the regression to the data
    #

    # Outputs:
    # A stack of three related timeseries line charts

    # FUNCTION:

    columns.a = ncol(Ra)
    columns.b = ncol(Rb)

#     if(columns.a > 1 | columns.b > 1)
#         legend.loc = "topleft"
#     else
#         legend.loc = NULL

#    plot.new()

    layout(matrix(c(1,2,3)),height=c(1.3,1,1.3),width=1)

    par(mar=c(1,4,4,2))
    if(is.null(main)){
         main = paste("Rolling ",width,"-Month Regression",sep="")
    }

    chart.RollingRegression(Ra, Rb, width = width, rf = rf, darken = darken , attribute = "Alpha", xaxis = FALSE, main = main, ylab = "Alpha", legend.loc=legend.loc, event.labels = event.labels, ...)

    par(mar=c(1,4,0,2))

    chart.RollingRegression(Ra, Rb, width = width, rf = rf, darken = darken, attribute = "Beta", main = "", ylab = "Beta", xaxis = FALSE, event.labels = NULL, ...)

    par(mar=c(5,4,0,2))

    chart.RollingRegression(Ra, Rb, width = width, rf = rf, attribute = "R-Squared", darken = darken, main = "", ylab = "R-Squared", event.labels = NULL, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: charts.RollingRegression.R,v 1.13 2007-11-19 03:43:12 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.12  2007/10/03 02:44:10  peter
# - legend will be hidden by default
# - legend location can be passed in through legend.loc parameter
#
# Revision 1.11  2007/06/29 15:54:03  peter
# - removed plot.new() because it was causing two page pdfs
#
# Revision 1.10  2007/04/14 15:01:55  brian
# - standardize Ra as first argument for asset returns
#
# Revision 1.8  2007/03/17 00:42:41  brian
# - correct use of F instead of FALSE to pass R CMD check
#
# Revision 1.7  2007/03/16 14:00:21  peter
# - added cvs footer
#
# Revision 1.6 2007-03-15 22:21:38 peter
# - removed data checks
#
# Revision 1.5 2007-03-14 19:45:38 peter
# - made legend conditional on having more than one data series
#
# Revision 1.4 2007-03-14 00:02:48 peter
# - switched order of top two charts
# - added legend to top chart
# - made inputs consistent with lower level functions
#
# Revision 1.3 2007-03-04 12:38:19 brian
# - update function definition to agree with usage using enumerated argument
#
# Revision 1.2 2007-02-22 13:11:36 peter
# - fixed parameter passing and title
#
# Revision 1.1 2007-02-07 09:53:20 peter
# - added function to the package
###############################################################################
