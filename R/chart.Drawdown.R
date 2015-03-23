#' Time series chart of drawdowns through time
#' 
#' A time series chart demonstrating drawdowns from peak equity attained
#' through time, calculated from periodic returns.
#' 
#' Any time the cumulative returns dips below the maximum cumulative returns,
#' it's a drawdown.  Drawdowns are measured as a percentage of that maximum
#' cumulative return, in effect, measured from peak equity.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param colorset color palette to use, set by default to rational choices
#' @param legend.loc places a legend into one of nine locations on the chart:
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#' center.
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso
#' \code{\link{plot}} \cr 
#' \code{\link{chart.TimeSeries}} \cr
#' \code{\link{findDrawdowns}} \cr 
#' \code{\link{sortDrawdowns}} \cr
#' \code{\link{maxDrawdown}} \cr 
#' \code{\link{table.Drawdowns}} \cr
#' \code{\link{table.DownsideRisk}}
###keywords ts
#' @examples
#' 
#' data(edhec)
#' chart.Drawdown(edhec[,c(1,2)], 
#' 		main="Drawdown from Peak Equity Attained", 
#' 		legend.loc="bottomleft")
#' @aliases Drawdowns
#' @export 
chart.Drawdown <-
function (R, geometric = TRUE, legend.loc = NULL, colorset = (1:12), ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart demonstrating drawdowns from peak equity
    # attained through time.

    # To find the maximum drawdown in a return series, we need to first
    # calculate the cumulative returns and the maximum cumulative return to
    # that point.  Any time the cumulative returns dips below the maximum
    # cumulative returns, it's a drawdown.  Drawdowns are measured as a
    # percentage of that maximum cumulative return, in effect, measured from
    # peak equity.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # legend.loc: use this to locate the legend, e.g., "topleft".  If it's
    #   left as NULL, then no legend is drawn.
    # colorset: use the name of any of the palattes above.

    # Outputs:
    # A timeseries line chart of the drawdown series

    # FUNCTION:

    # Calculate drawdown level
    drawdown = Drawdowns(R, geometric)

    # workaround provided by Samuel Le to handle single-column input
    if(NCOL(R)==1)
    {
        drawdown<-as.xts(drawdown)
        colnames(drawdown)<-colnames(R)
    }
    
    # Chart the drawdown level
    chart.TimeSeries(drawdown, colorset = colorset, legend.loc = legend.loc, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
