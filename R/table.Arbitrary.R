#' wrapper function for combining arbitrary function list into a table
#' 
#' This function creates a table of statistics from vectors of functions and
#' labels passed in.  The resulting table is formatted such that metrics are
#' calculated separately for each column of returns in the data object.
#' 
#' Assumes an input of period returns.  Scale arguements can be used to specify
#' the number of observations during a year (e.g., 12 = monthly returns).
#' 
#' The idea here is to be able to pass in sets of metrics and values, like:
#' 
#' metrics = c(DownsideDeviation(x,MAR=mean(x)), sd(subset(x,x>0)),
#' sd(subset(x,x<0)), DownsideDeviation(x,MAR=MAR),
#' DownsideDeviation(x,MAR=Rf=0), DownsideDeviation(x,MAR=0),maxDrawdown(x))
#' 
#' metricsNames = c("Semi Deviation", "Gain Deviation", "Loss Deviation",
#' paste("Downside Deviation (MAR=",MAR*scale*100,"%)", sep=""),
#' paste("Downside Deviation (rf=",rf*scale*100,"%)", sep=""), paste("Downside
#' Deviation (0%)", sep=""), "Maximum Drawdown" )
#' 
#' Here's how it's working right now: >
#' table.Arbitrary(monthlyReturns.ts,metrics=c("VaR","mean"),
#' metricsNames=c("modVaR","mean"),p=.95) \preformatted{ Actual S&P500TR modVaR
#' 0.04186461 0.06261451 mean 0.00945000 0.01013684 }
#' 
#' Passing in two different sets of attributes to the same function doesn't
#' quite work currently.  The issue is apparent in: >
#' table.Arbitrary(edhec,metrics=c("VaR", "VaR"), metricsNames=c("Modified
#' VaR","Traditional VaR"), modified=c(TRUE,FALSE)) \preformatted{
#' Convertible.Arbitrage CTA.Global Distressed.Securities Modified VaR
#' 0.04081599 0.0456767 0.1074683 Traditional VaR 0.04081599 0.0456767
#' 0.1074683 Emerging.Markets Equity.Market.Neutral Event.Driven Modified VaR
#' 0.1858624 0.01680917 0.1162714 Traditional VaR 0.1858624 0.01680917
#' 0.1162714 Fixed.Income.Arbitrage Global.Macro Long.Short.Equity Modified VaR
#' 0.2380379 0.03700478 0.04661244 Traditional VaR 0.2380379 0.03700478
#' 0.04661244 Merger.Arbitrage Relative.Value Short.Selling Funds.of.Funds
#' Modified VaR 0.07510643 0.04123920 0.1071894 0.04525633 Traditional VaR
#' 0.07510643 0.04123920 0.1071894 0.04525633 }
#' 
#' In the case of this example, you would simply call VaR as the second
#' function, like so: > table.Arbitrary(edhec,metrics=c("VaR",
#' "VaR"),metricsNames=c("Modified VaR","Traditional VaR")) \preformatted{
#' Convertible.Arbitrage CTA.Global Distressed.Securities Modified VaR
#' 0.04081599 0.04567670 0.10746831 Traditional VaR 0.02635371 0.04913361
#' 0.03517855 Emerging.Markets Equity.Market.Neutral Event.Driven Modified VaR
#' 0.18586240 0.01680917 0.11627142 Traditional VaR 0.07057278 0.01746554
#' 0.03563019 Fixed.Income.Arbitrage Global.Macro Long.Short.Equity Modified
#' VaR 0.23803787 0.03700478 0.04661244 Traditional VaR 0.02231236 0.03692096
#' 0.04318713 Merger.Arbitrage Relative.Value Short.Selling Funds.of.Funds
#' Modified VaR 0.07510643 0.04123920 0.1071894 0.04525633 Traditional VaR
#' 0.02510709 0.02354012 0.0994635 0.03502065 } but we don't know of a way to
#' compare the same function side by side with different parameters for each.
#' Suggestions Welcome.
#' 
#' @aliases table.Arbitrary statsTable
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param metrics lisdt of functions to apply
#' @param metricsNames column names for each function
#' @param \dots any other passthru parameters
#' @author Peter Carl
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' table.Arbitrary(edhec,metrics=c("VaR", "ES"),
#'                 metricsNames=c("Modified VaR","Modified Expected Shortfall"))
#' 
#' @export
table.Arbitrary <-
function (R, metrics=c("mean","sd"), metricsNames=c("Average Return","Standard Deviation"), ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function creates a table of statistics from vectors of functions and
    # labels passed in.  The resulting table is formatted such that metrics
    # are calculated separately for each column of returns in the data object.

    # Inputs:
    # Assumes an input of period returns.  Scale arguements can be used to
    # specify the number of observations during a year (e.g., 12 = monthly
    # returns).

    # Outputs:
    # A table with calculated metrics for each column

    # The idea here is to be able to pass in sets of metrics and values, like:

    # metrics = c(DownsideDeviation(x,MAR=mean(x)), sd.xts(subset(x,x>0)),
    # sd.xts(subset(x,x<0)), DownsideDeviation(x,MAR=MAR),
    # DownsideDeviation(x,MAR=Rf), DownsideDeviation(x,MAR=0),maxDrawdown(x))

    # metricsNames = c("Semi Deviation", "Gain Deviation", "Loss Deviation",
    # paste("Downside Deviation (MAR=",MAR*scale*100,"%)", sep=""),
    # paste("Downside Deviation (Rf=",Rf*scale*100,"%)", sep=""),
    # paste("Downside Deviation (0%)", sep=""), "Maximum Drawdown" )

    # Here's how it's working right now:
    # > statsTable(monthlyReturns.ts,metrics=c("VaR.CornishFisher","mean"),
    # metricsNames=c("modVaR","mean"),p=.95)
    #            Actual   S&P500TR
    # modVaR 0.04186461 0.06261451
    # mean   0.00945000 0.01013684

    # Passing in attributes doesn't quite work.  The issue is apparent in:
    # > statsTable(monthlyReturns.ts,metrics=c("VaR.CornishFisher", "VaR.CornishFisher"),
    #   metricsNames=c("Modified VaR","Traditional VaR"), modified=c(TRUE,FALSE))
    #                     Actual   S&P500TR
    # Modified VaR    0.06340849 0.09334976
    # Traditional VaR 0.06340849 0.09334976
    # Warning messages:
    # 1: the condition has length > 1 and only the first element will be used in: if (modified) {
    # 2: the condition has length > 1 and only the first element will be used in: if (modified) {
    # 3: the condition has length > 1 and only the first element will be used in: if (modified) {
    # 4: the condition has length > 1 and only the first element will be used in: if (modified) {

    # FUNCTION:

    y = checkData(R, method = "zoo")

    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)

    # for each column, do the following:
    for(column in 1:columns) {
        x = as.matrix(y[,column])

        # apply the calculations
        values = vector('numeric', 0)

        for(metric in metrics) {
            newvalue = apply(x, MARGIN = 2, FUN = metric, ...)
            values = c(values,newvalue)
        }

        if(column == 1) {
            resultingtable = data.frame(Value = values, row.names = metricsNames)
        }
        else {
            nextcolumn = data.frame(Value = values, row.names = metricsNames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    resultingtable

}
###############################################################################

statsTable <-
function(R, metrics=c("mean","sd"), metricsNames=c("Average Return","Standard Deviation"), ...)
{   # @author Peter Carl

    # Description:

    # This is a wrapper function to keep backwards compatability

    # FUNCTION:
    table.Arbitrary(R, metrics=metrics, metricsNames=metricsNames, ...)

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
