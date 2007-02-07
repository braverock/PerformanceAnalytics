`statsTable` <-
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

    # metrics = c(DownsideDeviation(x,MAR=mean(x)), sd(subset(x,x>0)),
    # sd(subset(x,x<0)), DownsideDeviation(x,MAR=MAR),
    # DownsideDeviation(x,MAR=rf), DownsideDeviation(x,MAR=0),maxDrawdown(x))

    # metricsNames = c("Semi Deviation", "Gain Deviation", "Loss Deviation",
    # paste("Downside Deviation (MAR=",MAR*scale*100,"%)", sep=""),
    # paste("Downside Deviation (rf=",rf*scale*100,"%)", sep=""),
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

    y = checkDataMatrix(R)

    # for each column, do the following:
    for(column in 1:columns) {
#     x = as.vector(y[,column])
    x = as.matrix(y[,column])
    x.length = length(x)
    x = x[!is.na(x)]
    x.na = x.length - length(x)

        # apply the calculations
        values = vector('numeric', 0)
    for(metric in metrics) {
#        values = c(values,metric)
            # I'm not quite sure why this requires the as.matrix() coersion
            newvalue = apply(as.matrix(x), MARGIN = 2, FUN = metric, ...)
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
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.Arbitrary.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################