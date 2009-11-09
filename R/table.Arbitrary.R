`table.Arbitrary` <-
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

`statsTable` <-
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
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.8  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.7  2008-10-14 14:37:29  brian
# - convert from matrix or data.frame to zoo in checkData call
#
# Revision 1.6  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.5  2007/12/27 20:59:35  peter
# - cleaned out commented lines
#
# Revision 1.3  2007/12/27 20:46:47  peter
# - fixed bug to calculate number of columns
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################