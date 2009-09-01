`chart.CumReturns` <-
function (R, wealth.index = FALSE, legend.loc = NULL, colorset = (1:12), begin = c("first","axis"), ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # Cumulates the returns given and draws a line graph of the results as
    # a cumulative return or a "wealth index".

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # wealth.index:  if true, shows the "value of $1", starting the cumulation
    #    of returns at 1 rather than zero
    # legend.loc: use this to locate the legend, e.g., "topright"
    # colorset: use the name of any of the palattes above
    # method: "none"

    # Outputs:
    # A timeseries line chart of the cumulative return series

    # FUNCTION:

    # Transform input data to a matrix
    begin = begin[1]
    x = checkData(R)

    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)

    # Calculate the cumulative return
    one = 0
    if(!wealth.index)
        one = 1

    ##find the longest column, calc cum returns and use it for starting values

    if(begin == "first") {
        length.column.one = length(x[,1])
    # find the row number of the last NA in the first column
        start.row = 1
        start.index = 0
        while(is.na(x[start.row,1])){
            start.row = start.row + 1
        }
        x = x[start.row:length.column.one,]

        reference.index = na.skip(x[,1],FUN=function(x) {cumprod(1+na.omit(x))})
    }
    for(column in 1:columns) {
        if(begin == "axis")
            start.index = TRUE
        else {
    # find the row number of the last NA in the target column
            start.row = 1
            while(is.na(x[start.row,column])){
                start.row = start.row + 1
            }
            start.index=ifelse(start.row > 1,TRUE,FALSE)
		
        }
	
        
        if(start.index){
            z = na.skip(x[,column],FUN = function(x,index=reference.index[(start.row - 1)]) {rbind(index,1+x)})
        }
        else{
            z = na.skip(x[,column],FUN = function(x) {1+x})
	}

        column.Return.cumulative = na.skip(z,FUN = function(x,one) {cumprod(x) - one},one=one)
        if(column == 1)
            Return.cumulative = column.Return.cumulative
        else
            Return.cumulative = merge(Return.cumulative,column.Return.cumulative)
    }
    if(columns == 1)
        Return.cumulative = as.xts(Return.cumulative)
    colnames(Return.cumulative) = columnnames

    # Chart the cumulative returns series
    chart.TimeSeries(Return.cumulative, col = colorset, legend.loc = legend.loc, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.CumReturns.R,v 1.12 2009-09-01 20:05:44 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.11  2009-08-31 21:20:20  brian
# - fix return accumulation after adding na.skip
#
# Revision 1.10  2009-08-31 20:51:27  brian
# - add new function na.skip to deal with non-contiguous NA's in data, may eventually go to xts
# - fix components of charts.PerformanceSummary to use na.skip
#
# Revision 1.9  2009-03-20 03:22:53  peter
# - added xts
#
# Revision 1.8  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.7  2007/06/18 03:34:33  brian
# - reverse default order of 'begin' argument to match charts.PerformanceSummary
#
# Revision 1.6  2007/04/25 20:06:28  peter
# - changed the 'method' tag to 'begin'
#
# Revision 1.5  2007/04/20 13:47:53  peter
# - added attribute 'method' with values of 'axis' and 'first' to indicate
#   where to attach shorter data lengths for comparison
#
# Revision 1.4  2007/03/13 03:59:26  peter
# - uses new checkData function
#
# Revision 1.3  2007/03/10 05:08:04  peter
# - revised function to take zoo objects and plot unequal time periods
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################