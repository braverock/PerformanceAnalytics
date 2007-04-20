`chart.CumReturns` <-
function (R, wealth.index = FALSE, legend.loc = NULL, colorset = (1:12), method = c("axis", "first"), ...)
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
    method = method[1]
    x = checkData(R, method = "zoo")
    
    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)

    # Calculate the cumulative return
    one = 0
    if(!wealth.index)
        one = 1

    ##find the longest column, calc cum returns and use it for starting values

    if(method == "first") {
        length.column.one = length(x[,1])
    # find the row number of the last NA in the first column
        start.row = 1
        start.index = 0
        while(is.na(x[start.row,1])){
            start.row = start.row + 1
        }
        x = x[start.row:length.column.one,]

        reference.index = cumprod(1+na.omit(x[,1]))
    }
    for(column in 1:columns) {
        if(method == "axis")
            start.index = 1
        else {
    # find the row number of the last NA in the target column
            start.row = 1
            start.index = 0
            while(is.na(x[start.row,column])){
                start.row = start.row + 1
            }
            if(start.row == 1){
                start.index = 0
            }
            else {
                start.index = reference.index[(start.row-1)]
            }
        }
        z=zoo(0)
        if(start.index > 1){
            z = rbind(start.index,1+na.omit(x[,column]))
        }
        else
            z = 1+na.omit(x[,column])
        column.Return.cumulative = (cumprod(z) - one)
        if(column == 1)
            Return.cumulative = column.Return.cumulative
        else
            Return.cumulative = merge(Return.cumulative,column.Return.cumulative)
    }
    if(columns == 1) 
        Return.cumulative = as.matrix(Return.cumulative)
    colnames(Return.cumulative) = columnnames

    # Chart the cumulative returns series
    chart.TimeSeries(Return.cumulative, col = colorset, legend.loc = legend.loc, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.CumReturns.R,v 1.5 2007-04-20 13:47:53 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
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