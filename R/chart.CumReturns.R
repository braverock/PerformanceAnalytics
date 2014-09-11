#' Cumulates and graphs a set of periodic returns
#' 
#' Chart that cumulates the periodic returns given and draws a line graph of
#' the results as a "wealth index".
#' 
#' Cumulates the return series and displays either as a wealth index or as
#' cumulative returns.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param wealth.index if \code{wealth.index} is \code{TRUE}, shows the "value
#' of $1", starting the cumulation of returns at 1 rather than zero
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param legend.loc places a legend into one of nine locations on the chart:
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#' center.
#' @param colorset color palette to use, set by default to rational choices
#' @param begin Align shorter series to: \itemize{ \item first - prior value of
#' the first column given for the reference or longer series or, \item axis -
#' the initial value (1 or zero) of the axis.  }
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso \code{\link{chart.TimeSeries}} \cr \code{\link{plot}}
#' @references Bacon, Carl. \emph{Practical Portfolio Performance Measurement
#' and Attribution}. Wiley. 2004. \cr
###keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(edhec)
#' chart.CumReturns(edhec[,"Funds of Funds"],main="Cumulative Returns")
#' chart.CumReturns(edhec[,"Funds of Funds"],wealth.index=TRUE, main="Growth of $1")
#' data(managers)
#' chart.CumReturns(managers,main="Cumulative Returns",begin="first")
#' chart.CumReturns(managers,main="Cumulative Returns",begin="axis")
#' 
#' @export 
chart.CumReturns <-
function (R, wealth.index = FALSE, geometric = TRUE, legend.loc = NULL, colorset = (1:12), begin = c("first","axis"), ...)
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
        if(geometric)
            reference.index = na.skip(x[,1],FUN=function(x) {cumprod(1+x)})
        else
            reference.index = na.skip(x[,1],FUN=function(x) {cumsum(x)})
    }
    for(column in 1:columns) {
        if(begin == "axis") {
            start.index = FALSE
		} else {
    		# find the row number of the last NA in the target column
            start.row = 1
            while(is.na(x[start.row,column])){
                start.row = start.row + 1
            }
            start.index=ifelse(start.row > 1,TRUE,FALSE)
        }
        if(start.index){
	        # we need to "pin" the beginning of the shorter series to the (start date - 1 period) 
	        # value of the reference index while preserving NA's in the shorter series
            if(geometric)
                z = na.skip(x[,column],FUN = function(x,index=reference.index[(start.row - 1)]) {rbind(index,1+x)})
            else
                z = na.skip(x[,column],FUN = function(x,index=reference.index[(start.row - 1)]) {rbind(1+index,1+x)})
        } else {
            z = 1+x[,column] 
        }
        column.Return.cumulative = na.skip(z,FUN = function(x, one, geometric) {if(geometric) cumprod(x)-one else (1-one) + cumsum(x-1)},one=one, geometric=geometric)
        if(column == 1)
            Return.cumulative = column.Return.cumulative
        else
            Return.cumulative = merge(Return.cumulative,column.Return.cumulative)
    }
    if(columns == 1)
        Return.cumulative = as.xts(Return.cumulative)
    colnames(Return.cumulative) = columnnames

    # Chart the cumulative returns series
    chart.TimeSeries(Return.cumulative, colorset = colorset, legend.loc = legend.loc, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
