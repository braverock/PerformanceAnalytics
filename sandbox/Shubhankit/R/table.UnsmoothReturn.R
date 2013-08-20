#' @title Compenent Decomposition of Table of Unsmooth Returns
#' 
#' @description Creates a table of estimates of moving averages for comparison across
#' multiple instruments or funds as well as their standard error and
#' smoothing index
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param ci confidence interval, defaults to 95\%
#' @param n number of series lags
#' @param p confidence level for calculation, default p=.99
#' @param digits number of digits to round results to
#' @author Peter Carl, Brian Peterson, Shubhankit Mohan
#' @keywords ts smooth return models
#' @rdname table.UnsmoothReturn
#' @export 
table.UnsmoothReturn <-
  function (R, n = 3, p= 0.95, digits = 4)
  {# @author 
    
    # DESCRIPTION:
    # Downside Risk Summary: Statistics and Stylized Facts
    
    # Inputs:
    # R: a regular timeseries of returns (rather than prices)
    # n : Number of lags
    # p = Confifence Level
    # Output:
    # A table of estimates of Moving Average
    
    y = checkData(R, method = "xts")
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)
    
    # for each column, do the following:
    for(column in 1:columns) {
      x = y[,column]
      
      z = c(arma(x,0,2)$theta[1],
        arma(x,0,2)$se.theta[1],
        arma(x,0,2)$theta[2],
        arma(x,0,2)$se.theta[2],
            arma(x,0,2)$se.theta[2])
      znames = c(
        "Moving Average(1)",
        "Std Error of MA(1)",
        "Moving Average(2)",
        "Std Error of MA(2)",
        "Smoothing Invest"
        
      )
      if(column == 1) {
        resultingtable = data.frame(Value = z, row.names = znames)
      }
      else {
        nextcolumn = data.frame(Value = z, row.names = znames)
        resultingtable = cbind(resultingtable, nextcolumn)
      }
    }
    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans

    
}

###############################################################################
# R (http://r-project.org/) 
#
# Copyright (c) 2004-2013 
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.UnSmoothReturn.R 
#
###############################################################################
