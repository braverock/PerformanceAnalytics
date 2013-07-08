#' Compenent Decomposition of Table of Unsmooth Returns for GLM Model
#' 
#' Creates a table of comparitive changes in Normality Properties for Third
#' and Fourth Moment Vectors i.e. Skewness and Kurtosis for Orignal and Unsmooth 
#' Returns Respectively
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param ci confidence interval, defaults to 95\%
#' @param n number of series lags
#' @param digits number of digits to round results to
#' @author R
#' @keywords ts unsmooth GLM return models
#'
#' @export 
table.ComparitiveReturn.GLM <-
  function (R, n = 3, digits = 4)
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
      skew = skewness(x)
      arma.coeff= arma(x,0,n)
      kurt= kurtosis(x)
      z = c(skew,
            ((sum(arma.coeff$theta^2)^1.5)*(skew/(sum(arma.coeff$theta^3)))),
            kurt,
             (kurt*(sum(arma.coeff$theta^2)^2)/(sum(arma.coeff$theta^4))))
      znames = c(
        "Skewness ( Orignal) ",
        "Skewness (Unsmooth)",
        "Kurtosis (Orignal)",
        "Kurtosis (Unsmooth)")
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
# $Id: table.ComparitiveReturn.GLM
#
###############################################################################
