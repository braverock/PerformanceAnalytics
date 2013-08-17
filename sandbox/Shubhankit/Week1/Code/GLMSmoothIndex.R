#' @title Getmansky Lo Markov Smoothing Index Parameter
#'  @description
#' A useful summary statistic for measuring the concentration of weights is
#' a sum of square of Moving Average lag coefficient.
#' This measure is well known in the industrial organization literature as the 
#' Herfindahl index, a measure of the concentration of firms in a given industry. 
#' The index is maximized when one coefficient is 1 and the rest are 0, in which case x ? 1: In the context of
#'smoothed returns, a lower value of x implies more smoothing, and the upper bound
#'of 1 implies no smoothing,  hence x is reffered as a ''smoothingindex' '.
#' 
#' \deqn{ R_t  =    {\mu} + {\beta}{{\delta}}_t+ \xi_t}
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @author Peter Carl
#' @aliases Return.Geltner
#' @references "An econometric model of serial correlation and illiquidity in 
#' hedge fund returns" Mila Getmansky1, Andrew W. Lo*, Igor Makarov
#' 
#' @keywords ts multivariate distribution models non-iid 
#' @examples
#' 
#' data(edhec)
#' head(GLMSmoothIndex(edhec))
#' 
#' @export
GLMSmoothIndex<-
  function(R = NULL, ...)
  {
    columns = 1
    columnnames = NULL
    #Error handling if R is not NULL
    if(!is.null(R)){
      x = checkData(R)
      columns = ncol(x)
      n = nrow(x)
      count = q
        x=edhec
        columns = ncol(x)
        columnnames = colnames(x)
        
        # Calculate AutoCorrelation Coefficient
        for(column in 1:columns) { # for each asset passed in as R
          y = checkData(x[,column], method="vector", na.rm = TRUE)
          sum = sum(abs(acf(y,plot=FALSE,lag.max=6)[[1]][2:7]));
          acflag6 = acf(y,plot=FALSE,lag.max=6)[[1]][2:7]/sum;
          values = sum(acflag6*acflag6)
          
          if(column == 1) {
            result.df = data.frame(Value = values)
            colnames(result.df) = columnnames[column]
          }
          else {
            nextcol = data.frame(Value = values)
            colnames(nextcol) = columnnames[column]
            result.df = cbind(result.df, nextcol)
          }
        }
      rownames(result.df)= paste("GLM Smooth Index")
      
        return(result.df)
      
    }  
  }

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: GLMSmoothIndex.R 2163 2012-07-16 00:30:19Z braverock $
#
###############################################################################
