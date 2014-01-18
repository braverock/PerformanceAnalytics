#'@title  GLM Index 
#'@description
#'Getmansky Lo Markov Smoothing Index is a useful summary statistic for measuring the concentration of weights is
#' a sum of square of Moving Average lag coefficient.
#' This measure is well known in the industrial organization literature as the 
#' \bold{ Herfindahl index}, a measure of the concentration of firms in a given industry. 
#' The index is maximized when one coefficient is 1 and the rest are 0. In the context of
#'smoothed returns, a lower value implies more smoothing, and the upper bound
#'of 1 implies no smoothing,  hence \eqn{\xi} is reffered as a '\bold{smoothingindex}'.
#'\deqn{ \xi =   \sum\theta(j)^2}
#'Where j belongs to 0 to k,which is the number of lag factors input.
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param ... Additional Parameters
#' @author Peter Carl, Brian Peterson, Shubhankit Mohan
#' @aliases Return.Geltner
#' @references \emph{Getmansky, Mila, Lo, Andrew W. and Makarov, Igor} An Econometric Model of Serial Correlation and Illiquidity in Hedge Fund Returns (March 1, 2003). MIT Sloan Working Paper No. 4288-03; MIT Laboratory for Financial Engineering Working Paper No. LFE-1041A-03; EFMA 2003 Helsinki Meetings. Paper available at SSRN: \url{http://ssrn.com/abstract=384700}
#' 
#' @keywords ts multivariate distribution models non-iid 
#' @examples
#'     require(PerformanceAnalytics)
#'  library(PerformanceAnalytics)
#'  data(edhec)
#' GLMSmoothIndex(edhec)
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
    edhec=NULL
  }

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: GLMSmoothIndex.R 2163 2012-07-16 00:30:19Z braverock $
#
###############################################################################
