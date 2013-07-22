#'@title The Noiseless Stationary Transfer Function Model of Appriasal Smoothing
#' @description calculate Geltner liquidity-adjusted return series
#' David Geltner developed a method to remove estimating or liquidity bias in
#' real estate index returns.  It has since been applied with success to other
#' return series that show autocorrelation or illiquidity effects.
#' 
#' The theory is that by correcting for autocorrelation, you are uncovering a
#' "true" return from a series of observed returns that contain illiquidity or
#' manual pricing effects.
#' 
#' The Geltner autocorrelation adjusted return series may be calculated via:
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param q number of lag factors
#' @param \dots any other passthru parameters
#' @author Brian Peterson
#' @references "Edhec Funds of Hedge Funds Reporting Survey : A Return-Based
#' Approach to Funds of Hedge Funds Reporting",Edhec Risk and Asset Management
#' Research Centre, January 2005,p. 27
#' 
#' Geltner, David, 1991, Smoothing in Appraisal-Based Returns, Journal of Real
#' Estate Finance and Economics, Vol.4, p.327-345.
#' 
#' Geltner, David, 1993, Estimating Market Values from Appraised Values without
#' Assuming an Efficient Market, Journal of Real Estate Research, Vol.8,
#' p.325-345.
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' head(Return.Geltner(managers[,1:3]),n=20)
#' 
#' @export
Return.ModGeltner <-
  function (Ra,q=3, ...)
  { # @author Brian G. Peterson, Peter Carl
    
    # Description:
    # Geltner Returns came from real estate where they are used to uncover a
    # liquidity-adjusted return series.
    
    # Ra    return vector
    
    # Function:
    R = checkData(Ra, method="xts")
    # Get dimensions and labels
    columns.a = ncol(R)
    columnnames.a = colnames(R)
    
    clean.modgeltner <- function(column.R,q=3) {
      # compute the lagged return series
      #lagR = lag(column.R, k=1)
      # compute the first order autocorrelation
      f_acf = as.numeric(acf(as.numeric(column.R), plot = FALSE)[1:q][[1]])
      # now calculate and return the Geltner series
      column.geltner = column.R
      for(i in 1:q){
      column.geltner = (column.geltner- lag(column.R, k=i)*f_acf[q])
      }
      column.geltner= column.geltner/sum(f_acf)
      }
    
    for(column.a in 1:columns.a) { # for each asset passed in as R
      # clean the data and get rid of NAs
      column.geltner = na.skip(R[,column.a], clean.modgeltner)
      
      if(column.a == 1)  { geltner = column.geltner }
      else { geltner = cbind (geltner, column.geltner) }
      
    }
    
    colnames(geltner) = columnnames.a
    
    # RESULTS:
    return(reclass(geltner,match.to=Ra))
    
  }

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.Geltner.R 2163 2012-07-16 00:30:19Z braverock $
#
###############################################################################
