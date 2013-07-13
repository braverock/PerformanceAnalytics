#' calculate Geltner liquidity-adjusted return series
#' 
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
#' \deqn{ }{Geltner.returns = [R(t) - R(t-1)*acf(R(t-1))]/1-acf(R(t-1)) }\deqn{
#' R_{G}=\frac{R_{t}-(R_{t-1}\cdot\rho_{1})}{1-\rho_{1}} }{Geltner.returns =
#' [R(t) - R(t-1)*acf(R(t-1))]/1-acf(R(t-1)) }
#' 
#' where \eqn{\rho_{1}}{acf(R(t-1))} is the first-order autocorrelation of the
#' return series \eqn{R_{a}}{Ra} and \eqn{R_{t}}{R(t)} is the return of
#' \eqn{R_{a}}{Ra} at time \eqn{t} and \eqn{R_{t-1}}{R(t-1)} is the one-period
#' lagged return.
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
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
Return.GLM <-
  function (Ra,q=3)
  { # @author Brian G. Peterson, Peter Carl
    
    # Description:
    # Geltner Returns came from real estate where they are used to uncover a
    # liquidity-adjusted return series.
    
    # Ra    return vector
    # q     Lag Factors
    # Function:
    R = checkData(Ra, method="xts")
    # Get dimensions and labels
    columns.a = ncol(R)
    columnnames.a = colnames(R)
    
    clean.GLM <- function(column.R,q=3) {
      ma.coeff = as.numeric(arma(column.R, order = c(0,q))$coef)
#      for( i in 1: q)
 #     {
#        if(q == 1){column.glm = ma.coeff[i]*lag(column.R,i)}
#else{        column.glm = ma.coeff[i]*lag(column.R,i)+ column.glm} 
 #     }
      column.glm = ma.coeff[q]*lag(column.R,q)
      # compute the lagged return series
      #lagR = lag(column.R, k=1)
      # compute the first order autocorrelation
      #f_acf = as.numeric(acf(as.numeric(column.R), plot = FALSE)[1][[1]])
      # now calculate and return the Geltner series
      #column.geltner = (column.R-(lagR*f_acf))/(1-f_acf)
    return(column.glm)
    }
    
    for(column.a in 1:columns.a) { # for each asset passed in as R
      # clean the data and get rid of NAs
      column.glma = na.skip(R[,column.a],clean.GLM)
      
      if(column.a == 1)  { glm = column.glma }
      else { glm = cbind (glm, column.glma) }
      
    }
    
    colnames(glm) = columnnames.a
    
    # RESULTS:
    return(reclass(glm,match.to=Ra))
    
  }

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.GLM.R 2163 2012-07-16 00:30:19Z braverock $
#
###############################################################################
