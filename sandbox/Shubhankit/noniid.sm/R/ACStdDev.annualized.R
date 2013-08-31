#' @title Autocorrleation adjusted Standard Deviation 
#' @description Incorporating the component of lagged autocorrelation factor into adjusted time scale standard deviation translation
#' @details Given a sample of historical returns R(1),R(2), . . .,R(T),the method assumes the fund manager smooths returns in the following manner, when 't' is the unit time interval:
#'  The square root time translation can be defined as :
#'   \deqn{ \sigma(T)  =  T \sqrt\sigma(t)}  
#' @aliases sd.multiperiod sd.annualized StdDev.annualized
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param lag : number of autocorrelated lag factors inputted by user
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param \dots any other passthru parameters
#' @author Peter Carl,Brian Peterson, Shubhankit Mohan
#' \url{http://en.wikipedia.org/wiki/Volatility_(finance)}
#' @references Burghardt, G., and L. Liu, \emph{ It's the Autocorrelation, Stupid (November 2012) Newedge
#' working paper.}
#' \url{http://www.amfmblog.com/assets/Newedge-Autocorrelation.pdf}
#' @keywords ts multivariate distribution models
#' @examples
#' library(PerformanceAnalytics)
#' data(edhec)
#' ACStdDev.annualized(edhec,3)
#' 
#' @export
#' @rdname ACStdDev.annualized
ACStdDev.annualized <- ACsd.annualized <- ACsd.multiperiod <-
  function (R,lag=6, scale = NA, ...)
  {
    columns.a = ncol(R)
    columnnames.a = colnames(R)
    if(is.na(scale) && !xtsible(R))
      stop("'x' needs to be timeBased or xtsible, or scale must be specified." )
    
    if(is.na(scale)) {
      freq = periodicity(R)
      switch(freq$scale,
             #kChec
             minute = {stop("Data periodicity too high")},
             hourly = {stop("Data periodicity too high")},
             daily = {scale = 252},
             weekly = {scale = 52},
             monthly = {scale = 12},
             quarterly = {scale = 4},
             yearly = {scale = 1}
      )
    }
    
    for(column.a in 1:columns.a) { # for each asset passed in as R
      # clean the data and get rid of NAs
      column.return = R[,column.a]
      acf = as.numeric(acf(as.numeric(column.return), plot = FALSE)[1:lag][[1]])
      coef= sum(acf*acf)
      if(!xtsible(R) & is.na(scale))
      {
        stop("'x' needs to be timeBased or xtsible, or scale must be specified." )
      }
      else
      {
        if(column.a == 1)  { result = as.numeric(StdDev.annualized(column.return))*(1+2*coef) }
      else { result = cbind (result, as.numeric(StdDev.annualized(column.return))*(1+2*coef)) }
    }
    }
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = "Autocorrelated Annualized Standard Deviation"
    return(result)
  }

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2013 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: ACStdDev.annualized.R 
#
###############################################################################
