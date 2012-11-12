#' Systematic risk of the return distribution
#'
#' Systematic risk as defined by Bacon(2008) is the product of beta by market 
#' risk. Be careful ! It's not the same definition as the one given by Michael
#' Jensen. Market risk is the standard deviation of the benchmark. The systematic
#' risk is annualized
#'
#' \deqn{\sigma_s = \beta * \sigma_m}{systematic risk = beta * market risk}
#'
#' where \eqn{\sigma_s} is the systematic risk, \eqn{\beta} is the regression beta,
#' and \eqn{\sigma_m} is the market risk
#'
#' @aliases SystematicRisk
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset 
#' @param Rf risk free rate, in same period as your returns
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.75
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(SystematicRisk(portfolio_bacon[,1], portfolio_bacon[,2])) #expected 0.013
#'
#' data(managers)
#' print(SystematicRisk(managers['1996',1], managers['1996',8]))
#' print(SystematicRisk(managers['1996',1:5], managers['1996',8]))
#'
#' @export 
SystematicRisk <-
function (Ra, Rb, Rf = 0, scale = NA, ...)
{
  # FUNCTION:
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  if(!is.null(dim(Rf)))
    Rf = checkData(Rf)
  
  if(is.na(scale)) {
    freq = periodicity(Ra)
    switch(freq$scale,
           minute = {stop("Data periodicity too high")},
           hourly = {stop("Data periodicity too high")},
           daily = {scale = 252},
           weekly = {scale = 52},
           monthly = {scale = 12},
           quarterly = {scale = 4},
           yearly = {scale = 1}
    )
  }
  
  Ra.ncols = NCOL(Ra) 
  Rb.ncols = NCOL(Rb)
  
  xRa = Return.excess(Ra, Rf)
  xRb = Return.excess(Rb, Rf)
  
  pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
  
  sr <- function(xRa, xRb, scale){
      result = CAPM.beta(xRa, xRb) * StdDev.annualized(xRb, scale=scale)
      return(result)
  }
  
  result = apply(pairs, 1, FUN = function(n, xRa, xRb, scale) sr(xRa[,n[1]], xRb[,n[2]], scale), xRa = xRa, xRb = xRb, scale=scale)
  
  if(length(result) ==1)
    return(result)
  else {
    dim(result) = c(Ra.ncols, Rb.ncols)
    colnames(result) = paste("Systematic Risk to ", colnames(Rb), " (Rf = ",Rf,")", sep="")
    rownames(result) = colnames(Ra)
    return(t(result))
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
# $Id$
#
###############################################################################
