#'
#'The objective is to determine the true underlying return by removing the 
#' autocorrelation structure in the original return series without making any assumptions 
#' regarding the actual time series properties of the underlying process. We are 
#' implicitly assuming by this approach that the autocorrelations that arise in reported 
#'returns are entirely due to the smoothing behavior funds engage in when reporting 
#' results. In fact, the method may be adopted to produce any desired 
#' level of autocorrelation at any lag and is not limited to simply eliminating all 
#'autocorrelations.It can be be said as the general form of Geltner Return Model
#'
#' @references "Hedge Fund Risk Factors and Value at Risk of Credit 
#' Trading Strategies , John Okunev & Derek White
#' 
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' head(Return.Okunev(managers[,1:3]),n=3)
#' 
#'
#' @export

Return.Okunev<-function(R,q=3)
{
  column.okunev=R
  column.okunev <- column.okunev[!is.na(column.okunev)]
  for(i in 1:q)
  {
    lagR = lag(column.okunev, k=i)
    column.okunev= (column.okunev-(lagR*quad(lagR,0)))/(1-quad(lagR,0))
  }
  return(c(column.okunev))
}
#' Recusrsive Okunev Call Function
quad <- function(R,d)
{
  coeff = as.numeric(acf(as.numeric(edhec[,1]), plot = FALSE)[1:2][[1]])
b=-(1+coeff[2]-2*d*coeff[1])
c=(coeff[1]-d)
  ans= (-b-sqrt(b*b-4*c*c))/(2*c)
  #a <- a[!is.na(a)]
  return(c(ans))               
}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.Okunev.R 2163 2012-07-16 00:30:19Z braverock $
#
###############################################################################

