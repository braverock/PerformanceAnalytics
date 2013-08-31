#' Okunev and White Return Model
#'
#' The objective is to determine the true underlying return by removing the 
#' autocorrelation structure in the original return series without making any assumptions 
#' regarding the actual time series properties of the underlying process. We are 
#' implicitly assuming by this approach that the autocorrelations that arise in reported 
#' returns are entirely due to the smoothing behavior funds engage in when reporting 
#' results. In fact, the method may be adopted to produce any desired 
#' level of autocorrelation at any lag and is not limited to simply eliminating all 
#' autocorrelations.It can be be said as the general form of Geltner Return Model
#'
#'@details 
#' Given a sample of historical returns \eqn{R(1),R(2), . . .,R(T)},
#' the method assumes the fund manager smooths returns in the following manner:
#' \deqn{ r(0,t)  =  \sum \beta (i) r(0,t-i) + (1- \alpha)r(m,t) }
#' Where :\deqn{  \sum \beta (i) = (1- \alpha) }
#' \bold{r(0,t)} : is the observed (reported) return at time t (with 0 adjustments to reported returns), 
#' \bold{r(m,t)} : is the true underlying (unreported) return at 
#' time t (determined by making m adjustments to reported returns).
#'
#' To remove the \bold{first m orders} of autocorrelation from a given return 
#' series we would proceed in a manner very similar to that detailed in 
#' \bold{\code{\link{Return.Geltner}} \cr}. We would initially remove the first order 
#' autocorrelation, then proceed to eliminate the second order autocorrelation 
#' through the iteration process. In general, to remove any order, m, 
#' autocorrelations from a given return series we would make the following 
#' transformation to returns:
#' autocorrelation structure in the original return series without making any 
#' assumptions regarding the actual time series properties of the underlying 
#' process. We are implicitly assuming by this approach that the autocorrelations 
#' that arise in reported returns are entirely due to the smoothing behavior funds 
#' engage in when reporting results. In fact, the method may be adopted to produce 
#' any desired level of autocorrelation at any lag and is not limited to simply 
#' eliminating all autocorrelations.
#' 
#' @param
#' R : an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param 
#' q : order of autocorrelation coefficient lag factors
#'
#'
#'
#' @references Okunev, John and White, Derek R., \emph{ Hedge Fund Risk Factors and Value at Risk of Credit Trading Strategies} (October 2003). 
#' Available at SSRN: \url{http://ssrn.com/abstract=460641} 
#' @author Peter Carl, Brian Peterson, Shubhankit Mohan
#' @seealso  \code{\link{Return.Geltner}} \cr
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' Return.Okunev(managers)
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

#helper function for Return.Okunev, not exported
quad <- function(R,d)
{
  coeff = as.numeric(acf(as.numeric(R, plot = FALSE)[1:2][[1]]))
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
