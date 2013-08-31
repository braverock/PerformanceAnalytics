#'@title OW Return Model
#'@description The objective is to determine the true underlying return by removing the 
#' autocorrelation structure in the original return series without making any assumptions 
#' regarding the actual time series properties of the underlying process. We are 
#' implicitly assuming by this approach that the autocorrelations that arise in reported 
#'returns are entirely due to the smoothing behavior funds engage in when reporting 
#' results. In fact, the method may be adopted to produce any desired 
#' level of autocorrelation at any lag and is not limited to simply eliminating all 
#'autocorrelations.It can be be said as the general form of Geltner Return Model
#'@details 
#'Given a sample of historical returns \eqn{R(1),R(2), . . .,R(T)},the method assumes the fund manager smooths returns in the following manner:
#' \deqn{ r(0,t)  =  \sum \beta (i) r(0,t-i) + (1- \alpha)r(m,t) }
#' Where :\deqn{  \sum \beta (i) = (1- \alpha) }
#' \bold{r(0,t)} : is the observed (reported) return at time t (with 0 adjustments to reported returns), 
#'\bold{r(m,t)} : is the true underlying (unreported) return at time t (determined by making m adjustments to reported returns).
#'
#'To remove the \bold{first m orders} of autocorrelation from a given return series we would proceed in a manner very similar to that detailed in \bold{ \code{\link{Return.Geltner}} \cr}. We would initially remove the first order autocorrelation, then proceed to eliminate the second order autocorrelation through the iteration process. In general, to remove any order, m, autocorrelations from a given return series we would make the following transformation to returns:
#' autocorrelation structure in the original return series without making any assumptions regarding the actual time series properties of the underlying process. We are implicitly assuming by this approach that the autocorrelations that arise in reported returns are entirely due to the smoothing behavior funds engage in when reporting results. In fact, the method may be adopted to produce any desired level of autocorrelation at any lag and is not limited to simply eliminating all autocorrelations.
#' @param
#' Ra : an xts, vector, matrix, data frame, timeSeries or zoo object of
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
#' head(Return.Okunev(managers[,1:3]),n=3)
#' 
#'
#' @export
Return.Okunev<-function(Ra,q=3)
{
  R = checkData(Ra, method="xts")
  # Get dimensions and labels
  columns.a = ncol(R)
  columnnames.a = colnames(R)
  clean.okunev <- function(column.R) {
    # compute the lagged return series
    
    lagR = na.omit(lag(column.R,1))
    # compute the first order autocorrelation
    column.R= (column.R-(lagR*quad(lagR,0)))/(1-quad(lagR,0))
    return(column.R)
  }
  
  quad <- function(R,d)
  {
    coeff = as.numeric(acf(as.numeric(R), plot = FALSE)[1:2][[1]])
    b=-(1+coeff[2]-2*d*coeff[1])
    c=(coeff[1]-d)
    ans= (-b-sqrt(b*b-4*c*c))/(2*c)
    #a <- a[!is.na(a)]
    return(c(ans))               
  }
  
  
  for(column.a in 1:columns.a) { # for each asset passed in as R
    # clean the data and get rid of NAs
    column.okunev=R[,column.a]
    for(i in 1:q)
    {
      column.okunev <- clean.okunev(column.okunev)
      column.okunev=na.omit(column.okunev)
    }
    if(column.a == 1)  { okunev = column.okunev }
    else { okunev = cbind (okunev, column.okunev) }
    
  }
  
  #return(c(column.okunev))
  colnames(okunev) = columnnames.a
  
  # RESULTS:
  return(reclass(okunev,match.to=Ra))
  
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
