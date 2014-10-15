#' aggregate returns through time
#' 
#' Higher frequency returns provided as a time series are converted to a lower 
#' frequency for a specified calendar period.
#' 
#' description here
#' 
#' Valid period character strings for period include: "weeks", "months", "quarters", and "years". 
#' These are calculated internally via \code{\link{endpoints}}. See that function's help page for further details.
#' 
#' @param R a time series of the per period returns  
#' @param period period to convert to. See details."years", "quarters", "months", "weeks"
#' @param geometric  use geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns, default TRUE
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso \code{\link{Return.cumulative}} \cr \code{\link{endpoints}} \cr
#' @aliases to.monthly.returns to.weekly.returns to.quarterly.returns to.yearly.returns
#' @examples
#' data(managers)
#' to.period.returns(managers[,1:5], period="years")
#' to.yearly.returns(managers["2002::",1:5], geometric=TRUE)
#' @export
to.period.returns <- function(R, period = c("years", "quarters", "months", "weeks"), geometric = TRUE, ...){
  x = checkData(R)
  period = period[1] 
  columnnames = colnames(x)
  if(!xtsible(R))
    stop("'R' needs to be timeBased or xtsible." )
  # Make sure period > frequency of R
  err=FALSE
  freq = periodicity(x)
  switch(freq$scale,
      minute = {stop("Data periodicity too high")},
      hourly = {stop("Data periodicity too high")},
      daily = {ifelse(!period %in% c("years", "quarters", "months", "weeks"), err <- TRUE,NA)},
      weekly = {ifelse(!period %in% c("years", "quarters", "months"), err <- TRUE,NA)},
      monthly = {ifelse(!period %in% c("years", "quarters"), err <- TRUE,NA)},
      quarterly = {ifelse(!period %in% c("years"), err <- TRUE,NA)},
      yearly = {stop("Data periodicity too low")}
  )
  if(err) stop("Period specified is higher than data periodicity.  Specify a lower frequency instead.")
  
  # Calculate cumulative return for aggregation periods
  period.apply(x, INDEX=endpoints(x, period), FUN=Return.cumulative, geometric=geometric)

  return(period.ret)
  
}
#' @export
to.weekly.returns <- function(R) {
  to.period.returns(R = R, period = "weeks", geometric=TRUE)
}
#' @export
to.monthly.returns <- function(R) {
  to.period.returns(R = R, period = "months", geometric=TRUE)
}
#' @export
to.quarterly.returns <- function(R) {
  to.period.returns(R = R, period = "quarters", geometric=TRUE)
}
#' @export
to.yearly.returns <- function(R) {
  to.period.returns(R = R, period = "years", geometric=TRUE)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: $
#
###############################################################################