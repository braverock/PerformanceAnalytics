#' aggregate contributions through time
#' 
#' Higher frequency contributions provided as a time series are converted to a lower 
#' frequency for a specified calendar period.
#' 
#' From the portfolio contributions of individual assets, such as those of a particular asset class 
#' or manager, the multiperiod contribution is neither summable from nor the geometric compounding of
#' single-period contributions.  Because the weights of the individual assets change through time as
#' transactions occur, the capital base for the asset changes.
#' 
#' Instead, the asset's multiperiod contribution is the sum of the asset's dollar contributions from
#' each period, as calculated from the wealth index of the total portfolio. Once contributions are
#' expressed in cumulative terms, asset contributions then sum to the returns of the total portfolio for 
#' the period.
#' 
#' Valid period character strings for period include: "weeks", "months", "quarters", and "years". 
#' These are calculated internally via \code{\link{endpoints}}. See that function's help page for further details.
#' 
#' @param Contributions a time series of the per period contribution to portfolio return of each asset 
#' @param period period to convert to. See details."years", "quarters", "months", "weeks"
#' @param \dots any other passthru parameters
#' @author Peter Carl, with thanks to Paolo Cavatore
#' @seealso \code{\link{Return.portfolio}} \cr \code{\link{endpoints}} \cr
#' @references Morningstar \emph{Total Portfolio Performance Attribution Methodology} p.36. Available at http://corporate.morningstar.com/US/documents/MethodologyDocuments/MethodologyPapers/TotalPortfolioPerformanceAttributionMethodology.pdf \cr
#' @aliases to.monthly.contributions to.weekly.contributions to.quarterly.contributions to.yearly.contributions
#' @examples
#' data(managers)
#' res_qtr_rebal = Return.portfolio(managers["2002::",1:5], weights=c(.05,.1,.3,.4,.15), rebalance_on = "quarters", verbose=TRUE)
#' to.period.contributions(res_qtr_rebal$contribution, period="years")
#' to.yearly.contributions(res_qtr_rebal$contribution)
#' @export
to.period.contributions <- function(Contributions, period = c("years", "quarters", "months", "weeks"), ...){
  C = checkData(Contributions)
  period = period[1] 
  columnnames = colnames(C)
  if(!xtsible(Contributions))
    stop("'Contributions' needs to be timeBased or xtsible." )
  # Make sure period > frequency of C
  err=FALSE
  freq = periodicity(C)
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
  
  # Calculate period return of portfolio from contributions
  pret = rowSums(C)
  pret = xts(pret, order.by=index(C))
  lag.cum.ret <- na.fill(lag(cumprod(1+pret),1),1) 
  wgt.contrib = C * rep(lag.cum.ret, NCOL(C))
  
  # Calculate aggregation periods
  ep = endpoints(C, period)
  dates = index(C)[endpoints(C, period)]

  # Summarize weighted contributions by period
  period.wgt.contrib = apply(wgt.contrib, 2, function (x, ep) period.apply(x, INDEX=ep, FUN=sum), ep=ep)
  period.wgt.contrib = as.xts(period.wgt.contrib, order.by=dates)

  # Normalize to the beginning of period value
  period.contrib = NULL
  for(i in 1:length(dates)) {
    if(i==1){
      span = paste0("::", dates[i])
    }else{
      span = paste0(dates[i-1], "::", dates[i])
    }
    period.contrib = rbind(period.contrib, colSums(wgt.contrib[span]/rep(head(lag.cum.ret[span],1),NCOL(wgt.contrib))))
  }
  period.contrib = as.xts(period.contrib, order.by = dates)
  period.contrib = cbind(period.contrib, rowSums(period.contrib))
  colnames(period.contrib) = c(columnnames, "Portfolio Return")
  period.contrib = reclass(period.contrib, x)

  return(period.contrib)
  
}
#' @export
to.weekly.contributions <- function(contributions) {
  to.period.contributions(contributions = contributions, period = "weeks")
}
#' @export
to.monthly.contributions <- function(contributions) {
  to.period.contributions(contributions = contributions, period = "months")
}
#' @export
to.quarterly.contributions <- function(contributions) {
  to.period.contributions(contributions = contributions, period = "quarters")
}
#' @export
to.yearly.contributions <- function(contributions) {
  to.period.contributions(contributions = contributions, period = "years")
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