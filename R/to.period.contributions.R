#'
#' Aggregate contributions through time
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
#' Valid period character strings for period include: "weeks", "months", "quarters", "years", or "all".
#' These are calculated internally via \code{\link[xts]{endpoints}}. See that function's help page for further details.
#'
#' For the special period "all", the contribution is calculated over all rows,
#' giving a single contribution across all observations.
#'
#' @param Contributions a time series of the per period contribution to portfolio return of each asset
#' @param period period to convert to.  See details. "weeks", "months", "quarters", "years", or "all".
#' @author Peter Carl, with thanks to Paolo Cavatore
#' @seealso \code{\link{Return.portfolio}} \cr \code{\link[xts]{endpoints}} \cr
#' @references Morningstar, \emph{Total Portfolio Performance Attribution Methodology}, p.36.
#'   Available at
#'   \url{https://corporate.morningstar.com/US/documents/MethodologyDocuments/MethodologyPapers/TotalPortfolioPerformanceAttributionMethodology.pdf}
#' @aliases to.monthly.contributions to.weekly.contributions to.quarterly.contributions to.yearly.contributions
#'
#' @export
#'
#' @examples
#' data(managers, package="PerformanceAnalytics")
#' 
#' res_qtr_rebal = Return.portfolio(  managers["2002::",1:5]
#'                                  , weights=c(.05,.1,.3,.4,.15)
#'                                  , rebalance_on = "quarters"
#'                                  , verbose=TRUE)
#'                                  
#' to.period.contributions(res_qtr_rebal$contribution, period="years")
#' to.yearly.contributions(res_qtr_rebal$contribution)
#'
to.period.contributions <- function(Contributions, period = c("years", "quarters", "months", "weeks", "all")){
  C = checkData(Contributions)
  period = period[1]
  if (!(period %in% c("years", "quarters", "months", "weeks", "all"))) stop("Invalid period: ", period)
  columnnames = colnames(C)
  if(!xtsible(Contributions))
    stop("'Contributions' needs to be timeBased or xtsible." )

  # Make sure period > frequency of C,
  # but only check the data frequency if there are more than one observation.
  # For single row observation, we do whole period attribution.
  if (nrow(C) > 1) {
    freq = periodicity(C)
    ok = switch(freq$scale,
                minute = {stop("Data periodicity too high")},
                hourly = {stop("Data periodicity too high")},
                daily = (period %in% c("years", "quarters", "months", "weeks", "all")),
                weekly = (period %in% c("years", "quarters", "months", "all")),
                monthly = (period %in% c("years", "quarters", "all")),
                quarterly = (period %in% c("years", "all")),
                yearly = {stop("Data periodicity too low")}
    )
    if (!ok)
      stop("Period specified is higher than data periodicity.  Specify a lower frequency instead.")
  }

  # Calculate period return of portfolio from contributions
  pret = rowSums(C)
  pret = xts(pret, order.by=index(C))
  lag.cum.ret = na.fill(xts::lag.xts(cumprod(1+pret),1),1)
  wgt.contrib = C * rep(lag.cum.ret, NCOL(C))

  # Calculate aggregation periods
  if( period == "all"){
    ep = c(0, nrow(C))
  }else{
    ep = endpoints(C, period)
  }
  dates = index(C)[ep]

  # Normalize to the beginning of period value
  period.contrib = NULL
  for(i in 1:length(dates)) {
    if(i==1){
      span = (index(C) <= dates[[1]])
    }else{
      span = (dates[[i-1]] < index(C) & index(C) <= dates[[i]])
    }
    period.contrib = rbind(period.contrib,
                           colSums(wgt.contrib[span]/rep(head(lag.cum.ret[span],1),NCOL(wgt.contrib))) )
  }
  period.contrib = xts(period.contrib, order.by = dates)
  period.contrib = cbind(period.contrib, rowSums(period.contrib))
  colnames(period.contrib) = c(columnnames, "Portfolio Return")
  period.contrib = reclass(period.contrib, Contributions)

  return(period.contrib)
}

#' @export
to.weekly.contributions <- function(Contributions) {
  to.period.contributions(Contributions = Contributions, period = "weeks")
}
#' @export
to.monthly.contributions <- function(Contributions) {
  to.period.contributions(Contributions = Contributions, period = "months")
}
#' @export
to.quarterly.contributions <- function(Contributions) {
  to.period.contributions(Contributions = Contributions, period = "quarters")
}
#' @export
to.yearly.contributions <- function(Contributions) {
  to.period.contributions(Contributions = Contributions, period = "years")
}

################################################################################
#
# R (https://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2020 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL).
# For full details see the file COPYING.
#
# $Id: $
#
################################################################################
