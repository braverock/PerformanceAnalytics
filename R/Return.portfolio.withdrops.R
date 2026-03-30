#' Calculate weighted returns for a portfolio of assets with different last trade dates
#'
#' This function builds upon \code{Return.portfolio} for cases when the assets have
#' variable-length histories. For example, a security in the portfolio could have a
#' shorter history than others due to M&A or delisting. Currently, \code{Return.portfolio}
#' warns and assigns a zero return to such securities with missing returns.
#' This may not always be ideal. This function forces a rebalance when security
#' returns become unavailable, excluding such securities. \code{Return.portfolio.withdrops}
#' splices together spans with all available assets in each period, using \code{Return.portfolio}
#' to calculate returns in each span of complete assets.  Accordingly, it is highly recommended
#' that users be familiar with \code{Return.portfolio} before using this function.
#' \code{Return.portfolio.withdrops} takes the same arguments as \code{Return.portfolio}
#'
#' By default, this function calculates the time series of portfolio returns given asset
#' returns, weights, and non-missing values. The function will drop an asset from the
#' calculation when it encounters the first missing value. Dropped securities will not be
#' reincluded in portfolio even if not missing in subsequent time periods.
#' In verbose mode, the function displays the the individual securities included  
#' in each time span of the calculation.
#' 
#' As in \code{Return.portfolio} when asset return and weights are matched by period,
#' contribution is simply the weighted return of the asset.  c_i = w_i * R_i
#' Contributions are summable across the portfolio to calculate the total portfolio return.
#' 
#' Contribution cannot be aggregated through time.  For example, say we have an equal 
#' weighted portfolio of five assets with monthly returns.  The geometric return of the 
#' portfolio over several months won't match any aggregation of the individual 
#' contributions of the assets, particularly if any rebalancing was done during the 
#' period.
#' 
#' To aggregate contributions through time such that they are summable to the geometric 
#' returns of the portfolio, the calculation must track changes in the notional value of 
#' the assets and portfolio.  For example, contribution during a quarter will be 
#' calculated as the change in value of the position through those three months, divided 
#' by the original value of the portfolio.  Approaching it this way makes the 
#' calculation robust to weight changes as well. c_pi = V_(t-p)i - V_t)/V_ti  
#' 
#' If the user does not specify weights, an equal weight portfolio is assumed.
#' The portfolio is rebalanced at the start of every period that an asset is dropped.  
#' Alternatively, a vector or single-row matrix of weights that matches the length 
#' of the asset columns may be specified.  In either case, if no rebalancing period is
#' specified, the weights will be applied at the beginning of the asset time series
#' and no further rebalancing will take place. If a rebalancing period is specified, 
#' the portfolio will be rebalanced to the starting weights at the interval specified.
#' 
#' Note that if \code{weights} is an xts object, then any value passed to 
#' \code{rebalance_on} is ignored. The \code{weights} object specifies the 
#' rebalancing dates, therefore a regular rebalancing frequency provided via
#' \code{rebalance_on} is not needed and ignored.
#' 
#' \code{Return.portfolio.withdrops} will work only on daily or lower frequencies.
#' If you are rebalancing intraday, you should be using a trades/prices framework like 
#' the \code{blotter} package, not a weights/returns framework.
#' 
#' Irregular rebalancing can be done by specifying a time series of weights.  The 
#' function uses the date index of the weights for xts-style subsetting of rebalancing 
#' periods.
#' 
#' Weights specified for rebalancing should be thought of as "end-of-period" weights. 
#' Rebalancing periods can be thought of as taking effect immediately after the close 
#' of the bar. So, a March 31 rebalancing date will actually be in effect for April 1. 
#' A December 31 rebalancing date will be in effect on Jan 1, and so forth. This 
#' convention was chosen because it fits with common usage, and because it simplifies 
#' xts Date subsetting via endpoints.
#'
#' In verbose mode, the function returns a list of data and intermediary calculations.
#' 
#' \itemize{
#'   \item{\code{returns:}}{ The portfolio returns.}
#'   \item{\code{assetenddates:}}{ The dates when any asset stops trading.
#'    If there is more than one date, it means that at least one asset stopped
#'     trading before all others.}
#'   \item{\code{completeperiods:}}{ Dates in which all asset prices are available.}
#'   \item{\code{completeassets:}}{ Assets in initial period when all are available.}
#'   \item{\code{weights:}}{ The weights function call argument.}
#'   \item{\code{partials:}}{ A list of the dates and assets in each of the successive periods
#'   to be spliced together. Each span will contain fewer assets than the one preceding it. 
#'   The first partial span is numbered 1 and so on.}
#' }
#' 
#' 
#' @param R An xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param weights A time series or single-row matrix/vector containing asset
#' weights, as decimal percentages, treated as beginning of period weights.  
#' See Details below.
#' @param wealth.index TRUE/FALSE whether to return a wealth index. Default FALSE
#' @param contribution if contribution is TRUE, add the weighted return 
#' contributed by the asset in a given period. Default FALSE
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic (FALSE)
#' to aggregate returns. Default TRUE. 
#' @param rebalance_on Default "none"; alternatively "daily" "weekly" "monthly" 
#' "annual"  to specify calendar-period rebalancing supported by 
#' \code{\link[xts]{endpoints}}. Ignored if \code{weights} is an xts object
#' that specifies the rebalancing dates.
#' @param value The beginning of period total portfolio value. This is used for calculating position value.
#' @param verbose If verbose is TRUE, return a list of intermediate calculations. 
#' See Details below.
#' @param \dots any other passthru parameters. Not currently used.
#' @return Returns a time series of returns weighted by the \code{weights}
#' parameter, or a list that includes the date and assets in each intermediate span.
#' @author Peter Carl, Ross Bennett, Brian Peterson, Hernando Cortina
#' @seealso \code{\link{Return.portfolio}}  \cr
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. Chapter 2\cr
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' edhec['2008-03-01/', 1] <- NA
#' edhec['2008-05-01/', 2] <- NA
#' edhec['2008-06-01/', 3] <- NA
#' 
#' Return.portfolio.withdrops(edhec["2008", 1:5], rebalance_on="quarters") # returns time series
#' Return.portfolio.withdrops(edhec["2008", 1:5], rebalance_on="quarters", verbose=TRUE) # returns list
#' # with a weights object
#' data(weights) # rebalance at the beginning of the year to various weights through time
#' chart.StackedBar(weights)
#' x <- Return.portfolio.withdrops(edhec["2000/", 1:11], weights=weights, verbose=TRUE)
#' chart.CumReturns(x$returns)
#' x$assetenddates
#' x$completeperiods
#' x$completeassets
#' x$partials
#' 
#' @rdname Return.portfolio.withdrops
#' @export Return.portfolio.withdrops


# Portfolio return when assets have same start date and different end dates
# Assets are dropped from portfolio following last available priced date

Return.portfolio.withdrops <- function(R, weights = NULL, wealth.index = FALSE, contribution = FALSE, 
                                       geometric = TRUE, rebalance_on = c(NA, "years", "quarters","months", "weeks", "days"), value = 1,
                                       verbose = FALSE, ...) {
  R <- checkData(R, method = "xts")
  
  if ( anyNA(R[first(index(R))]) ) stop('At least one portfolio constituent price is unavailable on first time period.')
  
  if ( !is.null(weights) && is.null(names(weights)) ) names(weights) <- names(R)
  
  # Get the last trade date of each asset
  checkasset <- function(R, col) if (all(is.na(R[, col]) == F)) last(index(R)) else 
    index(R)[ min(which(is.na(R[, col])))-1 ]
  
  enddates <- as.Date(sapply(R, checkasset))
  
  # All assets have same end date, so just use Return.portfolio
  if ( length(unique(enddates)) == 1 ) return(Return.portfolio(R = R, weights = weights, wealth.index = wealth.index, contribution = contribution, 
                                                             geometric = geometric, rebalance_on = rebalance_on, value = value,
                                                             verbose = verbose, ...))
  # Get the dates of the period returns to splice  
  assetperiods <- sort(unique(enddates))
  
  # Return for first span to splice when all assets are available
  R_complete <- R[paste0('/', assetperiods[1]),]
  
  ret_all <- Return.portfolio(R = R_complete, weights = weights, wealth.index = F, contribution = F, 
                              geometric = geometric, rebalance_on = rebalance_on, value = value,
                              verbose = F, ...)
  
  if (verbose) partials <- list()
  
  # Calculate returns for each of the spans with decreasing number of assets and splice them together
  for(span in 1:(length(assetperiods)-1)) {
    R_partial <- R[paste(assetperiods[span]+1, assetperiods[span+1], sep='::'), enddates > assetperiods[span] ]
    
    if ( !is.null(weights) && is.xts(weights)) {weights <- weights[, names(R_partial)]; 
    weights <- weights / rowSums(weights, na.rm = TRUE) }
    
    if ( !is.null(weights) && is.vector(weights)) {weights <- weights[names(R_partial)]; 
    weights <- weights / sum(weights, na.rm = TRUE) }
    
    if (verbose) {partials <- c(partials, list(span=span,
                                     periods=index(R_partial),
                                     assets=colnames(R_partial))) }
      
    ret_partial <- Return.portfolio(R = R_partial, weights = weights, wealth.index = F, contribution = F, 
                                    geometric = geometric, rebalance_on = rebalance_on, value = value,
                                    verbose = F, freq = periodicity(R)$scale, ...)
    
    ret_all <- rbind(ret_partial, ret_all)
                                       }
  
  if (wealth.index) ret_all <- cumprod(1+ret_all) 
  
  if (verbose) return( list(
                      assetenddates=assetperiods,
                      completeperiods=index(R_complete),
                      completeassets=colnames(R_complete),
                      weights=weights,
                      partials=partials,
                      returns=ret_all) )
    
  return(ret_all)
  }


###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2019 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
