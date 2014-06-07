#' Calculates weighted returns for a portfolio of assets
#' 
#' Calculates weighted returns for a portfolio of assets.
#' 
#' \code{Return.rebalancing} uses the date in the weights time series or matrix
#' for xts-style subsetting of rebalancing periods.  Rebalancing periods can be
#' thought of as taking effect immediately after the close of the bar.  So, a
#' March 31 rebalancing date will actually be in effect for April 1.  A
#' December 31 rebalancing date will be in effect on Jan 1, and so forth.  This
#' convention was chosen because it fits with common usage, and because it
#' simplifies xts Date subsetting via \code{endpoints}.
#' 
#' \code{Return.rebalancing} will rebalance only on daily or lower frequencies.
#' If you are rebalancing intraday, you should be using a trading/prices
#' framework, not a weights-based return framework.
#' 
#' @aliases Return.portfolio Return.rebalancing
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param beginning_weights a time series or single-row matrix/vector containing asset
#' weights, as decimal percentages, treated as beginning of next period weights.  See Details below.
#' @param wealth.index TRUE/FALSE whether to return a wealth index, default
#' FALSE
#' @param contribution if contribution is TRUE, add the weighted return
#' contributed by the asset in this period
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param \dots any other passthru parameters
#' @return returns a time series of returns weighted by the \code{weights}
#' parameter, possibly including contribution for each period
#' @author Brian G. Peterson
#' @seealso \code{\link{Return.calculate}} \cr
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. Chapter 2\cr
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' 
#' data(edhec)
#' data(weights)
#' 
#' # calculate an equal weighted portfolio return
#' round(Return.portfolio(edhec),4)
#' 
#' # now return the contribution too
#' round(Return.portfolio(edhec,contribution=TRUE),4)
#' 
#' # calculate a portfolio return with rebalancing
#' round(Return.rebalancing(edhec,weights),4)
#' 
#' @export
Return.rebalancing <- function (R, weights=NULL, on=c(NA, 'years', 'quarters', 'months', 'weeks', 'days'), verbose=FALSE, ..., adj.capital=FALSE) {
  on = on[1]
  R = checkData(R, method="xts")
  # find the right unit to subtract from the first return date to create a start date
  freq = periodicity(R)
  switch(freq$scale, 
	    seconds = { stop("Use a returns series of daily frequency or higher.") },
	    minute = { stop("Use a returns series of daily frequency or higher.") },
	    hourly = { stop("Use a returns series of daily frequency or higher.") },
	    daily = { time_unit = "day" },
	    weekly = { time_unit = "week" },
	    monthly = { time_unit= "month" },
	    quarterly = { time_unit = "quarter" },
	    yearly = { time_unit = "year"}
	)
  # calculates the end of the prior period
  start_date = seq(as.Date(index(R)[1]), length = 2, by = paste("-1", time_unit))[2]

  if(is.null(weights)){
    #   generate equal weight vector for return columns  
    weights = rep(1/NCOL(R), NCOL(R))
  }
  if(is.vector(weights)) { # weights are a vector
    if(is.na(endpoints)) { # and endpoints are not specified 
      # then use the weights only at the beginning of the returns series, without rebalancing
      weights = xts(weights, order.by=as.Date(start_date))
    }
    else { # and endpoints are specified
      #  generate a time series of the given weights at the endpoints
      weight_dates = c(as.Date(start_date),time(R[endpoints(R, on=on)]))
      weights = xts(matrix(rep(1/NCOL(R), length(weight_dates)*NCOL(R)), ncol=NCOL(R)), order.by=weight_dates)
    }
    colnames(weights) = colnames(R)
  }
  else { # check the beginning_weights object for errors
    # check that weights are given in a form that is probably a time series
    weights = checkData(weights, method="xts")
    # make sure that frequency(weights)<frequency(R) ?
    
    # make sure the number of assets in R matches the number of assets in weights
    if(NCOL(R) != NCOL(weights)){
      if(NCOL(R) > NCOL(weights)){
        R <- R[, 1:NCOL(weights)]
        warning("number of assets in beginning_weights is less than number of columns in returns, so subsetting returns.")
      } else {
        stop("number of assets is greater than number of columns in returns object")
      }
    }
  } # we should have good weights objects at this point

  leverage = 1
  # create an empty variables for storage
  x.capital_adj = NULL
  x.starting_weights = NULL
  x.ending_weights = NULL 
  x.sum_ending_weights = xts(matrix(1, ncol=1), order.by=as.Date(start_date))
  x.sum_starting_weights = NULL
  x.contributions = NULL 
  x.portfolio_return = NULL 
  # loop over rebalance periods
  start_date=index(weights)[1]

  for(i in 1:(NROW(weights)-1)) {
    # identify rebalance from and to dates (weights[i,], weights[i+1])
    from = as.Date(index(weights[i,]))+1
    to = as.Date(index(weights[i+1,]))
    returns = R[paste0(from,"::",to)]
    #print(returns)

    # get returns between rebalance dates
    for(j in 1:NROW(returns)){
      if(j==1) {# if first period of rebalance
        if(!adj.capital)
          starting_weights = as.numeric(last(x.sum_ending_weights,1)) * weights[i,]
        else
          starting_weights = weights[i,]
      }
      else
        starting_weights = last(x.ending_weights,1)
      contributions = coredata(starting_weights) * coredata(returns[j,])
      ending_weights = contributions + starting_weights # has the wrong date
      portfolio_return = sum(contributions)
      sum_prior_ending_weights = last(x.sum_ending_weights,1)
      sum_starting_weights = sum(starting_weights)
      sum_ending_weights = sum(ending_weights)
      capital_adj = sum(starting_weights) - sum_prior_ending_weights
  
      # store results
      x.starting_weights = rbind(x.starting_weights, xts(starting_weights, order.by=index(returns[j,])))
      x.contributions = rbind(x.contributions, xts(contributions, order.by=index(returns[j,])))
      x.ending_weights = rbind(x.ending_weights, xts(ending_weights, order.by=index(returns[j,])))
      x.portfolio_return = rbind(x.portfolio_return, xts(portfolio_return, order.by=index(returns[j,])))
      x.sum_starting_weights = rbind(x.sum_starting_weights, xts(sum_starting_weights, order.by=index(returns[j,])))
      x.sum_ending_weights = rbind(x.sum_ending_weights, xts(sum_ending_weights, order.by=index(returns[j,])))
      x.capital_adj = rbind(x.capital_adj, xts(capital_adj, order.by=index(returns[j,])))
    }
  }
  colnames(x.portfolio_return) = "Portfolio"
  colnames(x.capital_adj) = "Implied Capital Change"
  if(verbose){ # return full list of calculations
    result = list(Starting_Weights = x.starting_weights, 
                  Contributions = x.contributions, 
                  Ending_Weights = x.ending_weights, 
                  Portfolio_Return = x.portfolio_return, 
                  Sum_Ending_Weights = x.sum_ending_weights,
                  Implied_Capital_Adj = x.capital_adj
                  )
    return(result)
  }
  else { #  return resulting time series only
    result=x.portfolio_return
    result<-reclass(result, R)
    return(result)
  }
}

Return.rebalancing2 <- function (R, weights=NULL, on=c(NA, 'years', 'quarters', 'months', 'weeks', 'days'), verbose=FALSE, ..., adj.capital=FALSE) {
  on = on[1]
  R = checkData(R, method="xts")
  # find the right unit to subtract from the first return date to create a start date
  freq = periodicity(R)
  switch(freq$scale, 
         seconds = { stop("Use a returns series of daily frequency or higher.") },
         minute = { stop("Use a returns series of daily frequency or higher.") },
         hourly = { stop("Use a returns series of daily frequency or higher.") },
         daily = { time_unit = "day" },
         weekly = { time_unit = "week" },
         monthly = { time_unit= "month" },
         quarterly = { time_unit = "quarter" },
         yearly = { time_unit = "year"}
  )
  # calculates the end of the prior period
  start_date = seq(as.Date(index(R)[1]), length = 2, by = paste("-1", time_unit))[2]
  
  if(is.null(weights)){
    #   generate equal weight vector for return columns  
    weights = rep(1/NCOL(R), NCOL(R))
  }
  if(is.vector(weights)) { # weights are a vector
    if(is.na(endpoints)) { # and endpoints are not specified 
      # then use the weights only at the beginning of the returns series, without rebalancing
      weights = xts(weights, order.by=as.Date(start_date))
    }
    else { # and endpoints are specified
      #  generate a time series of the given weights at the endpoints
      weight_dates = c(as.Date(start_date),time(R[endpoints(R, on=on)]))
      weights = xts(matrix(rep(1/NCOL(R), length(weight_dates)*NCOL(R)), ncol=NCOL(R)), order.by=weight_dates)
    }
    colnames(weights) = colnames(R)
  }
  else { # check the beginning_weights object for errors
    # check that weights are given in a form that is probably a time series
    weights = checkData(weights, method="xts")
    # make sure that frequency(weights)<frequency(R) ?
    
    # make sure the number of assets in R matches the number of assets in weights
    if(NCOL(R) != NCOL(weights)){
      if(NCOL(R) > NCOL(weights)){
        R <- R[, 1:NCOL(weights)]
        warning("number of assets in beginning_weights is less than number of columns in returns, so subsetting returns.")
      } else {
        stop("number of assets is greater than number of columns in returns object")
      }
    }
  } # we should have good weights objects at this point
  
  leverage = 1
  # create an empty variables for storage
  #x.capital_adj = NULL
  x.capital_adj = xts(matrix(0, NROW(R), 1), as.Date(index(R)))
  
  #x.starting_weights = NULL
  x.starting_weights = xts(matrix(0, NROW(R), NCOL(R)), as.Date(index(R)))
  
  #x.ending_weights = NULL 
  x.ending_weights = x.starting_weights
  
  x.sum_ending_weights = xts(matrix(1, ncol=1), order.by=as.Date(start_date))
  
  #x.sum_starting_weights = NULL
  x.sum_starting_weights = xts(matrix(0, nrow(R), 1), as.Date(index(R)))
  
  #x.contributions = NULL
  x.contributions = x.starting_weights
  
  #x.portfolio_return = NULL
  x.portfolio_return = xts(matrix(0, NROW(R), 1), as.Date(index(R)))
  
  # loop over rebalance periods
  start_date=index(weights)[1]
  
  # counter
  k <- 1
  for(i in 1:(NROW(weights)-1)) {
    # identify rebalance from and to dates (weights[i,], weights[i+1])
    from = as.Date(index(weights[i,]))+1
    to = as.Date(index(weights[i+1,]))
    returns = R[paste0(from,"::",to)]
    #print(returns)
    
    # get returns between rebalance dates
    for(j in 1:NROW(returns)){
      if(j==1) {# if first period of rebalance
        if(!adj.capital)
          starting_weights = as.numeric(last(x.sum_ending_weights,1)) * weights[i,]
        else
          starting_weights = weights[i,]
      }
      else
        starting_weights = last(x.ending_weights,1)
      contributions = coredata(starting_weights) * coredata(returns[j,])
      ending_weights = contributions + starting_weights # has the wrong date
      portfolio_return = sum(contributions)
      sum_prior_ending_weights = last(x.sum_ending_weights,1)
      sum_starting_weights = sum(starting_weights)
      sum_ending_weights = sum(ending_weights)
      capital_adj = sum(starting_weights) - sum_prior_ending_weights
      
      # store results
      #x.starting_weights = rbind(x.starting_weights, xts(starting_weights, order.by=index(returns[j,])))
      x.starting_weights[k,] = starting_weights
      
      #x.contributions = rbind(x.contributions, xts(contributions, order.by=index(returns[j,])))
      x.contributions[k,] = contributions
      
      #x.ending_weights = rbind(x.ending_weights, xts(ending_weights, order.by=index(returns[j,])))
      x.ending_weights[k,] = ending_weights
      
      #x.portfolio_return = rbind(x.portfolio_return, xts(portfolio_return, order.by=index(returns[j,])))
      x.portfolio_return[k,] = portfolio_return
      
      #x.sum_starting_weights = rbind(x.sum_starting_weights, xts(sum_starting_weights, order.by=index(returns[j,])))
      x.sum_starting_weights[k,] = sum_starting_weights
      
      x.sum_ending_weights = rbind(x.sum_ending_weights, xts(sum_ending_weights, order.by=index(returns[j,])))
      #x.sum_ending_weights[k,] = sum_ending_weights
      
      #x.capital_adj = rbind(x.capital_adj, xts(capital_adj, order.by=index(returns[j,])))
      x.capital_adj[k,] = capital_adj
      k <- k + 1
    }
  }
  colnames(x.portfolio_return) = "Portfolio"
  colnames(x.capital_adj) = "Implied Capital Change"
  if(verbose){ # return full list of calculations
    result = list(Starting_Weights = x.starting_weights, 
                  Contributions = x.contributions, 
                  Ending_Weights = x.ending_weights, 
                  Portfolio_Return = x.portfolio_return, 
                  Sum_Ending_Weights = x.sum_ending_weights,
                  Implied_Capital_Adj = x.capital_adj
    )
    return(result)
  }
  else { #  return resulting time series only
    result=x.portfolio_return
    result<-reclass(result, R)
    return(result)
  }
}


#' Calculate weighted returns for a portfolio of assets
#'
#' Using a time series of returns and any regular or irregular time series of weights
#' for each asset, this function calculates the returns of a portfolio with the same 
#' periodicity of the returns data.  
#'
#' By default, this function calculates the time series of portfolio returns given asset
#' returns and weights. In verbose mode, the function returns a list of intermediary 
#' calculations that users may find helpful, including both asset contribution and  
#' asset value through time.
#' 
#' When asset return and weights are matched by period, contribution is simply the 
#' weighted return of the asset.  c_i = w_i * R_i Contributions are summable across the 
#' portfolio to calculate the total portfolio return.
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
#' Alternatively, a vector or single-row matrix of weights that matches the length 
#' of the asset columns may be specified.  In either case, if no rebalancing period is
#' specified, the weights will be applied at the beginning of the asset time series
#' and no further rebalancing will take place. If a rebalancing period is specified, 
#' the portfolio will be rebalanced to the starting weights at the interval specified.  
#' 
#' Return.rebalancing will work only on daily or lower frequencies. If you are 
#' rebalancing intraday, you should be using a trades/prices framework like 
#' {\link{\code{blotter}}}, not a weights/returns framework.
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
#' \itemize{
#'   \item{\code{returns}:}{ The portfolio returns.}
#'   \item{\code{contribution}:}{ The per period contribution to portfolio 
#'   return of each asset. Contribution is calculated as BOP weight times the 
#'   period's return divided by BOP value. Period contributions are summed 
#'   across the individual assets to calculate portfolio return}
#'   \item{\code{BOP.Weight}:}{ Beginning of Period (BOP) Weight for each 
#'   asset. An asset's BOP weight is calculated using the input weights 
#'   (or assumed weights, see below) and rebalancing parameters given. The next 
#'   period's BOP weight is either the EOP weights from the prior period or 
#'   input weights given on a rebalance period.}
#'   \item{\code{EOP.Weight:}}{ End of Period (BOP) Weight for each asset. 
#'   An asset's EOP weight is the sum of the asset's BOP weight and 
#'   contribution for the period divided by the sum of the contributions and 
#'   initial weights for the portfolio.}
#'   \item{\code{BOP.Value:}}{ BOP Value for each asset. The BOP value for each 
#'   asset is the asset's EOP value from the prior period, unless there is a 
#'   rebalance event.  If there is a rebalance event, the BOP value of the 
#'   asset is the rebalance weight times the EOP value of the portfolio. That 
#'   effectively provides a zero-transaction cost change to the position values 
#'   as of that date to reflect the rebalance.  Note that the sum of the BOP 
#'   values of the assets is the same as the prior period's EOP portfolio value.}
#'   \item{\code{EOP.Value:}}{ EOP Value for each asset. The EOP value is for 
#'   each asset is calculated as (1 + asset return) times the asset's BOP value. 
#'   The EOP portfolio value is the sum of EOP value across assets.}
#' }
#' 
#' To calculate BOP and EOP position value, we create an index for each position.  The 
#' sum of that value across assets represents an indexed value of the total portfolio.  
#' The change in value contained in slot seven is the asset's period return times its 
#' BOP value. 
#' 
#' From the value calculations, we can calculate different aggregations through time 
#' for the asset contributions.  Those are calculated as the EOP asset value less the 
#' BOP asset value; that quantity is divided by the BOP portfolio value.  
#' Across assets, those will sum to equal the geometric chained returns of the 
#' portfolio for that same time period.  The function does not do this directly, however.
#'
#' @aliases Return.portfolio Return.rebalancing
#' @param R An xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param weights A time series or single-row matrix/vector containing asset
#' weights, as decimal percentages, treated as beginning of period weights.  See Details below.
#' @param rebalance_on Default "none"; alternatively "daily" "weekly" "monthly" "annual"  to specify calendar-period rebalancing supported by \code{endpoints}.
#' @param value The beginning of period total portfolio value. This is used for calculating position value.
#' @param verbose If verbose is TRUE, return a list of intermediary calculations. 
#' See Details below.
#' @param \dots any other passthru parameters. Not currently used.
#' @return returns a time series of returns weighted by the \code{weights}
#' parameter, or a list that includes intermediate calculations
#' @author Peter Carl, Ross Bennett, Brian Peterson
#' @seealso \code{\link{Return.calculate}} \code{\link{xts::endpoints}} \cr
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. Chapter 2\cr
#' @keywords ts multivariate distribution models
#' @examples
#' data(edhec)
#' Return.rebalancing(edhec["1997",1:5], rebalance="quarterly") # returns time series
#' Return.rebalancing(edhec["1997",1:5], rebalance="quarterly", verbose=TRUE) # returns list
#' @export
Return.rebalancing3 <- function(R, 
                                weights=NULL,  
                                rebalance_on=c(NA, 'years', 'quarters', 'months', 'weeks', 'days'),
                                value=1,
                                verbose=FALSE,
                                ...){
  R = checkData(R, method="xts")
  rebalance_on = rebalance_on[1]
  
  # find the right unit to subtract from the first return date to create a start date
  freq = periodicity(R)
  switch(freq$scale, 
         seconds = { stop("Use a returns series of daily frequency or higher.") },
         minute = { stop("Use a returns series of daily frequency or higher.") },
         hourly = { stop("Use a returns series of daily frequency or higher.") },
         daily = { time_unit = "day" },
         weekly = { time_unit = "week" },
         monthly = { time_unit= "month" },
         quarterly = { time_unit = "quarter" },
         yearly = { time_unit = "year"}
  )
  
  # calculates the end of the prior period
  start_date = seq(as.Date(index(R)[1]), length = 2, by = paste("-1", time_unit))[2]
  
  if(is.null(weights)){
    # generate equal weight vector for return columns  
    weights = rep(1 / NCOL(R), NCOL(R))
  }
  if(is.vector(weights)) { # weights are a vector
    if(is.na(rebalance_on)) { # and endpoints are not specified 
      # then use the weights only at the beginning of the returns series, without rebalancing
      weights = xts(matrix(weights, nrow=1), order.by=as.Date(start_date))
    } else { # and endpoints are specified
      #  generate a time series of the given weights at the endpoints
      weight_dates = c(as.Date(start_date), index(R[endpoints(R, on=rebalance_on)]))
      weights = xts(matrix(rep(weights, length(weight_dates)), ncol=NCOL(R), byrow=TRUE), order.by=as.Date(weight_dates))
    }
    colnames(weights) = colnames(R)
  } else { # check the beginning_weights object for errors
    # check that weights are given in a form that is probably a time series
    weights = checkData(weights, method="xts")
    # make sure that frequency(weights)<frequency(R) ?
    
    # make sure the number of assets in R matches the number of assets in weights
    # Should we also check the names of R and names of weights?
    if(NCOL(R) != NCOL(weights)){
      if(NCOL(R) > NCOL(weights)){
        R = R[, 1:NCOL(weights)]
        warning("number of assets in beginning_weights is less than number of columns in returns, so subsetting returns.")
      } else {
        stop("number of assets is greater than number of columns in returns object")
      }
    }
  } # we should have good weights objects at this point
  
  if(as.Date(last(index(R))) < (as.Date(index(weights[1,]))+1)){
    stop(paste('last date in series',as.Date(last(index(R))),'occurs before beginning of first rebalancing period',as.Date(first(index(weights)))+1))
  }
  
  # Subset the R object if the first rebalance date is after the first date 
  # in the return series
  if(as.Date(index(weights[1,])) > as.Date(first(index(R)))) {
    R <- R[paste0(as.Date(index(weights[1,]))+1, "/")]
  }
  
  # bop = beginning of period
  # eop = end of period
  # Initialize objects
  bop_value = matrix(0, NROW(R), NCOL(R))
  colnames(bop_value) = colnames(R)
  eop_value = bop_value
  if(verbose){
    bop_weights = bop_value
    eop_weights = bop_value
    period_contrib = bop_value
  }
  ret = eop_value_total = bop_value_total = vector("numeric", NROW(R))
  
  # The end_value is the end of period total value from the prior period
  end_value <- value
  
  # initialize counter
  k = 1
  for(i in 1:NROW(weights)) {
    # identify rebalance from and to dates (weights[i,], weights[i+1]) and
    # subset the R(eturns) object
    from = as.Date(index(weights[i,]))+1
    if (i == nrow(weights)){
      to = as.Date(index(last(R))) # this is correct
    } else {
      to = as.Date(index(weights[(i+1),]))
    }
    returns = R[paste0(from, "::", to)]
    
    # Only enter the loop if we have a valid returns object
    if(nrow(returns) >= 1){
      # inner loop counter
      jj = 1
      for(j in 1:nrow(returns)){
        # We need to know when we are at the start of this inner loop so we can
        # set the correct beginning of period value. We start a new inner loop
        # at each rebalance date.
        # Compute beginning of period values
        if(jj == 1){
          bop_value[k,] = end_value * weights[i,]
        } else {
          bop_value[k,] = eop_value[k-1,]
        }
        bop_value_total[k] = sum(bop_value[k,])
        
        # Compute end of period values
        eop_value[k,] = (1 + coredata(returns[j,])) * bop_value[k,]
        eop_value_total[k] = sum(eop_value[k,])
        
        if(verbose){
          # Compute bop and eop weights
          bop_weights[k,] = bop_value[k,] / bop_value_total[k]
          eop_weights[k,] = eop_value[k,] / eop_value_total[k]
          # Compute period contribution
          period_contrib[k,] = returns[j,] * bop_value[k,] / sum(bop_value[k,])
        }
        
        # Compute portfolio returns
        # Could also compute this by summing contribution, but this way we
        # don't have to compute contribution if verbose=FALSE
        ret[k] = eop_value_total[k] / end_value - 1
        
        # Update end_value
        end_value = eop_value_total[k]
        
        # increment the counters
        jj = jj + 1
        k = k + 1
      }
    }
  }
  R.idx = index(R)
  ret = xts(ret, R.idx)
  colnames(ret) = "portfolio.returns"
  
  if(verbose){
    out = list()
    out$returns = ret
    out$contribution = xts(period_contrib, R.idx)
    out$BOP.Weight = xts(bop_weights, R.idx)
    out$EOP.Weight = xts(eop_weights, R.idx)
    out$BOP.Value = xts(bop_value, R.idx)
    out$EOP.Value = xts(eop_value, R.idx)
  } else {
    out = ret
  }
  return(out)
}

