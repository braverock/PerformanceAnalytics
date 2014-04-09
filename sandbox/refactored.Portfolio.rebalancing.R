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
Return.rebalancing2 <- function (R, weights=NA, on=c(NA, 'years', 'quarters', 'months', 'weeks', 'days'), verbose=FALSE, ..., adj.capital=FALSE) {
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
  start_date = seq(as.Date(index(R)[1]), length = 2, by = paste("-1", time_unit))[2]

  if(is.na(weights)){
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
    if(CNOL(R) != NCOL(weights)){
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
  x.leverage = matrix(1, ncol=1)
  x.starting_weights = NULL
  x.ending_weights = matrix(NA, ncol=NCOL(weights))
  x.contributions = matrix(NA, ncol=NCOL(weights))
  x.portfolio_return = matrix(NA, ncol=1)
  # loop over rebalance periods
  start_date=index(weights)[1]
  for(i in 1:NROW(weights)) {
    # identify rebalance from and to dates (weights[i,], weights[i+1])
    from = as.Date(index(weights[i,]))
    to = as.Date(index(weights[i+1,]))
    returns = R[paste0(from,"::",to)]
    # get returns between rebalance dates
    for(j in 1:NROW(returns)){
      if(j==1) # if first period of rebalance
        starting_weights = as.numeric(last(x.leverage,1)) * weights[i,]
      else
        starting_weights = last(x.ending_weights,1)
      contributions = as.vector(starting_weights) * as.vector(returns[j,])
      ending_weights = contributions + starting_weights # has the wrong date
      portfolio_return = sum(contributions)
      if(j==NROW(returns) & adj.capital==FALSE)
        leverage = sum(last(ending_weights,1))
      # store results
      if(is.null(x.starting_weights)) 
        x.starting_weights = starting_weights
      else
        x.starting_weights = rbind(x.starting_weights, starting_weights)
#       x.contributions = rbind(x.contributions, contributions)
      if(is.null(x.contributions)) 
        x.contributions = contributions
      else
        x.contributions = rbind(x.contributions, contributions)
#       x.ending_weights = rbind(x.ending_weights, ending_weights)
      if(is.null(x.ending_weights)) 
        x.ending_weights = ending_weights
      else
        x.ending_weights = rbind(x.ending_weights, ending_weights)
#       x.portfolio_return = rbind(x.portfolio_return, portfolio_return)
      if(is.null(x.portfolio_return)) 
        x.portfolio_return = portfolio_return
      else
        x.portfolio_return = rbind(x.portfolio_return, portfolio_return)
#       x.leverage = rbind(x.leverage, leverage)
      if(is.null(x.leverage)) 
        x.leverage = leverage
      else
        x.leverage = rbind(x.leverage, leverage)
    }

    # if verbose = TRUE
    #   return list
    # else
    #   return portfolio_return time series
  }
  result=portfolio_returns
  result<-reclass(result, R)
  result
}