#'@title 
#' Time series of Rolling Economic Drawdown
#'
#'@description
#' This function plots the time series of Rolling Economic Drawdown. 
#' For more details on rolling economic drawdown see \code{rollDrawdown}.
#'
#'@param R an xts, vector, matrix, data frame, timeseries, or zoo object of asset return.
#'@param rf risk free rate can be vector such as government security rate of return
#'@param h lookback period 
#'@param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining(FALSE) to aggregate returns, default is TRUE.
#'@param legend.loc set the legend.loc, as in \code{\link{plot}}
#'@param colorset set the colorset label, as in \code{\link{plot}}
#'@param \dots any other  variable
#'@author Pulkit Mehrotra
#'@seealso  \code{\link{EconomicDrawdown}} \code{\link{EDDCOPS}} 
#'\code{\link{rollDrawdown}} \code{\link{REDDCOPS}} \code{\link{rollEconomicMax}}
#'@references Yang, Z. George and Zhong, Liang, Optimal Portfolio Strategy to 
#'Control Maximum Drawdown - The Case of Risk Based Dynamic Asset Allocation (February 25, 2012)
#'@examples
#'data(edhec)
#'chart.REDD(edhec,0.08,20)
#'
#'@export

chart.REDD<-function(R,rf,h, geometric = TRUE,legend.loc = NULL, colorset = (1:12),...)
{
#DESCRIPTION:
#A function to create the chart for the rolling economic drawdown
#
  # calculates the Rolling Economic Drawdown(REDD) for
  # a return series.To calculate the rolling economic drawdown cumulative 
  # return and rolling economic max is calculated for each point. The risk 
  # free return(rf) and the lookback period(h) is taken as the input.
 

    rolldrawdown = rollDrawdown(R,geometric = TRUE,weights = NULL,rf,h)
    chart.TimeSeries(rolldrawdown, colorset = colorset, legend.loc = legend.loc, ...)
}


