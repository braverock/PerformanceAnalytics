# Two functions:
#   Return.normalize
#   chart.NDD

#' Calculate mean and volatility normalized time series
#'
#' 
#' @param R
#' @param targetMean specifies a return level to normalize each column to, in the same time units as the timeseries; default is zero
#' @param targetVol specifies a volatility level to normalize each column to, in the same time units as the timeseries
#' @param show.units.sd Normalize the plot's y-axis to units of annualized standard deviation
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param ... passes arguments to par
#' @return xts or other time series
#' @author Peter Carl
#' @references Burghardt, G. Duncan, R., and Liu, L. (2003). Deciphering Drawdowns. Risk Magazine. Available at: http://www.risk.net/data/risk/pdf/investor/0903_risk.pdf
#' @rdname Return.normalize
#' @export Return.normalize
#' @export chart.NDD
Return.normalize <- function(R, targetMean=0, targetVol, ...){
  # Peter Carl
  x=checkData(R)
  x.Mean=apply(x, MARGIN=2, FUN="mean", na.rm = TRUE)
  x.SD=StdDev(x)
  # @TODO wil this work for vector? checkData as matrix?
  # Apply z-score
  x.Z = apply(x, MARGIN=2, FUN=function(x){ (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE) }) # x.Z has mean=0, sd=1
  x.N= targetMean + x.Z * (rep(1, nrow(x)) %o% rep(targetVol,NCOL(x)))
  x.N = as.xts(x.N, by=index(x.R))
  x.N = reclass(x.N, R)
  return(x.N)
}

chart.NDD <- function(R, targetMean=0, targetVol, show.units.sd=FALSE, ylab="Drawdown", scale=NA, ...){
 # Peter Carl 
  # @TODO: determine periodicity of R, specify multiplier for x.NDD below
  if(is.na(scale)) {
    freq = periodicity(R)
    switch(freq$scale,
           minute = {stop("Data periodicity too high")},
           hourly = {stop("Data periodicity too high")},
           daily = {scale = 252},
           weekly = {scale = 52},
           monthly = {scale = 12},
           quarterly = {scale = 4},
           yearly = {scale = 1}
    )
  }
  x.N = Return.normalize(R, targetMean=targetMean, targetVol=targetVol)
  x.NDD = PerformanceAnalytics:::Drawdowns(x.N)
  if(show.units.sd){
    x.NDD = x.NDD/(targetVol*sqrt(scale)) # targetVol must be annualized
    ylab = "Annualized Std Dev"
  }
  # par(mar = c(3, 5, 2, 3)+0.1) #c(bottom, left, top, right)
  chart.TimeSeries(x.NDD, ylab=ylab, ...)
  # par(op)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id:  $
#
###############################################################################