# Two functions:
#   Return.normalize
#   chart.NDD

#' Calculate mean and volatility normalized time series
#'
#' 
#' @param R
#' @param targetMean
#' @param targetVol
#' @param ... passes arguments to par
#' @return xts or other time series
#' @author Peter Carl
#' @references Burghardt, G. Duncan, R., and Liu, L. (2003). Deciphering Drawdowns. Risk Magazine. Available at: http://www.risk.net/data/risk/pdf/investor/0903_risk.pdf
#' @rdname Return.normalize
#' @export Return.normalize
#' @export chart.NDD
Return.normalize <- (R, targetMean=0, targetVol=0, ...){
  # Peter Carl
  x=checkData(R)
  x.Mean=apply(x, MARGIN=2, FUN="mean", na.rm = TRUE)
  x.SD=StdDev(x)
  # @TODO wil this work for vector? checkData as matrix?
  # Apply z-score
  x.Z = apply(x, MARGIN=2, FUN=function(x){ (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE) }) # x.Z has mean=0, sd=1
  x.N= targetMean + x.Z * (rep(1, nrow(x.R)) %o% rep(targetVol,NCOL(x.R)))
  x.N = as.xts(x.N, by=index(x.R))
  x.N = reclass(x.N, R)
  return(x.N)
}

chart.NDD <- function(R, targetMean=0, targetVol=0){
 # Peter Carl 
  x.N = Return.normalize(x)
  x.NDD = PerformanceAnalytics:::Drawdowns(x.N)
  par(mar = c(3, 5, 2, 3)+0.1) #c(bottom, left, top, right)
  chart.TimeSeries(x.NDD[start.date,c(manager.col, index.cols, peer.cols)], colorset=colorset, lwd=lwdset, legend.loc=NULL, lty=lineset, main="", cex.axis=1.2, cex.lab=1.5)
  par(op)
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