# Generalize this to take any function, determine the rankings, and draw a bar plot in order of the ranking
# Peter Carl
chart.RankBars <- function(R, FUN="mean", ...){
  # @todo split dots between the function and par
  if(!is.function(FUN))
    FUN = match.fun("FUN",...)
  t.AC = table.Autocorrelation(last(x.R[,c(manager.col, index.cols, peer.cols)],36))
  y=colSums(t.AC[1:6,])
  layout(matrix(1:2,ncol=1), heights=c(3,2))
  par(mar = c(1, 5, 3, 3)+0.1) #c(bottom, left, top, right)
  barplot(y[order(y)], col=colorset[order(y)], border = NA, axisnames=FALSE, ylim=range(pretty(y)), cex.axis=1.2, cex.lab=1.5, cex.main=2, ylab="Sum of Lag 1-6 AC", main="Trailing 36-month Autocorrelation")
  box()
  barplot(as.numeric(t.AC[7,order(y)]), col=colorset[order(y)], ylim=range(pretty(c(0,1))), axisnames=FALSE, border=NA, cex.axis=1.2, cex.lab=1.5, ylab="Q(6) p-value")
  box()
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