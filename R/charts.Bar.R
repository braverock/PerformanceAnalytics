#' @rdname chart.Bar
#' @export
charts.Bar <-
function (R, main = "Returns", cex.legend = 0.8, cex.main=1, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a small multiples of time series bar charts 

    # FUNCTION:

    # Transform input data to a data frame
    R = checkData(R)

    # Get dimensions and labels
    columns = NCOL(R)
    columnnames = colnames(R)

    ymax = max(R, na.rm=TRUE)
    ymin = min(R, na.rm=TRUE)
    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    yaxis=TRUE
    p <- chart.TimeSeries(R, multi.panel = TRUE, 
                          type = "h", 
                          lend = "butt", 
                          main = main, 
                          ylim = c(ymin,ymax), 
                          yaxis = yaxis, 
                          colorset = "darkgreen", 
                          lwd=2, ...)
    for(panel in 1:(columns - 1)) {
      p <- addSeries(xts(rep(0, nrow(R)), time(R)), on = (panel + 1), col = "darkgray")
    }
    
    if(hasArg(cex.legend) || !isTRUE(cex.legend)) {
      warning("The cex.legend argument of chart.TimeSeries has been deprecated, and may be removed in a future release, see help('chart.TimeSeries') for more information.")
    }
    
    return(p)
    
#         chart.Histogram(R[,i,drop=FALSE], xlim=c(ymin,ymax), main="", axes=FALSE)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################