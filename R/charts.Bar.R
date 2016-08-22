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
    # pass title size
    p$Env$cex.main <- cex.main
    text.exp <- expression(text(xlim[1], 0.5, main, font = 2, col = theme$labels, 
                                offset = 0, cex = cex.main, pos = 4), 
                           text(xlim[2], 0.5, paste(start(xdata[xsubset]), end(xdata[xsubset]), sep = " / "), 
                                col = theme$labels, adj = c(0, 0), pos = 2))
    p$Env$actions[[5]] <- structure(structure(structure(text.exp, frame=1), clip=TRUE), env=p$Env)
    # pass legend size
    p$Env$cex.legend <- cex.legend
    for(panel in 1:columns) {
      p <- addSeries(xts(rep(0, nrow(R)), time(R)), on = panel, col = "darkgray")
    }
    
    return(p)
    

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