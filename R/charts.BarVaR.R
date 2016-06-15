#' @rdname chart.BarVaR
#' @export
charts.BarVaR <-
function (R, main = "Returns", cex.legend = 0.8, colorset=1:12, ylim=NA, ..., perpanel = NULL, show.yaxis = c("all", "firstonly", "alternating", "none"))
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a small multiples of time series bar charts 

    # FUNCTION:

    # Transform input data to a data frame
    R = checkData(R)
    show.yaxis = show.yaxis[1]
    # Get dimensions and labels
    columns = NCOL(R)
    columnnames = colnames(R)

    even <- function (x) {
	x%%2 == 0
    }

    if(is.na(ylim[1])){
        ymax = max(R, na.rm = TRUE)
        ymin = min(R, na.rm = TRUE)
        ylim=c(ymin, ymax)
    }
    else{
        ymin=ylim[1]
        ymax=ylim[2]
    }
    startcol = 1
    if(!is.null(perpanel))
	endcol = perpanel
    else {
	endcol = columns
	perpanel = columns
    }
    panels = ceiling(columns/perpanel)
    for(panel in 1:panels) { # Loop for panels

	# mar: a numerical vector of the form c(bottom, left, top, right) which
	# gives the number of lines of margin to be specified on the four sides
	# of the plot. The default is c(5, 4, 4, 2) + 0.1
	op <- par(oma = c(5,0,4,0), mar=c(0,4,0,4))
	result <- list()
	if(show.yaxis == "none")
	    yaxis = FALSE
	else
	    yaxis=TRUE
	for(i in startcol:endcol){ # loop for columns
	    if(i==startcol)
		legend.loc="bottomleft"
	    else
		legend.loc=NULL
	    if(even(i) & show.yaxis=="alternating")
		yaxis.right=TRUE
	    else
		  yaxis.right=FALSE
	    if(i == 1) 
	      plot_object <- chart.BarVaR(R[,i,drop=FALSE], xaxis=xaxis, main=columnnames[i], ylab="", ylim = c(ymin,ymax), yaxis=yaxis, yaxis.right=yaxis.right, lwd=2, legend.loc=legend.loc,  ...)
	    else
	      plot_object <- chart.BarVaR(R[,i,drop=FALSE], xaxis=xaxis, main=columnnames[i], ylab="", ylim = c(ymin,ymax), yaxis=yaxis, yaxis.right=yaxis.right, lwd=2, legend.loc=legend.loc, add = TRUE,...)
	    
	# TODO: Add histogram at the end, turned on its side to line up with yaxis
	# chart.Histogram(R[,i,drop=FALSE], xlim=c(ymin,ymax), main="", axes=FALSE)
	    if((i==1 & show.yaxis == "firstonly") | show.yaxis == "none")
		yaxis=FALSE
	} # loop for columns
  print(plot_object)
	par(op)
	
	startcol = endcol+1
	endcol = min(endcol+perpanel, columns)
    } # Loop for panels

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