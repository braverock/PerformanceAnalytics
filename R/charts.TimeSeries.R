#' @rdname chart.TimeSeries
#' @export
charts.TimeSeries <-
function (R,  space = 0, main = "Returns", ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a small multiples of time series bar charts 

    # FUNCTION:

    # Transform input data to a data frame
    R = checkData(R)

    # Get dimensions and labels
    columns = NCOL(R)
    columnnames = colnames(R)

# Create a page of bar chart panels (using chart.Bar) that are in a single column.
# Option to add a "total" exposure chart, which shows the positive and negative "gross" exposures in bars
# and the net exposure in an overlaid line
# Option to add an "R.squared" panel that shows the R-squared value through time in a line chart

# Establish common ylim across the bar charts
# Title at the top of the page
# x-axis in the bottom bar chart and the optional charts? or just in the bottom chart
# each bar chart title labeled with the factor name, left justified
# each panel may have a different color from colorset, but default set to the same color across all
    ymax = max(R, na.rm=TRUE)
    ymin = min(R, na.rm=TRUE)
    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1
    op <- par(oma = c(2,0,4,0), mar=c(0,4,0,4))
    layout(matrix(c(1:columns), ncol = 1, byrow = TRUE), widths=1)
    xaxis=FALSE
    yaxis=TRUE
    for(i in 1:columns){
         if(even(i))
            yaxis.right=TRUE
         else
             yaxis.right=FALSE
        if(i==columns)
            xaxis = TRUE
        chart.TimeSeries(R[,i,drop=FALSE],  xaxis=xaxis, main="", ylab=colnames(R)[i], ylim = c(ymin,ymax), yaxis=yaxis, yaxis.right=yaxis.right, ...)
        if(i==1)
            yaxis=FALSE
    }

    mtext(main,
        side = 3, outer = TRUE, 
        font = 2, cex = 1.2, line=1)
    par(op)
    

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