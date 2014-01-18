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

    layout(matrix(c(1:columns), ncol = 1, byrow = TRUE), widths=1)
    op <- par(oma = c(5,0,4,0), mar=c(0,4,0,4))
    xaxis=FALSE
    yaxis=TRUE
    for(i in 1:columns){
         if(even(i))
            yaxis.right=TRUE
         else
             yaxis.right=FALSE
        positives = R[,i,drop=FALSE]
        for(row in 1:length(R[,i,drop=FALSE])){ 
            positives[row,]=max(0,R[row,i])
        }
        negatives = R[,i,drop=FALSE]
        for(row in 1:length(R[,i,drop=FALSE])){ 
            negatives[row,]=min(0,R[row,i])
        }
        if(i==columns)
            xaxis = TRUE
        chart.TimeSeries(positives, type = "h", lend="butt", xaxis=xaxis, main="", ylab="", ylim = c(ymin,ymax), yaxis=yaxis, yaxis.right=yaxis.right, colorset="darkgreen", lwd=2, ...)
        lines(1:length(R[,1]), negatives, type="h", lend="butt", colorset="darkred", lwd=2)
        text(1, ymax, adj=c(0,1.2), cex = 0.8, labels = columnnames[i])

#         chart.Histogram(R[,i,drop=FALSE], xlim=c(ymin,ymax), main="", axes=FALSE)
        if(i==1)
            yaxis=FALSE
    }

    mtext(main,
        side = 3, outer = TRUE, 
        font = 2, cex = cex.main, line=1)
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
# $Id$
#
###############################################################################