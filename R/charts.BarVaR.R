`charts.BarVaR` <-
function (R,  space = 0, main = "Returns", cex.legend = 0.8, colorset=1:12, ...)
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
    layout(matrix(c(1:columns), nc = 1, byrow = TRUE), width=1)
    xaxis=FALSE
    yaxis=TRUE
    for(i in 1:columns){
         if(even(i))
            yaxis.right=TRUE
         else
             yaxis.right=FALSE
        if(i==columns)
            xaxis = TRUE
        chart.BarVaR(R[,i,drop=FALSE], xaxis=xaxis, main="", ylab="", ylim = c(ymin,ymax), yaxis=yaxis, yaxis.right=yaxis.right, col=colorset[i], lwd=2, ...)
        text(1, ymax, adj=c(0.5,1.2), cex = 0.8, labels = columnnames[i])

#         chart.Histogram(R[,i,drop=FALSE], xlim=c(ymin,ymax), main="", axes=FALSE)
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
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: charts.BarVaR.R,v $
# Revision 1.1  2009-09-17 03:48:40  peter
# - first commit of function
#
# Revision 1.1  2009-08-18 21:24:00  peter
# - multiple bar plots function
#
# Revision 1.2  2008-04-18 03:59:52  peter
# - added na.omit to avoid problems with missing data
#
# Revision 1.1  2008/02/23 05:55:21  peter
# - chart demonstrating fund exposures through time
#
#
###############################################################################
