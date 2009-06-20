`chart.StackedBar` <- 
function (w, colorset = NULL, space = 0.2, cex.axis=0.8, cex.legend = 0.8, cex.lab = 1, cex.labels = 0.8, cex.main = 1, xaxis=TRUE, legend.loc="under",  element.color = "darkgray", unstacked = TRUE, xlab="Date", ylab="Value", ylim=NULL, date.format = "%m/%y", major.ticks='auto', minor.ticks=TRUE, las = 0, xaxis.labels = NULL, ... ) 
{
    # Data should be organized as columns for each category, rows for each period or observation

    # @todo: Set axis color to element.color
    # @todo: Set border color to element.color

    w = checkData(w)
    w.columns = ncol(w)
    w.rows = nrow(w)

    time.scale = periodicity(w)$scale
    ep = axTicksByTime(w, major.ticks, format.labels = date.format)
    ep1 = ep
    posn = barplot(w, plot=FALSE)
    for(i in 1:length(ep)) 
        ep1[i] = posn[ep[i]]

    if(is.null(colorset))
        colorset=1:w.columns

    if(is.null(xlab))
        minmargin = 3
    else
        minmargin = 5

    if(unstacked & dim(w)[1] == 1){ # only one row is being passed into 'w', so we'll unstack the bars
        if(las > 1) {# set the bottom border to accomodate labels
            # mar: a numerical vector of the form c(bottom, left, top, right) which
            # gives the number of lines of margin to be specified on the four sides
            # of the plot. The default is c(5, 4, 4, 2) + 0.1

            # Kludgy, but it works.  Converts the length of the string into "lines" used by par("mar")
            # by dividing the length of the string in inches by what the plot uses as the height of 
            # a character by default, par("cin").  That gives a value that seems to work in most cases.
            # When that value is just plain too small, this calc provides a minimum value of "5", or
            # the usual default for the bottom margin.
            bottommargin = max(c(minmargin, (strwidth(colnames(w),units="in"))/par("cin")[1])) * cex.names

            par(mar = c(bottommargin, 4, 4, 2) +.1)
        }
# par(mai=c(max(strwidth(colnames(w), units="in")), 0.82, 0.82, 0.42)*cex.names)
        barplot(w, col = colorset[1], las = las, horiz = FALSE, space = space, xlab = xlab, cex.names = cex.names, axes = FALSE, ylim=ylim, ...)
        axis(2, col = element.color, las = las, cex.axis = cex.axis)

    }

    else { # multiple columns being passed into 'w', so we'll stack the bars and put a legend underneith
#         op <- par(no.readonly=TRUE)
        if(!is.null(legend.loc) ){
            if(legend.loc =="under") {# put the legend under the chart
                op <- par(no.readonly=TRUE)
                layout(rbind(1,2), height=c(6,1), width=1)
            }
#             else
#                 par(mar=c(5,4,4,2)+.1) # @todo: this area may be used for other locations later
        }

        # Brute force solution for plotting negative values in the bar charts:
        positives = w
        for(column in 1:ncol(w)){
            for(row in 1:nrow(w)){ 
                positives[row,column]=max(0,w[row,column])
            }
        }

        negatives = w
        for(column in 1:ncol(w)){
            for(row in 1:nrow(w)){ 
                negatives[row,column]=min(0,w[row,column])
            }
        }
        # Set ylim accordingly
        if(is.null(ylim)){
            ymax=max(0,apply(positives,FUN=sum,MARGIN=1))
            ymin=min(0,apply(negatives,FUN=sum,MARGIN=1))
            ylim=c(ymin,ymax)
        }

        barplot(t(positives), col=colorset, space=space, axisnames = FALSE, axes = FALSE, ylim=ylim, ...)
        barplot(t(negatives), add=TRUE , col=colorset, space=space, las = las, xlab = xlab, cex.names = cex.names, axes = FALSE, axisnames = FALSE, ylim=ylim, ...)
        axis(2, col = element.color, las = las, cex.axis = cex.axis)
        title(ylab = ylab, cex = cex.lab)
        if (xaxis) {
            if(minor.ticks)
                axis(1, at=posn, labels=FALSE, col='#BBBBBB')
            label.height = .25 + cex.axis * apply(t(names(ep1)),1, function(X) max(strheight(X, units="in")/par('cin')[2]) )
            if(is.null(xaxis.labels))
                xaxis.labels = names(ep1)
            else
                ep1 = 1:length(xaxis.labels)
            axis(1, at=ep1, labels=xaxis.labels, las=las, lwd=1, mgp=c(3,label.height,0), cex.axis = cex.axis) 
            #axis(1, at = lab.ind, lab=rownames[lab.ind], cex.axis = cex.axis, col = elementcolor)
            title(xlab = xlab, cex = cex.lab)
            # use axis(..., las=3) for vertical labels.
        }
        box(col = element.color)

        if(!is.null(legend.loc)){
            if(legend.loc =="under"){ # draw the legend under the chart
                par(mar=c(0,2,0,1)+.1) # set the margins of the second panel
                plot.new()
                if(w.columns <4)
                    ncol= w.columns
#                 else if(w.columns/2 < 4)
#                     ncol = w.columns/2
                else
                    ncol = 4
                legend("center", legend=colnames(w), cex = cex.legend, fill=colorset, ncol=ncol,
 box.col=element.color, border.col = element.color)
                par(op)
            } # if legend.loc is null, then do nothing
        }
#     par(op)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.StackedBar.R,v 1.14 2009-06-20 03:18:04 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.13  2008-10-14 14:37:29  brian
# - convert from matrix or data.frame to zoo in checkData call
#
# Revision 1.12  2008-07-11 02:42:15  peter
# - adjustments to margins again
# - added cex.axis for sizing axis text
#
# Revision 1.5  2008-04-18 03:56:15  peter
# - added a legend at the bottom
# - added smarts for displaying single column or stacked
# - made bottom margin sensitive to length of label names
#
# Revision 1.4  2008/02/27 04:01:10  peter
# - added cex.names for sizing xaxis tags
#
# Revision 1.3  2008/02/26 04:56:59  peter
# - fixed label calculation to handle correct dimension
#
# Revision 1.2  2008/02/26 04:38:53  peter
# - now handles multiple columns for fund
# - legend "under" draws correctly
# - bottom margin fits to text with cex=1
#
# Revision 1.1  2008/02/23 05:54:37  peter
# - primitive for weight displays and other charts
#
###############################################################################