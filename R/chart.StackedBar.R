#' create a stacked bar plot
#' 
#' This creates a stacked column chart with time on the horizontal axis and
#' values in categories.  This kind of chart is commonly used for showing
#' portfolio 'weights' through time, although the function will plot any values
#' by category.
#' 
#' This function is a wrapper for \code{\link{barplot}} but adds three
#' additional capabilities.  First, it calculates and sets a bottom margin for
#' long column names that are rotated vertically.  That doesn't always result
#' in the prettiest chart, but it does ensure readable labels.
#' 
#' Second, it places a legend "under" the graph rather than within the bounds
#' of the chart (which would obscure the data).  The legend is created from the
#' column names.  The default is to create the legend when there's more than
#' one row of data being presented.  If there is one row of data, the chart may
#' be "unstacked" and the legend removed.
#' 
#' Third, it plots or stacks negative values from an origin of zero, similar to
#' the behavior of \code{\link[lattice]{barchart}} from the 'lattice' package.
#' 
#' @param w a matrix, data frame or zoo object of values to be plotted.
#' Rownames should contain dates or period labels; column names should indicate
#' categories.  See examples for detail.
#' @param colorset color palette to use, set by default to rational choices
#' @param space the amount of space (as a fraction of the average bar width)
#' left before each bar, as in \code{\link{barplot}}. Default is 0.2.
#' @param cex.legend The magnification to be used for sizing the legend
#' relative to the current setting of 'cex', similar to \code{\link{plot}}.
#' @param cex.labels The magnification to be used for event line labels
#' relative to the current setting of 'cex'.
#' @param cex.lab The magnification to be used for x- and y-axis labels
#' relative to the current setting of 'cex'.
#' @param cex.main The magnification to be used for the chart title relative to
#' the current setting of 'cex'.
#' @param xaxis If true, draws the x axis
#' @param ylab Set the y-axis label, same as in \code{\link{plot}}
#' @param date.format Re-format the dates for the xaxis; the default is "\%m/\%y"
#' @param major.ticks Should major tickmarks be drawn and labeled, default
#' 'auto'
#' @param minor.ticks Should minor tickmarks be drawn, default TRUE
#' @param xaxis.labels Allows for non-date labeling of date axes, default is
#' NULL
#' @param cex.axis The magnification to be used for sizing the axis text
#' relative to the current setting of 'cex', similar to \code{\link{plot}}.
#' @param las sets the orientation of the axis labels, as described in
#' \code{\link{par}}.  Defaults to '3'.
#' @param legend.loc places a legend into a location on the chart similar to
#' \code{\link{chart.TimeSeries}}. The default, "under," is the only location
#' currently implemented for this chart.  Use 'NULL' to remove the legend.
#' @param element.color provides the color for drawing less-important chart
#' elements, such as the box lines, axis lines, etc.
#' @param unstacked logical.  If set to 'TRUE' \emph{and} only one row of data
#' is submitted in 'w', then the chart creates a normal column chart.  If more
#' than one row is submitted, then this is ignored.  See examples below.
#' @param xlab the x-axis label, which defaults to 'NULL'.
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param \dots arguments to be passed to \code{\link{barplot}}.
#' @note The 'w' attribute is so named because this is a popular way to show
#' portfolio weights through time.  That being said, this function isn't
#' limited to any particular values and doesn't provide any normalization, so
#' that the chart can be used more generally.
#' 
#' The principal drawback of stacked column charts is that it is very difficult
#' for the reader to judge size of 2nd, 3rd, etc., data series because they do
#' not have common baseline.  Another is that with a large number of series,
#' the colors may be difficult to discern.  As alternatives, Cleveland
#' advocates the use of trellis like displays, and Tufte advocates the use of
#' small multiple charts.
#' @author Peter Carl
#' @seealso \code{\link{barplot}}, \code{\link{par}}
#' @references Cleveland, W.S. (1994), The Elements of Graphing Data, Summit,
#' NJ: Hobart Press.
#' 
#' Tufte, Edward R. (2001) The Visual Display of Quantitative Information, 2nd
#' edition. The Graphics Press, Cheshire, Connecticut. See
#' http://www.edwardtufte.com for this and other references.
#' @keywords ts multivariate hplot
#' @examples
#' 
#' data(weights)
#' head(weights)
#' 
#' # With the legend "under" the chart
#' chart.StackedBar(weights, date.format="\%Y", cex.legend = 0.7, colorset=rainbow12equal)
#' 
#' # Without the legend
#' chart.StackedBar(weights, colorset=rainbow12equal, legend.loc=NULL)
#' 
#' # for one row of data, use 'unstacked' for a better chart
#' chart.StackedBar(weights[1,,drop=FALSE], unstacked=TRUE, las=3)
#' 
#' @export 
chart.StackedBar <- 
function (w, colorset = NULL, space = 0.2, cex.axis=0.8, cex.legend = 0.8, cex.lab = 1, cex.labels = 0.8, cex.main = 1, xaxis=TRUE, legend.loc="under",  element.color = "darkgray", unstacked = TRUE, xlab="Date", ylab="Value", ylim=NULL, date.format = "%b %y", major.ticks='auto', minor.ticks=TRUE, las = 0, xaxis.labels = NULL, ... ) 
{
#     op <- par(no.readonly=TRUE)
#     p <- median(diff(.index(w)))
#     if(is.null(p)) p=NA
    if(xtsible(w) & dim(w)[1]>1)# & is.null(p))# !is.na(p))
        chart.StackedBar.xts(w, colorset = colorset, space = space, cex.axis=cex.axis, cex.legend = cex.legend, cex.lab = cex.lab, cex.labels = cex.labels, cex.main = cex.main, xaxis=xaxis, legend.loc=legend.loc,  element.color = element.color, unstacked = unstacked, xlab=xlab, ylab=ylab, ylim=ylim, date.format = date.format, major.ticks=major.ticks, minor.ticks=minor.ticks, las = las, xaxis.labels = xaxis.labels, ... )
    else
        chart.StackedBar.matrix(w, colorset = colorset, space = space, cex.axis=cex.axis, cex.legend = cex.legend, cex.lab = cex.lab, cex.labels = cex.labels, cex.main = cex.main, xaxis=xaxis, legend.loc=legend.loc,  element.color = element.color, unstacked = unstacked, xlab=xlab, ylab=ylab, ylim=ylim, date.format = date.format, major.ticks=major.ticks, minor.ticks=minor.ticks, las = las, xaxis.labels = xaxis.labels, ... )
#     par(op)
}

chart.StackedBar.xts <- 
function (w, colorset = NULL, space = 0.2, cex.axis=0.8, cex.legend = 0.8, cex.lab = 1, cex.labels = 0.8, cex.main = 1, xaxis=TRUE, legend.loc="under",  element.color = "darkgray", unstacked = TRUE, xlab="Date", ylab="Value", ylim=NULL, date.format = "%b %y", major.ticks='auto', minor.ticks=TRUE, las = 0, xaxis.labels = NULL, ... ) 
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
    posn = barplot(w, plot=FALSE, space=space)
    for(i in 1:length(ep)) 
        ep1[i] = posn[ep[i]]

    if(is.null(colorset))
        colorset=1:w.columns

    if(is.null(xlab))
        minmargin = 3
    else
        minmargin = 5

    # multiple columns being passed into 'w', so we'll stack the bars and put a legend underneith
    if(!is.null(legend.loc) ){
        if(legend.loc =="under") {# put the legend under the chart
            op <- par(no.readonly=TRUE)
            layout(rbind(1,2), heights=c(6,1), widths=1)
            par(mar=c(3,4,4,2)+.1) # set the margins of the first panel
# c(bottom, left, top, right)
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
    barplot(t(negatives), add=TRUE , col=colorset, space=space, las = las, xlab = xlab, cex.names = cex.lab, axes = FALSE, axisnames = FALSE, ylim=ylim, ...)
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
#             title(xlab = xlab, cex = cex.lab)
        # use axis(..., las=3) for vertical labels.
    }
    box(col = element.color)

    if(!is.null(legend.loc)){
        if(legend.loc =="under"){ # draw the legend under the chart
            par(mar=c(0,2,0,1)+.1) # set the margins of the second panel
            plot.new()
            if(w.columns <4)
                ncol= w.columns
            else
                ncol = 4
            legend("center", legend=colnames(w), cex = cex.legend, fill=colorset, ncol=ncol, box.col=element.color, border.col = element.color)
            par(op)
        } # if legend.loc is null, then do nothing
    }
#     par(op)
}

### NOT A TIME SERIES OBJECT

chart.StackedBar.matrix <- 
function (w, colorset = NULL, space = 0.2, cex.axis=0.8, cex.legend = 0.8, cex.lab = 1, cex.labels = 0.8, cex.main = 1, xaxis=TRUE, legend.loc="under",  element.color = "darkgray", unstacked = TRUE, xlab="Date", ylab="Value", ylim=NULL, date.format = "%b %y", major.ticks='auto', minor.ticks=TRUE, las = 0, xaxis.labels = NULL, ... ) 
{
    # Data should be organized as columns for each category, rows for each period or observation

    w = checkData(w, method="matrix")
    w.columns = NCOL(w)
    w.rows = NROW(w)

    posn = barplot(t(w), plot=FALSE, space=space)

    if(is.null(colnames(w)))
        legend.loc = NULL

    if(is.null(colorset))
        colorset=1:w.columns

    if(is.null(xlab))
        minmargin = 3
    else
        minmargin = 5

    if(unstacked & dim(w)[1] == 1){ # only one row is being passed into 'w', so we'll unstack the bars
        if(las > 1) {# set the bottom border to accomodate labels
            bottommargin = max(c(minmargin, (strwidth(colnames(w),units="in"))/par("cin")[1])) * cex.lab
            par(mar = c(bottommargin, 4, 4, 2) +.1)
        }
        barplot(w, col = colorset[1], las = las, horiz = FALSE, space = space, xlab = "", cex.names = cex.lab, axes = FALSE, ylim=ylim, ...)
        axis(2, col = element.color, las = las, cex.axis = cex.axis)
        box(col = element.color)
    }
    else { # multiple columns being passed into 'w', so we'll stack the bars and put a legend underneith
        if(!is.null(legend.loc) ){
            if(legend.loc =="under") {# put the legend under the chart
                op <- par(no.readonly=TRUE)
                layout(rbind(1,2), heights=c(6,1), widths=1)
                par(mar=c(3,4,4,2)+.1) # set the margins of the first panel
            }
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

        barplot(t(positives), col=colorset, space=space, axisnames = FALSE, axes = FALSE, ylim=ylim, xlab="", ...)
        barplot(t(negatives), add=TRUE , col=colorset, space=space, las = las, xlab = "", cex.names = cex.lab, axes = FALSE, axisnames = FALSE, ylim=ylim, ...)
        axis(2, col = element.color, las = las, cex.axis = cex.axis)
        title(ylab = ylab, cex = cex.lab)
        if (xaxis) {
            label.height = .25 + cex.axis * max(strheight(rownames(w), units="in")/par('cin')[2])
            if(is.null(xaxis.labels))
                xaxis.labels = rownames(w)
            axis(1, at=posn, labels=xaxis.labels, las=las, lwd=1, mgp=c(3,label.height,0), cex.axis = cex.axis) #at=1:length(xaxis.labels)
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
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
