#' box whiskers plot wrapper
#' 
#' A wrapper to create box and whiskers plot with some defaults useful for
#' comparing distributions.
#' 
#' We have also provided controls for all the symbols and lines in the chart.
#' One default, set by \code{as.Tufte=TRUE}, will strip chartjunk and draw a
#' Boxplot per recommendations by Edward Tufte. It can also be useful when
#' comparing several series to sort them in order of ascending or descending
#' "mean", "median", "variance" by use of \code{sort.by} and
#' \code{sort.ascending=TRUE}.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param names logical. if TRUE, show the names of each series
#' @param as.Tufte logical. default FALSE. if TRUE use method derived for Tufte
#' for limiting chartjunk
#' @param sort.ascending logical.  If TRUE sort the distributions by ascending
#' \code{sort.by}
#' @param sort.by one of "NULL", "mean", "median", "variance"
#' @param colorset color palette to use, set by default to rational choices
#' @param symbol.color draws the symbols described in
#' \code{mean.symbol},\code{median.symbol},\code{outlier.symbol} in the color
#' specified
#' @param mean.symbol symbol to use for the mean of the distribution
#' @param median.symbol symbol to use for the median of the distribution
#' @param outlier.symbol symbol to use for the outliers of the distribution
#' @param show.data numerical vector of column numbers to display on top of
#' boxplot, default NULL
#' @param add.mean logical. if TRUE, show a line for the mean of all
#' distributions plotted
#' @param xlab set the x-axis label, same as in \code{\link{plot}}
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param element.color specify the color of chart elements.  Default is
#' "darkgray"
#' @param \dots any other passthru parameters
#' @return box plot of returns
#' @author Peter Carl
#' @seealso \code{\link[graphics]{boxplot}}
#' @references Tufte, Edward R.  \emph{The Visual Display of Quantitative
#' Information}. Graphics Press. 1983. p. 124-129
###keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(edhec)
#' chart.Boxplot(edhec)
#' chart.Boxplot(edhec,as.Tufte=TRUE)
#' 
#' @export 
chart.Boxplot <-
function (R, names = TRUE, as.Tufte = FALSE, sort.by = c(NULL, "mean", "median", "variance"), colorset = "black", symbol.color = "red", mean.symbol = 1, median.symbol = "|", outlier.symbol = 1, show.data = NULL, add.mean = TRUE, sort.ascending = FALSE, xlab="Return", main = "Return Distribution Comparison", element.color = "darkgray", ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create box and whiskers plot, but with some sensible defaults
    # useful for comparing distributions.

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    R = checkData(R, method="data.frame")

    columns = ncol(R)
    rows = nrow(R)
    columnnames = colnames(R)

    column.order = NULL

    sort.by = sort.by[1]

    op <- par(no.readonly=TRUE)

    if(names){
        par(mar=c(5,12,4,2) + 0.1)
    }

    if(length(colorset) < columns)
        colorset = rep(colorset, length.out = columns)

    if(length(symbol.color) < columns)
        symbol.color = rep(symbol.color, length.out = columns)

    if(length(mean.symbol) < columns)
        mean.symbol = rep(mean.symbol, length.out = columns)

    means = sapply(R, mean, na.rm = TRUE)

    switch(sort.by,
        mean = {
            column.order = order(means)
            ylab = paste("Sorted by Mean", sep="")
        },
        median = {
            medians = sapply(R, median, na.rm = TRUE)
            column.order = order(medians)
            ylab = paste("Sorted by Median", sep="")
        },
        variance = {
            variances = sapply(R, var, na.rm = TRUE)
            column.order = order(variances)
            ylab = paste("Sorted by Variance", sep="")
        },
        {
            column.order = 1:columns
            ylab = paste("Unsorted", sep="")
        }
    ) # end switch

    if(as.Tufte){
        boxplot(R[,column.order], horizontal = TRUE, names = names, main = main, xlab = xlab, ylab = "", pars = list(boxcol = "white", medlty = "blank", medpch = median.symbol, medlwd = 2, medcex = .8, medcol = colorset[column.order], whisklty = c(1,1), whiskcol = colorset[column.order], staplelty = "blank", outpch = outlier.symbol, outcex = .5, outcol = colorset[column.order] ), axes = FALSE, ...)
    }
    else{
        boxplot(R[,column.order], horizontal = TRUE, names = names, main = main, xlab = xlab, ylab = "", pars = list(boxcol = colorset[column.order], medlwd = 1, medcol = colorset[column.order], whisklty = c(1,1), whiskcol = colorset[column.order], staplelty = 1, staplecol = colorset[column.order], staplecex = .5, outpch = outlier.symbol, outcex = .5, outcol = colorset[column.order] ), axes = FALSE, boxwex=.6, ...)
    } # end else

    if(!is.null(show.data)) {
      highlight.color=1:24
      for (item in show.data) {
        points(as.vector(R[item,column.order]), 1:columns, col=highlight.color[item]) #, pch = mean.symbol[column.order], col=symbol.color[column.order])
      }
    }
    if(add.mean)
        points(means[column.order], 1:columns, pch = mean.symbol[column.order], col=symbol.color[column.order])

    if(names){
        labels = columnnames
        axis(2, cex.axis = 0.8, col = element.color, labels = labels[column.order], at = 1:columns, las = 1)
    }
    else{
        labels = ""
        axis(2, cex.axis = 0.8, col = element.color, labels = labels[column.order], at = 1:columns, las = 1, tick = FALSE)
    }
    axis(1, cex.axis = 0.8, col = element.color)
    

#     if(names)
#         title(sub=ylab)
#     else
#         title(sub=ylab)
    box(col=element.color)

    abline(v=0, lty="solid",col=element.color)

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
