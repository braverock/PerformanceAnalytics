`chart.Regression` <-
function (Ra, Rb, Rf, excess.returns = FALSE, reference.grid = TRUE, main = "Title", ylab=NULL, xlab=NULL, xlim = NA, colorset = 1:12, symbolset = 1:12, darken = FALSE , legend.loc = NULL, ylog = FALSE, fit = c("loess", "linear", "spline"), span = 2/3, degree = 1, family = c("symmetric", "gaussian"),  ylim = NA, evaluation = 50, cex= .8, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # Draws a scatter chart.  This is another chart "primitive", since it
    # only contains a set of sensible defaults.  This function is
    # intended to be used in a wrapper that is written for a particular purpose.
    # This is just a handy way to standardize the formatting of multiple charts.

    # Inputs:
    # x and y = assumes that data is a regular time series, not irregular.  Can take
    # any type of object, whether a matrix, data frame, or timeSeries.
    # legend.loc = use this to locate the legend, e.g., "topright"
    # colorset = use the name of any of the palattes above
    # reference.grid = if true, draws a grid aligned with the points on the
    #    x and y axes.
    # darken = if true, draws the chart elements in "darkgray" rather than
    #    "gray".  Makes it easier to print for some printers.


    # All other inputs are the same as "plot" and are principally included
    # so that some sensible defaults could be set.

    # Output:
    # Draws a scatter chart with some sensible defaults.

    # FUNCTION:


    # Transform input data to a data frame
    Ra = checkData(Ra, method = "zoo")
    Rb = checkData(Rb, method = "zoo")
    if(!is.null(dim(Rf))){
        Rf = checkData(Rf, method = "zoo")
    }
    if(excess.returns){
        Ra = Return.excess(Ra, Rf)
        Rb = Return.excess(Rb, Rf)
    }

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)
    legendnames = NULL

    if(is.na(xlim[1]))
        xlim = range(as.vector(na.omit(Rb)))
    if(is.na(ylim[1]))
        ylim = range(as.vector(na.omit(Ra))) #range(Ra, r.loess$y, na.rm = TRUE)
    if(is.null(xlab)){
        if(excess.returns)
            xlab = "Excess Return of Benchmarks"
        else
            xlab = "Return of Benchmarks"
    }
    if(is.null(ylab)){
        if(excess.returns)
            ylab = "Excess Return of Assets"
        else
            ylab = "Return of Assets"
    }

    if(darken)
        elementcolor = "darkgray" #better for the printer
    else
        elementcolor = "lightgray" #better for the screen

    # Calculate
    color.tic = 0
    for(column.a in 1:columns.a) { # for each asset passed in as R
    color.tic = color.tic + 1
        for(column.b in 1:columns.b) { # against each asset passed in as Rb
            merged.assets = merge(Ra[,column.a,drop=FALSE], Rb[,column.b,drop=FALSE])
            merged.assets.df = as.data.frame(na.omit(merged.assets))


            if(column.a == 1 & column.b == 1){
                plot(merged.assets.df[,2], merged.assets.df[,1], col = colorset[color.tic], pch = symbolset[color.tic], xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main, axes = FALSE, cex = cex, ...)
            }
            else {
                plot.xy(merged.assets.df[,c(2,1)], type="p", col = colorset[color.tic], pch = symbolset[color.tic], cex = cex,  ...)

            }
           for (s_fit in fit) {
              switch(s_fit,
                    linear = {
                        abline(lm( merged.assets.df[,1] ~ merged.assets.df[,2]), col = colorset[color.tic])
                    }
                    loess = { # see scatter.smooth
                        #r.loess = 0
                        r.loess = loess.smooth(merged.assets.df[,2], merged.assets.df[,1], span, degree, family, evaluation)
                        lines(r.loess, col = colorset[color.tic], lty = 2)
                        #lines(loess(merged.assets.df[,2] ~ merged.assets.df[,1]), col = colorset[color.tic])
                    }
                    # commented because it is similar to loess in default parameters
                    # spline = {
                    #    # requires library(pspline)
                    #    r.spline = sm.spline(merged.assets.df[,2], merged.assets.df[,1], df=4)
                    #    lines(r.spline, col = colorset[color.tic], lty = 4)
                    #    #lines(loess(merged.assets.df[,2] ~ merged.assets.df[,1]), col = colorset[color.tic])
                    #}
               )
           } # end fit

            legendnames= c(legendnames, paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to "))
            if (column.b != columns.b)
                color.tic = color.tic + 1
        }
    if(reference.grid) {
        grid(col = elementcolor)
        abline(h = 0, col = elementcolor)
        abline(v = 0, col = elementcolor)
    }
    axis(1, col = elementcolor, ...)
    axis(2, col = elementcolor, ...)
    box(col = elementcolor)

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, pch = symbolset, cex=cex, border.col = elementcolor, lwd = 1, bg = "white", legend = legendnames)
    }

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
# $Id: chart.Regression.R,v 1.5 2007-12-27 20:36:39 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2007/12/27 19:03:42  brian
# - change function name from chart.MultiScatter to chart.Regression in prep for public release
#
# Revision 1.3  2007/11/19 03:38:07  peter
# - allow cex to be passed through correctly
#
# Revision 1.2  2007/09/26 03:13:29  peter
# - added a spline fit
# - switched the order of the lowess fit variables
#
# Revision 1.1  2007/04/23 14:35:22  peter
# - scatterplot to handle multiple columns of data
#
#
###############################################################################