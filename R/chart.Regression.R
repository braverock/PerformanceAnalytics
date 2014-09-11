#' Takes a set of returns and relates them to a market benchmark in a
#' scatterplot
#' 
#' Uses a scatterplot to display the relationship of a set of returns to a
#' market benchmark.  Fits a linear model and overlays the resulting model.
#' Also overlays a Loess line for comparison.
#' 
#' 
#' @param Ra a vector of returns to test, e.g., the asset to be examined
#' @param Rb a matrix, data.frame, or timeSeries of benchmark(s) to test the
#' asset against
#' @param Rf risk free rate, in same period as the returns
#' @param excess.returns logical; should excess returns be used?
#' @param reference.grid if true, draws a grid aligned with the points on the x
#' and y axes
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param ylab set the y-axis title, same as in \code{\link{plot}}
#' @param xlab set the x-axis title, same as in \code{\link{plot}}
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}
#' @param colorset color palette to use
#' @param symbolset symbols to use, see also 'pch' in \code{\link{plot}}
#' @param element.color provides the color for drawing chart elements, such as
#' the box lines, axis lines, etc. Default is "darkgray"
#' @param legend.loc places a legend into one of nine locations on the chart:
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#' center.
#' @param ylog Not used
#' @param fit for values of "loess", "linear", or "conditional", plots a line
#' to fit the data.  Conditional lines are drawn separately for positive and
#' negative benchmark returns.  "Quadratic" is not yet implemented.
#' @param span passed to loess line fit, as in \code{\link{loess.smooth}}
#' @param degree passed to loess line fit, as in \code{\link{loess.smooth}}
#' @param family passed to loess line fit, as in \code{\link{loess.smooth}}
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param evaluation passed to loess line fit, as in \code{\link{loess.smooth}}
#' @param cex set the cex size, same as in \code{\link{plot}}
#' @param legend.cex set the legend size
#' @param lwd set the line width for fits, same as in \code{\link{lines}}
#' @param \dots any other passthru parameters to \code{\link{plot}}
#' @author Peter Carl
#' @seealso \code{\link{plot}}
#' @references Chapter 7 of Ruppert(2004) gives an extensive overview of CAPM,
#' its assumptions and deficiencies.
###keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(managers)
#' chart.Regression(managers[, 1:2, drop = FALSE], 
#' 		managers[, 8, drop = FALSE], 
#' 		Rf = managers[, 10, drop = FALSE], 
#' 		excess.returns = TRUE, fit = c("loess", "linear"), 
#' 		legend.loc = "topleft")
#' 
#' @export 
chart.Regression <-
function (Ra, Rb, Rf = 0, excess.returns = FALSE, reference.grid = TRUE, main = "Title", ylab=NULL, xlab=NULL, xlim = NA, colorset = 1:12, symbolset = 1:12, element.color = "darkgray", legend.loc = NULL, ylog = FALSE, fit = c("loess", "linear", "conditional", "quadratic"), span = 2/3, degree = 1, family = c("symmetric", "gaussian"),  ylim = NA, evaluation = 50, legend.cex= 0.8, cex = 0.8, lwd = 2, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # Draws a scatter chart.  

    # Inputs:
    # x and y = assumes that data is a regular time series, not irregular.  Can take
    # any type of object, whether a matrix, data frame, or timeSeries.
    # legend.loc = use this to locate the legend, e.g., "topright"
    # colorset = use the name of any of the palattes above
    # reference.grid = if true, draws a grid aligned with the points on the
    #    x and y axes.

    # All other inputs are the same as "plot" and are principally included
    # so that some sensible defaults could be set.

    # Output:
    # Draws a scatter chart with some sensible defaults.

# @todo: Add a quadratic curve fit
    # Because there appears to be a bit of a bend in the scatterplot, let's try fitting a quadratic curve instead of a linear curve. Note: Fitting a quadratic curve is still considered linear regression. This may seem strange, but the reason is that the quadratic regression model assumes that the response y is a linear combination of 1, x, and x2. Notice the special form of the lm command when we implement quadratic regression. The I function means "as is" and it resolves any ambiguity in the model formula:
    # 
    #    model2 <- lm(y~x+I(x^2))
    #    summary(model2)
    # 
    # Here is how to find the estimates of beta using the closed-form solution:
    # 
    #    X <- cbind(1, x, x^2) # Create nx3 X matrix
    #    solve(t(X) %*% X) %*% t(X) %*% y # Compare to the coefficients above
    # 
    # Plotting the quadratic curve is not a simple matter of using the abline function. To obtain the plot, we'll first create a sequence of x values, then apply the linear combination implied by the regression model using matrix multiplication:
    # 
    #    xx <- seq(min(x),max(x),len=200)
    #    yy <- model2$coef %*% rbind(1,xx,xx^2)
    #    lines(xx,yy,lwd=2,col=3)

    # FUNCTION:


    # Transform input data to a data frame
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf))){
        Rf = checkData(Rf)
    }
    if(excess.returns){
        Ra = Return.excess(Ra, Rf)
        Rb = Return.excess(Rb, Rf)
    }

    # Get dimensions and labels
    columns.a = NCOL(Ra)
    columns.b = NCOL(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)
    legendnames = NULL

    if(is.na(xlim[1]))
        xlim = range(as.vector(na.omit(Rb)))
    xmin = xlim[1]
    xmax = xlim[2]
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
                        abline(lm( merged.assets.df[,1] ~ merged.assets.df[,2]), col = colorset[color.tic], lwd = lwd)
                    },
                    loess = { # see scatter.smooth
                        #r.loess = 0
                        r.loess = loess.smooth(merged.assets.df[,2], merged.assets.df[,1], span, degree, family, evaluation)
                        lines(r.loess, col = colorset[color.tic], lty = 2)
                        #lines(loess(merged.assets.df[,2] ~ merged.assets.df[,1]), col = colorset[color.tic])
                    },
                    # commented because it is similar to loess in default parameters
                    # spline = {
                    #    # requires library(pspline)
                    #    r.spline = sm.spline(merged.assets.df[,2], merged.assets.df[,1], df=4)
                    #    lines(r.spline, col = colorset[color.tic], lty = 4)
                    #    #lines(loess(merged.assets.df[,2] ~ merged.assets.df[,1]), col = colorset[color.tic])
                    #}
                    conditional = { # up/down market linear fit for conditional beta
                    # @todo: remove from default action
                        bull.lm = lm( merged.assets.df[,1] ~ merged.assets.df[,2], subset = (merged.assets.df[,2] >0))
                        bear.lm = lm( merged.assets.df[,1] ~ merged.assets.df[,2], subset = (merged.assets.df[,2] <0))
                        segments(xmin, bear.lm$coef[1] + bear.lm$coef[2]*xmin, 0, bear.lm$coef[1], col = colorset[color.tic], lwd=lwd)
                        segments(xmax, bull.lm$coef[1] + bull.lm$coef[2]*xmax, 0, bull.lm$coef[1], col = colorset[color.tic], lwd=lwd)
                    },
                    quadratic = { 
                        model2.lm = lm( merged.assets.df[,1] ~ merged.assets.df[,2] + I(merged.assets.df[,2]^2))
                        curve(predict(model2.lm, newdata = merged.assets.df[,2]), add = TRUE, col = "red")

                    }
               )
           } # end fit

            legendnames= c(legendnames, paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to "))
            if (column.b != columns.b)
                color.tic = color.tic + 1
        }
    if(reference.grid) {
        grid(col = element.color)
        abline(h = 0, col = element.color)
        abline(v = 0, col = element.color)
    }
    axis(1, col = element.color, ...)
    axis(2, col = element.color, ...)
    box(col = element.color)

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, pch = symbolset, cex=legend.cex, border.col = element.color, lwd = 1, bg = "white", legend = legendnames)
    }

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
