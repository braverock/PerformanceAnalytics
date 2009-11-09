`chart.Regression` <-
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
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
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
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.11  2009-10-02 18:53:17  peter
# - changed checkData to use xts
# - changed default Rf to 0
#
# Revision 1.10  2009-04-07 22:22:53  peter
# - uses element.color parameter
#
# Revision 1.9  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.8  2008-04-18 03:38:50  peter
# - fixed x-axis
# - added method for conditional beta lines
# - added notes for extending fct to gauging market timing
#
# Revision 1.7  2007/12/29 15:38:06  peter
# - fixed switch statement
#
# Revision 1.6  2007/12/27 20:41:43  brian
# - fix switch syntax error
#
# Revision 1.5  2007/12/27 20:36:39  brian
# - change fit to use a list and a switch
#
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