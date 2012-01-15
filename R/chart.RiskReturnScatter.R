chart.RiskReturnScatter <-
function (R, Rf = 0, main = "Annualized Return and Risk", add.names = TRUE, xlab = "Annualized Risk", ylab = "Annualized Return", method = "calc", geometric = TRUE, scale = NA, add.sharpe = c(1,2,3), add.boxplots = FALSE, colorset = 1, symbolset = 1, element.color = "darkgray", legend.loc = NULL, xlim = NULL, ylim = NULL, cex.legend = 1, cex.axis = 0.8, cex.main = 1, cex.lab = 1, ...)
{ # @author Peter Carl

    # DESCRIPTION:

    # A wrapper to create a scatter chart of annualized returns versus
    # annualized risk (standard deviation) for comparing manager peRformance.
    # Also puts a box plot into the margins to help identify the relative
    # peRformance quartile.

    # Inputs:
    # R = usually a set of monthly return, but can also be a pre-calculated
    #   return and risk measure (set method = "nocalc", see below).  If the input
    #   is a set of monthly peRformance returns, this function will calculate
    #   the appropriate return and risk summary according to the method flag.
    # method = If method is set to "nocalc" then we assume that R is a column of
    #   return and a column of risk (e.g., annualized returns, annualized risk),
    #   in that order.  Other method cases can be set for different risk/return
    #   calculations.
    # Rf = this is the risk free rate.  Remember to set this to the same
    #   periodicity as the data being passed in.
    # add.sharpe = this draws a Sharpe ratio line that indicates Sharpe ratio
    #   levels.  Lines are drawn with a y-intercept of the risk free rate and
    #   the slope of the appropriate Sharpe ratio level.  Lines should be
    #   removed where not appropriate (e.g., sharpe.ratio = NULL).
    # add.names = plots the row name with the data point.  Can be removed with
    #   by setting it to NULL.
    # add.boxplots = adds a boxplot summary of the data on the axis.

    # Code inspired by a chart on:
    # http://zoonek2.free.fr/UNIX/48_R/03.html

    if (method == "calc")  x = checkData(R, method = "zoo")
    else x=t(R)
    
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf, method = "zoo")

    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = rownames(x)

    # @todo: strip out basic elements to a scatter plot wrapper
    if(length(colorset) < columns)
        colorset = rep(colorset, length.out = columns)

    if(length(symbolset) < columns)
        symbolset = rep(symbolset, length.out = columns)

    # @todo: add flags to use other risk measures

    if(method == "calc"){
        # Assume we're passed in a series of monthly returns.  First, we'll
        # annualized returns and risk
        comparison = t(table.AnnualizedReturns(x[,columns:1], Rf = Rf, geometric = geometric, scale = scale))

        returns = comparison[,1]
        risk = comparison[,2]
        #sharpe = comparison[,3]
        rnames = row.names(comparison)
    } else {
        # We have to make an assumption about the input here
        x=t(x[,ncol(x):1])
        returns = x[,1]
        risk = x[,2]
        rnames = names(returns)
    }

    # Set the charts to show the origin
    if(is.null(xlim[1]))
        xlim = c(0, max(risk) + 0.02)
    if(is.null(ylim[1]))
        ylim = c(min(c(0,returns)), max(returns) + 0.02)

    if(add.boxplots){
        original.layout <- par()
        layout( matrix( c(2,1,0,3), 2, 2, byrow=TRUE ),
                c(1,6), c(4,1),
            )
        par(mar=c(1,1,5,2))
    }

    # Draw the principal scatterplot
    plot(returns ~ risk,
        xlab='', ylab='',
        las = 1, xlim=xlim, ylim=ylim, col = colorset[columns:1], pch = symbolset[columns:1], axes= FALSE, ...)
#     abline(v = 0, col = elementcolor)
    if(ylim[1] != 0){
        abline(h = 0, col = element.color)
    }
    axis(1, cex.axis = cex.axis, col = element.color)
    axis(2, cex.axis = cex.axis, col = element.color)

    if(!add.boxplots){
        title(ylab = ylab, cex.lab = cex.lab)
        title(xlab = xlab, cex.lab = cex.lab)
    }

    # Add Sharpe ratio lines
    # @todo: Drawing Sharpe ratio lines currently throws warnings; change test statement
    if(!is.na(add.sharpe[1])) {
        for(line in add.sharpe) {
        abline(a=(Rf*12),b=add.sharpe[line],col="gray",lty=2)
        }
    }

    # Label the data points
    if(add.names)
        text(x = risk,y = returns, labels = rnames, pos=4, cex = 0.8, col = colorset[columns:1]) # adj = -0.1

    # Add a rug so that data points are easier to identify
    rug(side=1, risk, col = element.color)
    rug(side=2, returns, col = element.color)

    title(main = main, cex.main = cex.main)

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = cex.legend, border.col = element.color, pch = symbolset, bg = "white", legend = columnnames)
    }

    #title(sub='From Inception', line=1)
    box(col = element.color)

    if(add.boxplots){
        # Draw the Y-axis histogram
        par(mar=c(1,2,5,1))
        boxplot(returns, axes = FALSE, ylim = ylim)
        title(ylab = ylab, line = 0, cex.lab = cex.lab)

        # Draw the X-axis histogram
        par(mar=c(5,1,1,2))
        boxplot(risk, horizontal = TRUE, axes = FALSE, ylim=xlim)
        title(xlab = xlab, line = 1, cex.lab = cex.lab)

        par(original.layout)
    }
#     par(op)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################