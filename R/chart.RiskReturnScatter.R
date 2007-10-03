`chart.RiskReturnScatter` <-
function (R, rf = 0, main = "Annualized Return and Risk", add.names = TRUE, xlab = "Annualized Risk", ylab = "Annualized Return", method = "calc", add.sharpe = c(1,2,3), add.boxplots = FALSE, colorset = 1, symbolset = 1, darken = FALSE , legend.loc = NULL, xlim = NULL, ylim = NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:

    # A wrapper to create a scatter chart of annualized returns versus
    # annualized risk (standard deviation) for comparing manager performance.
    # Also puts a box plot into the margins to help identify the relative
    # performance quartile.

    # Inputs:
    # R = usually a set of monthly return, but can also be a pre-calculated
    #   return and risk measure (set method = "nocalc", see below).  If the input
    #   is a set of monthly performance returns, this function will calculate
    #   the appropriate return and risk summary according to the method flag.
    # method = If method is set to "nocalc" then we assume that R is a column of
    #   return and a column of risk (e.g., annualized returns, annualized risk),
    #   in that order.  Other method cases can be set for different risk/return
    #   calculations.
    # rf = this is the risk free rate.  Remember to set this to the same
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

    x = checkData(R, method = "zoo")
    if(!is.null(dim(rf)))
        rf = checkData(rf, method = "zoo")

    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = rownames(x)

    # @todo: strip out basic elements to a scatter plot wrapper
    # Set color for key elements, easy to darken for the printer
    if(darken)
        elementcolor = "darkgray" #better for the printer
    else
        elementcolor = "lightgray" #better for the screen

    if(length(colorset) < columns)
        colorset = rep(colorset, length.out = columns)

    if(length(symbolset) < columns)
        symbolset = rep(symbolset, length.out = columns)

    # @todo: add flags to use other risk measures

    if(method == "calc"){
        # Assume we're passed in a series of monthly returns.  First, we'll
        # annualized returns and risk
        comparison = t(table.AnnualizedReturns(x[,columns:1], rf = rf))

        returns = comparison[,1]
        risk = comparison[,2]
        #sharpe = comparison[,3]
    } else {
        # We have to make an assumption about the input here
        returns = x[,1]
        risk = x[,2]
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
        las = 1, xlim=xlim, ylim=ylim, cex.axis = .8, col = colorset[columns:1], pch = symbolset[columns:1], axes= FALSE, ...)
#     abline(v = 0, col = elementcolor)
    if(ylim[1] != 0){
        abline(h = 0, col = elementcolor)
    }
    axis(1, cex.axis = 0.8, col = elementcolor)
    axis(2, cex.axis = 0.8, col = elementcolor)

    if(!add.boxplots){
        title(ylab = ylab)
        title(xlab = xlab)
    }

    # Add Sharpe ratio lines
    # @todo: Drawing Sharpe ratio lines currently throws warnings; change test statement
    if(!is.na(add.sharpe[1])) {
        for(line in add.sharpe) {
        abline(a=(rf*12),b=add.sharpe[line],col="gray",lty=2)
        }
    }

    # Label the data points
    if(add.names)
        text(x = risk,y = returns, labels = row.names(comparison), adj = -0.1, cex = 0.8, col = colorset[columns:1])

    # Add a rug so that data points are easier to identify
    rug(side=1, risk, col = elementcolor)
    rug(side=2, returns, col = elementcolor)

    title(main = main)

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = 0.8, border.col = elementcolor, pch = symbolset, bg = "white", legend = columnnames)
    }

    #title(sub='From Inception', line=1)
    box(col = elementcolor)

    if(add.boxplots){
        # Draw the Y-axis histogram
        par(mar=c(1,2,5,1))
        boxplot(returns, axes = FALSE, ylim = ylim)
        title(ylab = ylab, line = 0)

        # Draw the X-axis histogram
        par(mar=c(5,1,1,2))
        boxplot(risk, horizontal = TRUE, axes = FALSE, ylim=xlim)
        title(xlab = xlab, line = 1)

        #par(original.layout)
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
# $Id: chart.RiskReturnScatter.R,v 1.7 2007-10-03 02:46:18 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.6  2007/09/24 02:49:34  peter
# - chart elements now consistent with time series charts
# - prints columns backwards so that earlier columns printed on top of later
# - return axis now unbounded below zero, although it will show zero
# - zero return line drawn if min is not zero
#
# Revision 1.5  2007/08/16 14:29:16  peter
# - modified checkData to return Zoo object
# - added checkData to handle Rf as a time series rather than a point est
#
# Revision 1.4  2007/04/09 12:31:27  brian
# - syntax and usage changes to pass R CMD check
#
# Revision 1.3  2007/04/02 21:53:25  peter
# - changed to checkData function
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################