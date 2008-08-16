`chart.SnailTrail` <-
function (R, rf = 0, main = "Annualized Return and Risk", add.names = c("all", "lastonly", "firstandlast", "none"), xlab = "Annualized Risk", ylab = "Annualized Return", add.sharpe = c(1,2,3), colorset = 1:12, symbolset = 16, darken = FALSE , legend.loc = NULL, xlim = NULL, ylim = NULL, width = 12, stepsize = 12, lty=1, lwd=2, cex.axis=0.8, cex.main = 1, cex.lab = .9, cex.text = 0.8,...)
{ # @author Peter Carl

    # DESCRIPTION:

    # A chart that shows rolling calculations of annualized return and annualized
    # standard deviation have proceeded through time.  Lines and dots are darker for
    # more recent time periods.

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


    # Code inspired by a chart on:
    # http://zoonek2.free.fr/UNIX/48_R/03.html

    x = checkData(R, method = "zoo")
    if(!is.null(dim(rf)))
        rf = checkData(rf, method = "zoo")
#     x = na.omit(x)
    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = rownames(x)
    maxrows = 0

    add.names = add.names[1]

    # start the calculations with the start date of the first column
    length.column.one = length(x[,1])
    # find the row number of the last NA in the first column
    start.row = 1
    start.index = 0
    while(is.na(x[start.row,1])){
        start.row = start.row + 1
    }
    x = x[start.row:length.column.one,,drop=FALSE]


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

    plot.new()
###
    for(column in 1:columns) {
        # Assume we're passed in a series of monthly returns.  First, we'll
        # annualized returns and risk
        y = x[,column,drop=FALSE]
        y = na.omit(y)
#        y=x[,column,drop=FALSE]
        returns.column = rollapply(y[(nrow(y)%%stepsize+1):nrow(y),1,drop=FALSE], width = width, FUN = Return.annualized, na.pad = FALSE, align = "right",by=stepsize)
    
        risk.column = rollapply(y[(nrow(y)%%stepsize+1):nrow(y),1,drop=FALSE], width = width, FUN = StdDev.annualized, na.pad = FALSE, align = "right",by=stepsize)

        maxrows = max(maxrows, length(returns.column))

        if(column == 1){
            returns = returns.column
            risk = risk.column
        }
        else {
            returns = merge(returns,returns.column)
            risk = merge(risk,risk.column)
        }
    }

    # Set the charts to show the origin
    if(is.null(xlim[1]))
        xlim = c(0, max(risk, na.rm=TRUE) + 0.02)
    if(is.null(ylim[1]))
        ylim = c(min(c(0,returns), na.rm=TRUE), max(returns, na.rm=TRUE) + 0.02)
    plot.window(xlim=xlim, ylim=ylim)

    for(column in columns:1) {

        # colortrail = gray(0:(length(returns))/(length(returns)))
        colors = colorRamp(c(colorset[column],"white"))
        colortrail = rgb(colors((0:maxrows)/maxrows),max=255)
        n.rows = length(returns[,column])
        m.rows = length(na.omit(returns[,column]))
        # Draw the principal scatterplot
#         plot(returns ~ risk,
#             xlab='', ylab='',
#             las = 1, xlim=xlim, ylim=ylim, cex.axis = .8, col = colortrail[length(returns):1], pch = symbolset[columns:1], axes= FALSE, add=TRUE,...)

        for(i in 1:length(returns[,column])){
#             points(risk[i],returns[i], pch=symbolset[column], col = colortrail[length(returns)-i+1])
            points(risk[i,column],returns[i,column], pch=symbolset[column], col = colortrail[maxrows-i+1])
        }
        # Attach the points with lines
        for(i in 2:length(returns[,column])){
            segments(risk[i,column], returns[i,column], risk[i-1,column], returns[i-1,column],col = colortrail[maxrows-i+1], lty = lty, lwd = lwd)
        }

        # Label the data points
        if(!is.null(add.names))
            if(add.names == "lastonly"){
                labels = rep("",n.rows)
                labels[n.rows] = as.character(time(returns[n.rows,column]))
            }
            else if(add.names == "firstandlast"){
                labels = rep("",n.rows)
                labels[n.rows] = as.character(time(returns[n.rows,column]))
                labels[(n.rows-m.rows+1)] = as.character(time(returns[(n.rows-m.rows+1),column]))
            }
            else if(add.names == "all") {
                labels = time(returns)
            }
            else
                labels = NULL
            text(x = risk[,column,drop=FALSE],y = returns[,column,drop=FALSE], labels = labels, adj = -0.2, cex = cex.text, col = colortrail[maxrows:1])
    }
###

    if(ylim[1] != 0){
        abline(h = 0, col = elementcolor)
    }
    axis(1, cex.axis = cex.axis, col = elementcolor)
    axis(2, cex.axis = cex.axis, col = elementcolor)
    title(ylab = ylab, cex.lab = cex.lab)
    title(xlab = xlab, cex.lab = cex.lab)

    # Add Sharpe ratio lines
    # @todo: Drawing Sharpe ratio lines currently throws warnings; change test statement
    if(!is.na(add.sharpe[1])) {
        for(line in add.sharpe) {
        abline(a=(rf*12),b=add.sharpe[line],col="gray",lty=2)
        }
    }

    title(main = main, cex.main = cex.main) # @todo: put window into title by default

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = 0.8, border.col = elementcolor, pch = symbolset, bg = "white", legend = columnnames)
    }

    #title(sub='From Inception', line=1)
    box(col = elementcolor)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.SnailTrail.R,v 1.4 2008-08-16 03:40:32 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2008-06-26 01:46:26  peter
# - removed 'first' attribute
#
# Revision 1.2  2008-06-24 02:26:59  peter
# - changed 'add.labels' to 'add.names' to be consistent with chart.RiskReturnScatter
#
# Revision 1.7  2007/10/03 02:46:18  peter
# - colors and symbol sets now stretched to match the number of columns
# - name text colors prints backwards to match the order of the dots
#
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