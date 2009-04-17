`chart.CaptureRatios` <-
function (Ra, Rb, main = "Capture Ratio", add.names = TRUE, xlab = "Downside Capture", ylab = "Upside Capture", colorset = 1, symbolset = 1, legend.loc = NULL, xlim = NULL, ylim = NULL, cex.legend = 1, cex.axis=0.8, cex.main = 1, cex.lab = 1, element.color="darkgray", benchmark.color = "darkgray",...)
{ # @author Peter Carl

    # DESCRIPTION:

    # A wrapper to create a scatter chart of upside capture versus 
    # downside capture for comparing manager performance.

    # Inputs:
    # R = usually a set of monthly return.
    # add.names = plots the row name with the data point.  Can be removed with
    #   by setting it to NULL.

    ratios = table.CaptureRatios(Ra, Rb) # Could use other methods?
    upside = ratios[,1]
    downside = ratios[,2]

    columns = ncol(ratios)
    rows = nrow(ratios)
    columnnames = colnames(ratios)
    rownames = rownames(ratios)
    benchmarkname= colnames(Rb[,1,drop=FALSE])

    if(length(colorset) < rows)
        colorset = rep(colorset, length.out = rows)

    if(length(symbolset) < rows)
        symbolset = rep(symbolset, length.out = rows)

    # Set the charts to show the origin
    if(is.null(xlim[1]))
        xlim = c(min(0.75, downside - 0.2), max(1.25, downside + 0.2)) 
    if(is.null(ylim[1]))
        ylim = c(min(0.75, upside - 0.2), max(1.25, upside + 0.2))

    # Draw the principal scatterplot
    plot(upside ~ downside,
        xlab='', ylab='',
        las = 1, xlim=xlim, ylim=ylim, col = colorset[rows:1], pch = symbolset[rows:1], axes= FALSE, ...)
    # Draw crosshairs and dot for benchmark and label it
    abline(v = 1, col = benchmark.color, lty=1)
    abline(h = 1, col = benchmark.color, lty=1)
    abline(0,1, col = element.color, lty=2)
    points(1,1, pch=16, col = benchmark.color)

    axis(1, cex.axis = cex.axis, col = element.color)
    axis(2, cex.axis = cex.axis, col = element.color)
    title(ylab = ylab, cex.lab = cex.lab)
    title(xlab = xlab, cex.lab = cex.lab)

    # Label the data points
    if(add.names){
        text(x = downside, y = upside, labels = rownames, pos=4, cex = 0.8, col = colorset[rows:1]) # adj = -0.1
        text(x = 1, y = 1, labels = benchmarkname, adj=c(-.1,-.5), cex = 0.8, col = benchmark.color) # adj = -0.1
    }

    # Add a rug so that data points are easier to identify
    rug(side=1, downside, col = element.color)
    rug(side=2, upside, col = element.color)

    title(main = main, cex.main = cex.main)

    if(!is.null(legend.loc)){
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = cex.legend, border.col = element.color, pch = symbolset, bg = "white", legend = rownames)
    }

    box(col = element.color)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.CaptureRatios.R,v 1.2 2009-04-17 04:15:56 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2009-04-02 03:04:45  peter
# - initial commit to cvs
#
# Revision 1.11  2008-08-16 03:39:54  peter
# - fixed point label adjustment
#
# Revision 1.10  2008-07-01 02:47:40  peter
# - added cex.legend
#
# Revision 1.9  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.8  2008-04-18 03:41:42  peter
# - added cex attributes for passing in formatting changes
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
