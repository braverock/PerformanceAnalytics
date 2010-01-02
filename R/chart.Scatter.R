`chart.Scatter` <-
function (x, y, reference.grid = TRUE, main = "Title", ylab=NULL, xlab=NULL, xlim = NA, ylim = NA, colorset = 1, symbolset = 1, element.color = "darkgray", cex.axis = 0.8, cex.legend = 0.8, cex.lab = 1, cex.main = 1, ...)
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

    # All other inputs are the same as "plot" and are principally included
    # so that some sensible defaults could be set.

    # Output:
    # Draws a scatter chart with some sensible defaults.

    # FUNCTION:
    x = checkData(x, method = "vector") ### ???
    y = checkData(y, method = "vector")

    # pass in: cex.axis = cex.axis, cex.main = cex.main, cex.lab = cex.lab
    plot(x~y, main = main, pch = symbolset, col=colorset, ...)

    if(reference.grid) {
        grid(col = element.color)
        abline(h = 0, col = element.color)
        abline(v = 0, col = element.color)
    }

    rug(side=1, x, col = element.color)
    rug(side=2, y, col = element.color)

    box(col = element.color)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.9  2009-06-02 03:17:28  peter
# - cleaned out old comments
#
# Revision 1.8  2009-04-18 02:56:53  peter
# - argument cleanup and codoc issues
#
# Revision 1.7  2009-04-07 22:26:18  peter
# - added rug and other graphic features
#
# Revision 1.6  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.5  2008-04-18 03:51:00  peter
# - added cex attributes
#
# Revision 1.4  2007/04/25 20:07:12  peter
# - added x-y axes
#
# Revision 1.3  2007/04/02 21:58:42  peter
# - changed colorset default
# - changed to use checkData
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################