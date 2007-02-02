`chart.Boxplot` <-
function (R, horizontal = TRUE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create box and whiskers plot, but with some sensible defaults
    # useful for comparing distributions.

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # This adjusts for longer labels on the left
    par(mar=c(5,10,4,2) + 0.1)

    boxplot(as.data.frame(R), horizontal = horizontal, las = 1, boxwex = .5, axis.cex = .8, text.cex = .5, ...)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Boxplot.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################