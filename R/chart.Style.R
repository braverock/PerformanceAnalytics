`chart.Style` <-
function (R.fund, R.style, method = "constrained", main = NULL, colorset = "darkgray", legend.loc = NULL, ylim = NULL, las = 3, horiz = FALSE, cex = 1, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # R-Squared could deliver adjusted R-Squared if we wanted

    # FUNCTION:

    # Transform input data to a data frame
    R.fund = checkData(R.fund[,1,drop=FALSE], method = "zoo")
    R.style = checkData(R.style, method = "zoo")

    # Calculate
    result = style.fit(R.fund, R.style, method = method)
    weights = t(result$weights)

    if(is.null(main))
        main = paste(colnames(R.fund)[1] ," Style Weights", sep="")

    if(is.null(ylim))
        if(method == "constrained") ylim = c(0,1)
        else ylim = NULL

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1
    if(las > 1)
        par(mar=c(max(stringDims(colnames(R.style))$width)/2, 4, 4, 2)+.1, cex = cex)

#     @todo: deal with horiz = TRUE; set las, mar, and ylim correctly

    barplot(weights, main = main, legend.loc = legend.loc, col = colorset, ylim = ylim, las = las, horiz = horiz, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Style.R,v 1.2 2008-02-23 05:35:56 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2008/02/23 05:32:37  peter
# - simple bar chart of a fund's exposures to a set of factors, as determined
# by style.fit
#
#
###############################################################################
