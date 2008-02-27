`chart.Style` <-
function (R.fund, R.style, method = "constrained", main = NULL, colorset = rainbow(ncol(R.style)), ylim = NULL, las = 3, horiz = FALSE, cex.names = 1, legend.loc="under", leverage = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # R-Squared could deliver adjusted R-Squared if we wanted

    # FUNCTION:

    # Transform input data to a data frame
    R.fund = checkData(R.fund, method = "zoo")
    R.style = checkData(R.style, method = "zoo")

    # Calculate
    result = style.fit(R.fund, R.style, method = method, leverage = leverage)
    weights = as.matrix(result$weights)

    if(is.null(main))
        main = paste(colnames(R.fund)[1] ," Style Weights", sep="")

    if(is.null(ylim))
        if(method == "constrained" & leverage == FALSE) ylim = c(0,1)
        else ylim = NULL

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1


#     @todo: deal with horiz = TRUE; set las, mar, and ylim correctly
#   move this into chart.StackedBar.  
    if(dim(result$weights)[2] == 1){
        if(las > 1)
            par(mar=c(max(stringDims(colnames(R.style))$width)/2, 4, 4, 2)*cex.names+.1, cex = cex)
        barplot(t(weights), main = main, legend.loc = legend.loc, col = colorset, ylim = ylim, las = las, horiz = horiz, ...)
    }
    else {
#         if(las > 1)
#             par(mar=c(max(stringDims(colnames(R.fund))$width)/2, 4, 4, 2)+.1, cex = cex)
        chart.StackedBar(weights, main = main, legend.loc = legend.loc, col = colorset, ylim = ylim, las = las, horiz = horiz, cex.names = cex.names, ...)
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
# $Id: chart.Style.R,v 1.5 2008-02-27 04:05:32 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2008/02/26 04:49:06  peter
# - handles single column fits better
#
# Revision 1.3  2008/02/26 04:39:40  peter
# - moved legend and margin control into chart.StackedBar
# - handles multiple columns
#
# Revision 1.2  2008/02/23 05:35:56  peter
# - set ylim more sensibly depending on method
#
# Revision 1.1  2008/02/23 05:32:37  peter
# - simple bar chart of a fund's exposures to a set of factors, as determined
# by style.fit
#
#
###############################################################################
