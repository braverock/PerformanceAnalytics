`chart.Style` <-
function (R.fund, R.style, method = c("constrained", "unconstrained", "normalized"), leverage = FALSE, main = NULL, ylim = NULL, unstacked=TRUE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # R-Squared could deliver adjusted R-Squared if we wanted

    # FUNCTION:

    # Transform input data to a data frame
    R.fund = checkData(R.fund)
    R.style = checkData(R.style)
    method = method[1]

    # Calculate
    result = style.fit(R.fund, R.style, method = method, leverage = leverage)
    weights = t(as.matrix(result$weights))

    if(is.null(main))
        main = paste(colnames(R.fund)[1] ," Style Weights", sep="")

    if(is.null(ylim))
        if(method == "constrained" & leverage == FALSE) ylim = c(0,1)
        else ylim = NULL

    chart.StackedBar(weights, main = main, ylim = ylim, unstacked = unstacked, ...)
#     barplot(weights, main = main, ylim = ylim, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2008-07-11 03:24:52  peter
# - fixed error with alignment of results
#
# Revision 1.6  2008-04-18 03:58:04  peter
# - reduced to a wrapper to chart.StackedBar
#
# Revision 1.5  2008/02/27 04:05:32  peter
# - added 'leverage' tag to eliminate sum to one constraint
# - added cex.names for controlling size of xaxis labels
#
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
