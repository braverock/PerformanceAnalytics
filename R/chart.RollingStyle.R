`chart.RollingStyle` <-
function (R.fund, R.style, method = c("constrained","unconstrained","normalized"), width = 12, main = paste(colnames(R.fund)[1]," Rolling ", width ,"-Month Style Weights", sep=""), xaxis = TRUE, colorset = (1:12), legend.loc = "under", na.pad = FALSE, space = 0, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # R-Squared could deliver adjusted R-Squared if we wanted

    # FUNCTION:

    # Transform input data to a data frame
    R.fund = checkData(R.fund[,1,drop=FALSE], method = "zoo")
    R.style = checkData(R.style, method = "zoo")

    method = method[1]

    # Get dimensions and labels
    columns.fund = ncol(R.fund)
    columns.style = ncol(R.style)
    columnnames.fund = colnames(R.fund)
    columnnames.style = colnames(R.style)


    # Calculate
    merged.assets = na.omit(merge(R.fund, R.style))
    result = rollapply(data = merged.assets, FUN= function(x) {t(style.fit(R.fund= x[,1,drop=FALSE], R.style = x[,-1,drop=FALSE], method = method)$weights)}, width = width, by = 1, by.column = FALSE, na.pad = FALSE, align = "right")

    colnames(result) = columnnames.style

    chart.StackedBar(t(result), main = main, legend.loc = legend.loc, col = colorset, space = space, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.RollingStyle.R,v 1.2 2008-04-18 03:59:52 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2008/02/23 05:55:21  peter
# - chart demonstrating fund exposures through time
#
#
###############################################################################
