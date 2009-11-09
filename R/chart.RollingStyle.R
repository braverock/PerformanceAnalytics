`chart.RollingStyle` <-
function (R.fund, R.style, method = c("constrained","unconstrained","normalized"), leverage = FALSE, width = 12, main = NULL, space = 0, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # R-Squared could deliver adjusted R-Squared if we wanted

    # FUNCTION:

    # Transform input data to a data frame
    R.fund = checkData(R.fund[,1,drop=FALSE])
    R.style = checkData(R.style)

    method = method[1]

    # Get dimensions and labels
    columns.fund = ncol(R.fund)
    columns.style = ncol(R.style)
    columnnames.fund = colnames(R.fund)
    columnnames.style = colnames(R.style)


    # Calculate
    merged.assets = na.omit(merge(R.fund, R.style))

    result = xts:::rollapply.xts(merged.assets, FUN= function(x, method, leverage) {t(style.fit(R.fund = x[,1,drop=FALSE], R.style = x[,-1,drop=FALSE], method = method, leverage = leverage)$weights)}, width = width, method = method, leverage = leverage, by = 1, by.column = FALSE, na.pad = FALSE, align = "right")

    if (is.null(main)){
        freq = periodicity(R.fund)

        switch(freq$scale,
            minute = {freq.lab = "minute"},
            hourly = {freq.lab = "hour"},
            daily = {freq.lab = "day"},
            weekly = {freq.lab = "week"},
            monthly = {freq.lab = "month"},
            quarterly = {freq.lab = "quarter"},
            yearly = {freq.lab = "year"}
        )

        main = paste(colnames(R.fund)[1]," Rolling ", width ,"-",freq.lab," Style Weights", sep="")
    }
    colnames(result) = columnnames.style

    chart.StackedBar(result, main = main, space = space, ...)

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
# Revision 1.4  2009-10-15 21:50:19  brian
# - updates to add automatic periodicity to chart labels, and support different frequency data
#
# Revision 1.3  2008-07-11 03:22:01  peter
# - removed unnecessary function attributes
#
# Revision 1.2  2008-04-18 03:59:52  peter
# - added na.omit to avoid problems with missing data
#
# Revision 1.1  2008/02/23 05:55:21  peter
# - chart demonstrating fund exposures through time
#
#
###############################################################################
