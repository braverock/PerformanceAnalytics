apply.rolling <- function (R, width, trim = TRUE, gap = 12, by = 1, FUN = "mean", ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # FUNCTION:
    R = checkData(R)
    R = na.omit(R)
    rows=NROW(R)
    result = xts(, order.by = time(R))
    dates=time(R)

    calcs = matrix()

    if(width == 0) { # from inception
        gap = gap
    }
    else
        gap = width
    steps = seq(from = rows, to = gap, by = -by)
    steps = steps[order(steps)]
    for(row in steps) {
        if (width == 0)  # from inception
            r = R[1:row,]
        else
            r = R[(row-width+1):row,]
        calc = apply(r, MARGIN = 2, FUN = FUN, ...=...)
        calcs = rbind(calcs, calc)
    }
    calcs = xts(calcs[-1],order.by=dates[steps])
    result = merge(result, calcs)
# print(result)
#     colnames(result)=colnames(R)
    result = reclass(result, R)
    return (result)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2011 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################