`apply.rolling` <-
function (R, width, trim = TRUE, gap = 12, by = 1, FUN = "mean", ...)
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
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2009-10-02 21:27:39  peter
# - removed multi-column support so larger objects can be passed in
#
# Revision 1.4  2009-10-02 18:44:10  peter
# - revamped to provide xtsible windows
#
# Revision 1.3  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.2  2007/04/04 02:45:04  peter
# - added some backflips to name the single column zoo object
#
# Revision 1.1  2007/03/20 03:28:20  peter
# - uses zoo functions to apply functions to rolling windows
#
###############################################################################
