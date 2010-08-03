KellyRatio <-
function (R, Rf = 0, method = "half")
{ # @author Brian G. Peterson

    # DESCRIPTION:
    # The Kelly Criterion was identified by Bell Labs scientist
    # can be basically stated as
    # bet size is the ratio of edge over odds
    # mathematically, you are maximizing log-utility
    #
    # Kelly criterion says: f should equal the expected excess return of the strategy divided by the expected variance of the excess return, or
    # f = (m-r)/s2

    # FUNCTION:
    R = checkData(R)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    kr <- function (R, Rf, method)
    {
        xR = Return.excess(R, Rf)
        KR =  mean(xR, na.rm=TRUE)/sd(R, na.rm=TRUE)^2
        if (method == "half") {
            KR = KR/2
        }
        return(KR)
    }

    result = apply(R, 2, kr, Rf = Rf, method = method)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = "Kelly Ratio"
    return (result)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################