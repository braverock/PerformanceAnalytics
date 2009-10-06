`KellyRatio` <-
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
    rownames(result) = "Kelly Ratio"
    return (result)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: KellyRatio.R,v 1.6 2009-10-06 02:59:22 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2009-10-01 01:45:47  peter
# - added multi-column support
#
# Revision 1.4  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.3  2007/03/14 00:54:06  brian
# - updates to parameters for standardization
#
# Revision 1.2  2007/02/22 18:24:48  brian
# - fix syntaxt error in calc
#
# Revision 1.1  2007/02/21 17:25:19  brian
# - Initial Revision of Kelly Ratio calculation
#
###############################################################################