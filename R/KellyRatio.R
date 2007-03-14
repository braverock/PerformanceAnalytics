`KellyRatio` <-
function (Ra, rf = 0, method = "half")
{ # @author Brian G. Peterson

    # DESCRIPTION:
    # The Kelly Criterion was identified by Bell Labs scientist
    #
    # can be basically stated as
    #
    # bet size is the ratio of edge over odds
    #
    # mathematically, you are maximizing log-utility
    #
    # Kelly criterion says: f should equal the expected excess return of the strategy divided by the expected variance of the excess return, or
    #
    # f = (m-r)/s2

    # FUNCTION:

    Ra = checkDataVector(Ra)
    leverage =  mean(Ra - rf)/std(Ra)^2

    if (method == "half") {
        leverage = leverage/2
    }
    return(leverage)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: KellyRatio.R,v 1.3 2007-03-14 00:54:06 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.2  2007/02/22 18:24:48  brian
# - fix syntaxt error in calc
#
# Revision 1.1  2007/02/21 17:25:19  brian
# - Initial Revision of Kelly Ratio calculation
#
###############################################################################