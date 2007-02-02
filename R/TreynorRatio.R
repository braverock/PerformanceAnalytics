`TreynorRatio` <-
function (R, Rb, rf = 0, scale = 12, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    #

    # FUNCTION:

    x = checkDataVector(R)
    y = checkDataVector(Rb)

    beta = CAPM.beta(R, Rb, rf = rf)
    treynorRatio = (Return.annualized(x, scale = scale) - rf*scale)/beta
    treynorRatio

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: TreynorRatio.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################