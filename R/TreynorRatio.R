`TreynorRatio` <-
function (Ra, Rb, rf = 0, scale = 12, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    #

    # FUNCTION:

    Ra = checkData(Ra, method = "vector", ...=...)
    Rb = checkData(Rb, method = "vector", ...=...)

    beta = CAPM.beta(Ra, Rb, rf = rf, ...=...)
    Re = Return.excess(Ra, rf=rf, ...=...)
    treynorRatio = (Return.annualized(Re, scale = scale, ...=...))/beta
    treynorRatio

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: TreynorRatio.R,v 1.7 2008-06-02 16:05:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.6  2007/08/30 12:18:32  brian
# - add passing of dots
#
# Revision 1.5  2007/04/09 03:31:01  peter
# - uses checkData
# - uses Return.excess
#
# Revision 1.4  2007/03/14 00:54:06  brian
# - updates to parameters for standardization
#
# Revision 1.3  2007/03/12 15:34:43  brian
# - add equations to documentation
# - standardize on Ra for Returns of asset
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################
