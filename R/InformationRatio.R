`InformationRatio` <-
function (R, Rb, scale = 12)
{ # @author Peter Carl

    # DESCRIPTION
    # InformationRatio = ActivePremium/TrackingError

    # Inputs:
    # Outputs:

    # FUNCTION
    assetReturns.vec = checkDataVector(R)
    benchmarkReturns.vec = checkDataVector(Rb)

    ActivePremium = ActivePremium(assetReturns.vec,benchmarkReturns.vec, scale = scale)
    TrackingError = TrackingError(assetReturns.vec,benchmarkReturns.vec, scale = scale)

    InformationRatio = ActivePremium/TrackingError

    InformationRatio
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Rbsk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: InformationRatio.R,v 1.2 2007-02-07 13:20:52 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged foles to version control
# Bug 890
#
###############################################################################