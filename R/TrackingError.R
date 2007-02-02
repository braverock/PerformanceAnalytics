`TrackingError` <-
function (R, Ri, scale = 12)
{ # @author Peter Carl

    # DESCRIPTION
    # TrackingError = sqrt(sum(assetReturns.vec - indexReturns.vec)^2 /
    #                   (length(assetReturns.vec) - 1)) * sqrt(scale)

    # Inputs:
    # Outputs:

    # FUNCTION
    assetReturns.vec = checkDataVector(R)
    indexReturns.vec = checkDataVector(Ri)

    TrackingError = sqrt(sum(assetReturns.vec - indexReturns.vec)^2 / (length(assetReturns.vec) - 1)) * sqrt(scale)

    TrackingError

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: TrackingError.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################