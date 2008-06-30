`TrackingError` <-
function (Ra, Rb, scale = 12)
{ # @author Peter Carl

    # DESCRIPTION
    # TrackingError = sqrt(sum(assetReturns.vec - benchmarkReturns.vec)^2 /
    #                   (length(assetReturns.vec) - 1)) * sqrt(scale)

    # Inputs:
    # Outputs:

    # FUNCTION
    assetReturns.vec = checkDataVector(Ra)
    benchmarkReturns.vec = checkDataVector(Rb)

    #TrackingError = sqrt(sum(assetReturns.vec - benchmarkReturns.vec)^2 / (length(assetReturns.vec) - 1)) * sqrt(scale)
    TrackingError = sd(assetReturns.vec - benchmarkReturns.vec) * sqrt(scale)
    TrackingError

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: TrackingError.R,v 1.8 2008-06-30 21:42:09 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.6  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.5  2007/04/04 00:23:01  brian
# - typos and minor comment updates
#
# Revision 1.4  2007/03/11 16:53:19  brian
# - add equations and text to documentation
# - standardize on Ra as the Return of the Asset
# - standardize on Ra as first argument where that wasn't previously true
#
# Revision 1.3  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.2  2007/02/07 13:20:52  brian
# - change Ri to Rb for benchmark asset to standardize parameters
# - change indexReturns.vec to benchmarkReturns.vec for consistency
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################