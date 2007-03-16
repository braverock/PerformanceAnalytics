`Return.excess` <-
function (R, rf)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculates the returns of an asset in excess of the given 
    # "risk free rate" for the period.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # rf: a measure of the risk free rate, whether a period average
    #     (a single number) or a timeseries vector

    # Outputs:
    # A timeseries of the calculated series

    # FUNCTION:

    # Transform input data to a timeseries (zoo) object
    R = checkData(R, method="zoo")

    # if the risk free rate is delivered as a timeseries, we'll check it
    # and convert it to a zoo object.
    if(!is.null(dim(rf)))
        rf = checkData(rf, method = "zoo")

    ## arithmetic on zoo objects intersects them first
    R.excess = R[,1,drop = FALSE] - rf

    # RESULTS:
    return(R.excess)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.excess.R,v 1.3 2007-03-16 13:59:20 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
#
###############################################################################
