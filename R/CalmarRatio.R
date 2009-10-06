`CalmarRatio` <-
function (R, scale = NA)
{ # @author Brian G. Peterson

    # DESCRIPTION:
    # Inputs:
    # Ra: in this case, the function anticipates having a return stream as input,
    #    rather than prices.
    # scale: number of periods per year
    # Outputs:
    # This function returns a Calmar Ratio

    # FUNCTION:

    R = checkData(R)
    if(is.na(scale)) {
        freq = periodicity(R)
        switch(freq$scale,
            minute = {stop("Data periodicity too high")},
            hourly = {stop("Data periodicity too high")},
            daily = {scale = 252},
            weekly = {scale = 52},
            monthly = {scale = 12},
            quarterly = {scale = 4},
            yearly = {scale = 1}
        )
    }
    annualized_return = Return.annualized(R, scale=scale)
    drawdown = abs(maxDrawdown(R))
    result = annualized_return/drawdown
    rownames(result) = "Calmar Ratio"
    return(result)
}

`SterlingRatio` <-
function (R, scale=NA, excess=.1)
{ # @author Brian G. Peterson

    # DESCRIPTION:
    # Inputs:
    # Ra: in this case, the function anticipates having a return stream as input,
    #    rather than prices.
    # scale: number of periods per year
    # Outputs:
    # This function returns a Sterling Ratio

    # FUNCTION:

    R = checkData(R)
    if(is.na(scale)) {
        freq = periodicity(R)
        switch(freq$scale,
            minute = {stop("Data periodicity too high")},
            hourly = {stop("Data periodicity too high")},
            daily = {scale = 252},
            weekly = {scale = 52},
            monthly = {scale = 12},
            quarterly = {scale = 4},
            yearly = {scale = 1}
        )
    }
    annualized_return = Return.annualized(R, scale=scale)
    drawdown = abs(maxDrawdown(R)-excess)
    result = annualized_return/drawdown
    rownames(result) = paste("Sterling Ratio (Excess = ", round(excess*100,0), "%)", sep="")
    return(result)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CalmarRatio.R,v 1.8 2009-10-06 15:14:44 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2009-10-06 03:00:31  peter
# - added label to results
#
# Revision 1.6  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.5  2009-10-01 01:53:07  peter
# - missed one
#
# Revision 1.4  2009-10-01 01:52:13  peter
# - changed Ra to R for consistency
#
# Revision 1.3  2009-10-01 01:45:17  peter
# - added multi-column support
# - added periodicity check for scale
#
# Revision 1.2  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.1  2007/06/23 12:06:08  brian
# - initial revision of Calmar and Sterling Ratio functions and docs
#   originally requested by Khanh Nguyen <chaokhanh@yahoo.com>
#
###############################################################################