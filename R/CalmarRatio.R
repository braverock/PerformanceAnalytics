`CalmarRatio` <-
function (Ra, scale=12)
{ # @author Brian G. Peterson

    # DESCRIPTION:
    # Inputs:
    # Ra: in this case, the function anticipates having a return stream as input,
    #    rather than prices.
    # scale: number of periods per year
    # Outputs:
    # This function returns a Calmar Ratio

    # FUNCTION:

    Ra = checkData(Ra, method = "zoo")
    annualized_return = Return.annualized(Ra, scale=scale)
    drawdown          = abs(maxDrawdown(Ra))
    return(annualized_return/drawdown)
}

`SterlingRatio` <-
function (Ra, scale=12, excess=.1)
{ # @author Brian G. Peterson

    # DESCRIPTION:
    # Inputs:
    # Ra: in this case, the function anticipates having a return stream as input,
    #    rather than prices.
    # scale: number of periods per year
    # Outputs:
    # This function returns a Sterling Ratio

    # FUNCTION:

    Ra = checkData(Ra, method = "zoo")
    annualized_return = Return.annualized(Ra, scale=scale)
    drawdown          = abs(maxDrawdown(Ra)-excess)
    return(annualized_return/drawdown)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CalmarRatio.R,v 1.2 2008-06-02 16:05:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/06/23 12:06:08  brian
# - initial revision of Calmar and Sterling Ratio functions and docs
#   originally requested by Khanh Nguyen <chaokhanh@yahoo.com>
#
###############################################################################