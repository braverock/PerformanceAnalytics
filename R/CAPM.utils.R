# This file exists to contain several related and small CAPM utility functions.
# CAPM.alpha and CAPM.beta could probably have gone in here too, but they're already in separate files

`CAPM.CML.slope` <-
function (Rb, rf = 0 )
{ #author Brian G. Peterson

  #the Capital Market Line slope is a wrapper for the Sharpe Ratio on the benchmark asset
  #
  # Rb = Return vector of the benchmark or market portfolio
  return(SharpeRatio(Rb,rf))
}

`CAPM.CML` <-
function (Ra, Rb, rf = 0)
{ #@author Brian G. Peterson

    Ra = checkData(Ra, method="zoo")
    Rb = checkData(Rb, method="zoo")
    rf = checkData(rf, method="zoo")

    if (length(Ra) != length(Rb))
        stop("Returns to be assessed have unequal time periods. Are there NAs in the data?")

    CML = mean(rf) + CAPM.CML.slope(Rb, rf)*mean(Ra)

    return(CML)
}

`CAPM.RiskPremium` <-
function (Ra, rf = 0)
{ #@author Brian G. Peterson

    Ra = checkDataVector(Ra)
    rf = checkDataVector(rf)

    riskpremium = mean(Ra - rf)

    return (riskpremium)
}

`CAPM.SML.slope` <-
function (Rb, rf = 0)
{ #@author Brian G. Peterson

    Rb = checkDataVector(Rb)

    SML.slope = 1/CAPM.RiskPremium(Rb, rf)

    return(SML.slope)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CAPM.utils.R,v 1.6 2008-06-02 16:05:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2007/08/15 20:08:46  brian
# - fix warning for quote formatting
#
# Revision 1.4  2007/08/14 21:03:57  peter
# - changed checkData to use zoo instead of vector
#
# Revision 1.3  2007/08/14 20:53:27  peter
# - changed rf to mean(rf) in CAPM.CML
#
# Revision 1.2  2007/03/11 16:53:19  brian
# - add equations and text to documentation
# - standardize on Ra as the Return of the Asset
# - standardize on Ra as first argument where that wasn't previously true
#
# Revision 1.1  2007/03/03 18:10:46  brian
# - Initial Revision of functions and documentation for CAPM utils on CML, SML, and RiskPremium
#
###############################################################################
