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
function (R, Rb, rf = 0)
{ #@author Brian G. Peterson

    R = checkDataVector(R)
    Rb = checkDataVector(Rb)

    if (length(R) != length(Rb))
        stop("Returns to be assessed have unequal time periods. Are there NA's in the data?")

    CML = rf + CAPM.CML.slope(Rb, rf)*mean(R)

    return(CML)
}

`CAPM.RiskPremium` <-
function (R, rf = 0)
{ #@author Brian G. Peterson

    R = checkDataVector(R)

    riskpremium = mean(R - rf)

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
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CAPM.utils.R,v 1.1 2007-03-03 18:10:46 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################