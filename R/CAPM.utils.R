# This file exists to contain several related and small CAPM utility functions.
# CAPM.alpha and CAPM.beta could probably have gone in here too, but they're already in separate files

`CAPM.CML.slope` <-
function (Rb, Rf = 0 )
{ #author Brian G. Peterson

    #the Capital Market Line slope is a wrapper for the Sharpe Ratio on the benchmark asset
    #
    # Rb = Return vector of the benchmark or market portfolio
    result = SharpeRatio(Rb,Rf,FUN="StdDev")
    names = colnames(Rb)
    rownames(result) = paste("Capital Market Line Slope:", names)
    return(result) 
}

`CAPM.CML` <-
function (Ra, Rb, Rf = 0)
{ #@author Brian G. Peterson

    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    cml <-function (Ra, Rb, Rf)
    {
        CML = mean(Rf, na.rm=TRUE) + CAPM.CML.slope(Rb, Rf)*mean(Ra, na.rm=TRUE)
        return(CML)
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb, Rf) cml(Ra[,n[1]], Rb[,n[2]], Rf), Ra = Ra, Rb = Rb, Rf = Rf)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Capital Market Line:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

`CAPM.RiskPremium` <-
function (Ra, Rf = 0)
{ #@author Brian G. Peterson

    Ra = checkData(Ra)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    xRa = Return.excess(Ra, Rf)
    result = apply(xRa, 2, mean, na.rm=TRUE)
    dim(result) = c(1,NCOL(Ra))
    colnames(result) = colnames(Ra)
    rownames(result) = paste("Risk Premium (Rf=", round(mean(Rf)*100,1),"%)", sep="")
    return (result)
}

`CAPM.SML.slope` <-
function (Rb, Rf = 0)
{ #@author Brian G. Peterson

    result = 1/CAPM.RiskPremium(Rb, Rf)
    names = colnames(Rb)
    rownames(result) = paste("Security Market Line:", names)
    return(result)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.11  2009-10-10 12:40:08  brian
# - update copyright to 2004-2009
#
# Revision 1.10  2009-10-06 15:14:44  peter
# - fixed rownames
# - fixed scale = 12 replacement errors
#
# Revision 1.9  2009-10-06 03:00:52  peter
# - added label to results
#
# Revision 1.8  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.7  2009-09-30 02:22:59  peter
# - added multi-column support
#
# Revision 1.6  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.5  2007/08/15 20:08:46  brian
# - fix warning for quote formatting
#
# Revision 1.4  2007/08/14 21:03:57  peter
# - changed checkData to use zoo instead of vector
#
# Revision 1.3  2007/08/14 20:53:27  peter
# - changed Rf to mean(Rf) in CAPM.CML
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
