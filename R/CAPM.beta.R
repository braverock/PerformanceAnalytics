`CAPM.beta` <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a CAPM beta.

    # Inputs:
    # Ra: vector of returns for the asset being tested
    # Rb: vector of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as x and y.

    # Output:
    # 

    # FUNCTION:
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    beta <-function (xRa, xRb)
    {
        merged = na.omit(merge(xRa, xRb))
        model.lm = lm(merged[,1] ~ merged[,2], merged)
        beta = coef(model.lm)[[2]]
        beta
    }

    result = apply(pairs, 1, FUN = function(n, xRa, xRb) beta(xRa[,n[1]], xRb[,n[2]]), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

`CAPM.beta.bull` <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a conditional CAPM beta for up markets.

    # Inputs:
    # Ra: time series of returns for the asset being tested
    # Rb: time series of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a time series
    #     of the same length as x and y.

    # Output:
    # Bear market beta

    # FUNCTION:
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    beta <-function (xRa, xRb)
    {
        merged = na.omit(merge(xRa, xRb))
        merged = as.data.frame(merged)
        colnames(merged) = c("xRa","xRb")
        model.lm = lm(xRa ~ xRb, merged, subset= (xRb > 0))
        beta = coef(model.lm)[[2]]
        beta
    }

    result = apply(pairs, 1, FUN = function(n, xRa, xRb) beta(xRa[,n[1]], xRb[,n[2]]), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Bull Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

`CAPM.beta.bear` <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a conditional CAPM beta for down markets 

    # Inputs:
    # Ra: time series of returns for the asset being tested
    # Rb: time series of returns for the benchmark the asset is being gauged against
    # Rf: risk free rate in the same periodicity as the returns.  May be a time series
    #     of the same length as Ra and Rb.

    # Output:
    # Bear market beta

    # FUNCTION:
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    beta <-function (xRa, xRb)
    {
        merged = na.omit(merge(xRa, xRb))
        merged = as.data.frame(merged)
        colnames(merged) = c("xRa","xRb")
        model.lm = lm(xRa ~ xRb, merged, subset= (xRb < 0))
        beta = coef(model.lm)[[2]]
        beta
    }

    result = apply(pairs, 1, FUN = function(n, xRa, xRb) beta(xRa[,n[1]], xRb[,n[2]]), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Bear Beta:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}


`TimingRatio` <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function calculates the ratio of the two conditional CAPM betas (up and down).

    beta.bull = CAPM.beta.bull(Ra, Rb, Rf = Rf)
    beta.bear = CAPM.beta.bear(Ra, Rb, Rf = Rf)
    result = beta.bull/beta.bear
    names = colnames(Rb)
    rownames(result) = paste("Timing Ratio:", names)
    return(result)
}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CAPM.beta.R,v 1.15 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.14  2009-10-06 03:01:04  peter
# - added label to results
#
# Revision 1.13  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.12  2009-09-29 14:29:47  peter
# - rewrite of function using apply for multi-column support
#
# Revision 1.11  2009-09-15 20:34:45  peter
# - fixed checkData for Rf such that a single value can be passed
#
# Revision 1.10  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.9  2008-04-18 03:27:19  peter
# - converted checkData to use zoo
# - added functions for conditional beta calcs: beta.bear, beta.bull, and timingRatio
#
# Revision 1.8  2007/09/14 02:03:25  peter
# - altered error message to correct syntax matching in editor
#
# Revision 1.7  2007/08/30 12:19:17  brian
# - change quoting to clean up syntax
#
# Revision 1.6  2007/03/11 16:53:19  brian
# - add equations and text to documentation
# - standardize on Ra as the Return of the Asset
# - standardize on Ra as first argument where that wasn't previously true
#
# Revision 1.5  2007/02/28 03:26:00  peter
# - added checkDataVector for Rf
#
# Revision 1.4  2007/02/08 21:43:39  brian
# - standardize parameters to R and Rb for consistency with other functions
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