`CAPM.alpha` <-
function (Ra, Rb, Rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a CAPM alpha.

    # Inputs:
    # R: vector of returns for the asset being tested
    # Rb: vector of returns for the benchmark the asset is being gauged against
    # R and Rb are assumed to be matching periods
    # Rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as R and y.

    # Output:
    # CAPM alpha

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

    alpha <-function (xRa, xRb)
    {
        merged = as.data.frame(na.omit(merge(xRa, xRb)))
        model.lm = lm(merged[,1] ~ merged[,2], merged)
        alpha = coef(model.lm)[[1]]
        alpha
    }

    result = apply(pairs, 1, FUN = function(n, xRa, xRb) alpha(xRa[,n[1]], xRb[,n[2]]), xRa = xRa, xRb = xRb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Alpha:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
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
# Revision 1.10  2009-10-06 03:01:15  peter
# - added label to results
#
# Revision 1.9  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.8  2009-09-29 14:30:00  peter
# - rewrite of function using apply for multi-column support
#
# Revision 1.7  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.6  2007/03/11 16:53:19  brian
# - add equations and text to documentation
# - standardize on Ra as the Return of the Asset
# - standardize on Ra as first argument where that wasn't previously true
#
# Revision 1.5  2007/02/28 03:23:41  peter
# - added checkDataVector to Rf
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