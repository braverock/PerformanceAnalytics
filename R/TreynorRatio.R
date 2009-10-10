`TreynorRatio` <-
function (Ra, Rb, Rf = 0, scale = NA)
{ # @author Peter Carl

    # DESCRIPTION:
    #

    # FUNCTION:
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    if(is.na(scale)) {
        freq = periodicity(Ra)
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

    tr <-function (xRa, xRb, scale)
    {
        beta = CAPM.beta(xRa, xRb)
        TR = (Return.annualized(xRa, scale = scale))/beta
        TR
    }

    result = apply(pairs, 1, FUN = function(n, xRa, xRb, scale) tr(xRa[,n[1]], xRb[,n[2]], scale), xRa = xRa, xRb = xRb, scale = scale)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Treynor Ratio:", colnames(Rb))
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
# $Id: TreynorRatio.R,v 1.12 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.11  2009-10-06 15:14:44  peter
# - fixed rownames
# - fixed scale = 12 replacement errors
#
# Revision 1.10  2009-10-06 02:55:06  peter
# - added label to results
#
# Revision 1.9  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.8  2009-10-01 01:47:21  peter
# - added multi-column support
# - added periodicity for scaling
#
# Revision 1.7  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.6  2007/08/30 12:18:32  brian
# - add passing of dots
#
# Revision 1.5  2007/04/09 03:31:01  peter
# - uses checkData
# - uses Return.excess
#
# Revision 1.4  2007/03/14 00:54:06  brian
# - updates to parameters for standardization
#
# Revision 1.3  2007/03/12 15:34:43  brian
# - add equations to documentation
# - standardize on Ra for Returns of asset
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################
