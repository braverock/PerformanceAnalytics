`SharpeRatio.annualized` <-
function (R, Rf = 0, scale = NA, geometric=TRUE)
{ # @author Peter Carl

    # DESCRIPTION:

    # Using an annualized Sharpe Ratio is useful for comparison.  The annualized
    # Sharpe ratio is computed by dividing the annualized mean monthly excess
    # return by the annualized monthly standard deviation of excess return.

    # @todo: monthly standard deviation of ***excess*** return

    # Inputs:
    # R: in this case, the function anticipates having a return stream as input,
    #     rather than prices.
    # Rf: the risk free rate MUST be in the same periodicity as the data going in.

    # FUNCTION:
    R = checkData(R)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

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

    sr <-function (R, Rf, scale)
    {
        xR = Return.excess(R, Rf)
        SR = Return.annualized(xR, scale=scale, geometric=geometric)/StdDev.annualized(R, scale=scale)
        SR
    }

    result = apply(R, 2, sr, Rf=Rf, scale=scale)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = paste("Annualized Sharpe Ratio (Rf=", round(mean(Rf)*scale*100,1), "%)", sep="")
    return (result)

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
# Revision 1.13  2009-10-06 15:14:44  peter
# - fixed rownames
# - fixed scale = 12 replacement errors
#
# Revision 1.12  2009-10-06 02:56:53  peter
# - added label to results
#
# Revision 1.11  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.10  2009-10-02 18:37:19  peter
# - updated copyright
#
# Revision 1.9  2009-09-30 02:48:05  peter
# - added multi-column support
#
# Revision 1.8  2009-09-17 03:01:22  peter
# - using xts internally
#
# Revision 1.7  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.6  2007/08/16 14:12:40  peter
# - added checkData for Rf
#
# Revision 1.5  2007/04/09 03:33:19  peter
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