`ActivePremium` <-
function (Ra, Rb, scale = NA)
{ # @author Peter Carl

    # FUNCTION
    Ra = checkData(Ra)
    Rb = checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

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

    ap <-function (Ra, Rb, scale)
    {
        merged = na.omit(merge(Ra, Rb)) # align
        ap = (Return.annualized(merged[,1], scale = scale) - Return.annualized(merged[,2], scale = scale))
        ap
    }

#     ActivePremium = (Return.annualized(assetReturns.vec, scale = scale) - Return.annualized(benchmarkReturns.vec, scale = scale))
    result = apply(pairs, 1, FUN = function(n, Ra, Rb, scale) ap(Ra[,n[1]], Rb[,n[2]], scale), Ra = Ra, Rb = Rb, scale = scale)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Active Premium:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: ActivePremium.R,v 1.10 2009-10-06 15:14:44 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.9  2009-10-06 03:01:38  peter
# - added label to results
#
# Revision 1.8  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.7  2009-09-30 01:31:43  peter
# - added multiple-column support
#
# Revision 1.6  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.5  2007/04/04 00:23:01  brian
# - typos and minor comment updates
#
# Revision 1.4  2007/03/11 16:53:19  brian
# - add equations and text to documentation
# - standardize on Ra as the Return of the Asset
# - standardize on Ra as first argument where that wasn't previously true
#
# Revision 1.3  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.2  2007/02/07 13:20:52  brian
# - change Ri to Rb for benchmark asset to standardize parameters
# - change indexReturns.vec to benchmarkReturns.vec for consistency
#
# Revision 1.1  2007/02/02 19:06:14  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################