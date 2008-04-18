`CAPM.beta` <-
function (Ra, Rb, rf = 0, digits = 4)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a CAPM beta.

    # Inputs:
    # R: vector of returns for the asset being tested
    # Rb: vector of returns for the benchmark the asset is being gauged against
    # R and Rb are assumed to be matching periods and NO TEST IS MADE TO CHECK
    # rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as x and y.

    # Output:
    #

    # FUNCTION:

    assetReturns = checkData(Ra)
    benchmarkReturns = checkData(Rb)
    rf = checkData(rf)

#     if (length(assetReturns) != length(benchmarkReturns))
#         stop("Returns to be assessed have unequal time periods. Are there NA\'s in the data?")

    # Make these excess returns
    assetExcessRet = Return.excess(assetReturns, rf)
    indexExcessRet = Return.excess(benchmarkReturns, rf)

    # The data object now needs to be coerced into a data.frame for lm
    merged.z = na.omit(merge(assetExcessRet, indexExcessRet))
    merged.df = as.data.frame(merged.z)
    colnames(merged.df) = c("Asset.excess","Index.excess")

    # regress
    model.lm = lm(Asset.excess ~ Index.excess, merged.df)

#    alpha = coef(model.lm)[[1]]
    beta = coef(model.lm)[[2]]
    beta
}

`CAPM.beta.bull` <-
function (Ra, Rb, rf = 0, digits = 4)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a conditional CAPM beta for up markets.

    # Inputs:
    # R: vector of returns for the asset being tested
    # Rb: vector of returns for the benchmark the asset is being gauged against
    # R and Rb are assumed to be matching periods and NO TEST IS MADE TO CHECK
    # rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as x and y.

    # Output:
    #

    # FUNCTION:

    assetReturns = checkData(Ra)
    benchmarkReturns = checkData(Rb)
    rf = checkData(rf)

#     if (length(assetReturns) != length(benchmarkReturns))
#         stop("Returns to be assessed have unequal time periods. Are there NA\'s in the data?")

    # Make these excess returns
    assetExcessRet = Return.excess(assetReturns, rf)
    indexExcessRet = Return.excess(benchmarkReturns, rf)

    # The data object now needs to be coerced into a data.frame for lm
    merged.z = na.omit(merge(assetExcessRet, indexExcessRet))
    merged.df = as.data.frame(merged.z)
    colnames(merged.df) = c("Asset.excess","Index.excess")

    # regress
    model.lm = lm(Asset.excess ~ Index.excess, merged.df, subset= (Index.excess > 0))

#    alpha = coef(model.lm)[[1]]
    beta = coef(model.lm)[[2]]
    beta
}

`CAPM.beta.bear` <-
function (Ra, Rb, rf = 0, digits = 4)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a conditional CAPM beta for down markets 

    # Inputs:
    # R: vector of returns for the asset being tested
    # Rb: vector of returns for the benchmark the asset is being gauged against
    # R and Rb are assumed to be matching periods and NO TEST IS MADE TO CHECK
    # rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as x and y.

    # Output:
    #

    # FUNCTION:

    assetReturns = checkData(Ra)
    benchmarkReturns = checkData(Rb)
    rf = checkData(rf)

#     if (length(assetReturns) != length(benchmarkReturns))
#         stop("Returns to be assessed have unequal time periods. Are there NA\'s in the data?")

    # Make these excess returns
    assetExcessRet = Return.excess(assetReturns, rf)
    indexExcessRet = Return.excess(benchmarkReturns, rf)

    # The data object now needs to be coerced into a data.frame for lm
    merged.z = na.omit(merge(assetExcessRet, indexExcessRet))
    merged.df = as.data.frame(merged.z)
    colnames(merged.df) = c("Asset.excess","Index.excess")

    # regress
    model.lm = lm(Asset.excess ~ Index.excess, merged.df, subset= (Index.excess < 0))

#    alpha = coef(model.lm)[[1]]
    beta = coef(model.lm)[[2]]
    beta
}


`timing.ratio` <-
function (Ra, Rb, rf = 0, digits = 4)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function calculates the ratio of the two conditional CAPM betas (up and down).

    beta.bull = CAPM.beta.bull(Ra, Rb, rf = rf)
    beta.bear = CAPM.beta.bear(Ra, Rb, rf = rf)
    ratio = beta.bull/beta.bear
    ratio
}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CAPM.beta.R,v 1.9 2008-04-18 03:27:19 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
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
# - added checkDataVector for rf
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