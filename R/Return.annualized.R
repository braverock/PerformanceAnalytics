`Return.annualized` <-
function (R, scale = NA, geometric = TRUE )
{ # @author Peter Carl

    # Description:

    # An average annualized return is convenient for comparing returns.
    # @todo: This function could be used for calculating geometric or simple
    # returns

    # R = periods under analysis
    # scale = number of periods in a year (daily f = 252, monthly f = 12,
    # quarterly f = 4)

    # arithmetic average: ra = (f/n) * sum(ri)
    # geometric average: rg = product(1 + ri)^(f/n) - 1

    # @todo: don't calculate for returns less than 1 year

    # FUNCTION:
    if (is.vector(R)) {
        R = checkData (R)
        R = na.omit(R)
        n = length(R)
        if(!xtsible(R) & is.na(scale))
            stop("'R' needs to be timeBased or xtsible, or scale must be specified." )
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
        #do the correct thing for geometric or simple returns
        if (geometric) {
            # geometric returns
            result = prod(1 + R)^(scale/n) - 1
        } else {
            # simple returns
            result = mean(R) * scale
        }
        result
    }
    else {
        R = checkData(R, method = "xts")
        result = apply(R, 2, Return.annualized, scale = scale, geometric = geometric)
        dim(result) = c(1,NCOL(R))
        colnames(result) = colnames(R)
        rownames(result) = "Annualized Return"
        return(result)
    }
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
# Revision 1.12  2009-10-06 02:58:00  peter
# - added label to results
#
# Revision 1.11  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.10  2009-10-02 18:38:42  peter
# - moved scale test
# - revised copyright
#
# Revision 1.9  2009-09-30 01:42:35  peter
# - added multi-column support
# - detects scale from periodicity
#
# Revision 1.8  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.7  2007/08/16 14:10:44  peter
# - updated checkData function
#
# Revision 1.6  2007/07/14 17:26:36  brian
# - add handling for geometric=FALSE (simple returns)
#
# Revision 1.5  2007/03/14 00:54:06  brian
# - updates to parameters for standardization
#
# Revision 1.4  2007/03/11 19:18:50  brian
# - standardize variable naming
#
# Revision 1.3  2007/02/15 01:14:43  brian
# - standardize parameter variaable names
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################