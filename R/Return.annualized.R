`Return.annualized` <-
function (Ra, scale = 12, geometric = TRUE )
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

    Ra = checkDataVector(Ra)
    Ra = Ra[!is.na(Ra)]
    n = length(Ra)
    # currently only uses geometric return to annualize returns
    #return(prod(1 + x)^(scale/length(x)) - 1)
    return(prod(1 + Ra)^(scale/n) - 1)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.annualized.R,v 1.5 2007-03-14 00:54:06 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
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