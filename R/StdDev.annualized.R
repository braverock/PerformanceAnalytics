`sd.multiperiod` <-
function (x, na.rm=FALSE, periods = 12, ...)
{
    #scale standard deviation by multiplying by the square root of the number of periods to scale by
	return(sqrt(periods)*sd(x, na.rm=na.rm))
}

`sd.annualized` <-
function (x, na.rm=FALSE, periods = 12, ...)
{
	sd.multiperiod(x, na.rm=na.rm, periods = scale)
}

`StdDev.annualized` <-
function (Ra, na.rm=FALSE, scale = 12)
{   # wrapper function for backwards compatibility
	sd.multiperiod(x, na.rm=na.rm, periods = scale)
}

###############################################################################
# sd function wrappers for backwards compatibility
`StdDev` <-
function(Ra)
{ # @author Brian G. Peterson
    result = sd(Ra))

    # Return Value:
    result
}

`std` <-
function(Ra) {
    # NOTE: std function is listed in the doc for fBasics, but not implemented
    return(sd(Ra))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: StdDev.annualized.R,v 1.5 2007-05-15 11:57:52 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2007/03/12 15:45:50  brian
# - add equations to documentation
# - standardize on Ra for Returns of asset
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