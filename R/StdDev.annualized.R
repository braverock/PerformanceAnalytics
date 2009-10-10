`sd.multiperiod` <-
function (x, scale = NA)
{
    if (is.vector(x)) {
        #scale standard deviation by multiplying by the square root of the number of periods to scale by
        if(!xtsible(x) & is.na(scale))
            stop("'x' needs to be timeBased or xtsible, or scale must be specified." )
        x = checkData (x)
        if(is.na(scale)) {
            freq = periodicity(x)
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
        sqrt(scale)*sd(x, na.rm=TRUE)
    } else { 
        result = apply(x, 2, sd.multiperiod, scale=scale)
        dim(result) = c(1,NCOL(x))
        colnames(result) = colnames(x)
        rownames(result) = "Annualized Standard Deviation"
        return(result)
    }
}

`sd.annualized` <-
function (x, scale = NA)
{   # wrapper function for backwards compatibility
    sd.multiperiod(x, scale = scale)
}

`StdDev.annualized` <-
function (R, scale = NA)
{   # wrapper function for backwards compatibility
    sd.multiperiod(R, scale = scale)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: StdDev.annualized.R,v 1.20 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.19  2009-10-06 15:14:44  peter
# - fixed rownames
# - fixed scale = 12 replacement errors
#
# Revision 1.18  2009-10-06 02:55:38  peter
# - added label to results
#
# Revision 1.17  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.16  2009-10-02 18:36:38  peter
# - fixed scale testing
#
# Revision 1.15  2009-09-30 03:00:04  peter
# - added periodicity for setting scale
#
# Revision 1.14  2008-10-14 14:37:29  brian
# - convert from matrix or data.frame to zoo in checkData call
#
# Revision 1.13  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.12  2007/08/26 10:01:46  brian
# - code comment and indentation cleanup, no functional change
#
# Revision 1.11  2007/08/26 09:54:28  brian
# - simplify recursion to eliminate possibility of endless loop
#
# Revision 1.10  2007/08/25 22:55:49  brian
# - modify to mimic class behavior of sd function
#   should handle both single and multicolumn data smoothly now
#
# Revision 1.9  2007/08/16 14:27:37  peter
# - added NA removal default
# - modified checkData to return a vector
#
# Revision 1.8  2007/06/07 23:02:20  brian
# - update passing of ... into functions
# - fix scale/periods cut and paste error
#
# Revision 1.7  2007/06/04 14:32:33  peter
# - fixed x and Ra replacement error
#
# Revision 1.6  2007/05/15 20:02:01  brian
# - fix syntax error (extra paren)
#
# Revision 1.5  2007/05/15 11:57:52  brian
# - standardize usage to match common R usage
# - define sd.annualized and sd.multiperiod as generic extensions of R core sd fn
# - move StdDev and std wrappers to this file
#
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
