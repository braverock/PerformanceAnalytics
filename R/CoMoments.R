# Compute co-moment matrices

# for CoVarianceMatrix, use 'cov'


# @FIX: mean(R) doesn't work with zoo objects
# @FIX: is naming of these functions sensible?  or should they be renamed?

`Return.centered` <-
function (R,...)
{ # @author Peter Carl and Kris Boudt

    # DESCRIPTION:
    # Calculates the returns less the mean return of the asset

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns

    # Outputs:
    # A timeseries of the calculated series

    # FUNCTION:

    # Transform input data to a timeseries (zoo) object
    R = checkData(R, method="zoo")

    # Get dimensions and labels
    columns.a = ncol(R)

    if(columns.a==1){
       R.centered = zoo(NA);
       R.mean = zoo(NA);
       R.mean = mean(R[, drop=FALSE])
       R.centered = R[ , drop=FALSE] - R.mean
    }else{
       R.mean = apply(R,2,'mean', na.rm=TRUE)
       # returns a vector holding the mean return for each asset
    
       R.centered = R - R.mean
       # return the matrix of centered returns
   }


   # RESULTS:
    return(R.centered)
}

###############################################################################

`CoSkewnessMatrix` <-
function (R, ...)
{ # @author Kris Boudt
    return(M3.MM(R))
}

###############################################################################

`CoKurtosisMatrix` <-
function (R, ...)
{ # @author Kris Boudt
    return(M4.MM(R))
}

###############################################################################

centeredmoment = function(R,power)
{# @author Kris Boudt, Peter Carl
    R = checkData(R)
    out = mean(na.omit(Return.centered(R)^power))
    return(out);
}

###############################################################################

centeredcomoment = function(R1,R2,p1,p2,normalize=FALSE)
{# @author Kris Boudt, Peter Carl, and Brian G. Peterson

    R1 = checkData(R1); R2 = checkData(R2);
    out = mean( na.omit( Return.centered(R1)^p1 * Return.centered(R2)^p2))

    if(normalize) {
        out=out/ ( (sd(R1)^p1)*(sd(R2)^p2) )
    }
    return(out);
}


###############################################################################

CoVariance<- function(R1,R2)
{# @author Kris Boudt, Peter Carl
   return( centeredcomoment(R1,R2,p1=1,p2=1,normalize=F)   )
}

BetaCoVariance <- function(R1,R2)
{# @author Kris Boudt, Peter Carl
   return( centeredcomoment(R1,R2,p1=1,p2=1,normalize=T)   )
}


CoSkewness <- function(R1,R2)
{# @author Kris Boudt, Peter Carl
   return( centeredcomoment(R1,R2,p1=1,p2=2,normalize=F)   )
}

BetaCoSkewness <- function(R1,R2)
{# @author Kris Boudt, Peter Carl
   return( centeredcomoment(R1,R2,p1=1,p2=2,normalize=T)   )
}

CoKurtosis <- function(R1,R2)
{# @author Kris Boudt, Peter Carl
   return( centeredcomoment(R1,R2,p1=1,p2=3,normalize=F)   )
}

BetaCoKurtosis <- function(R1,R2)
{# @author Kris Boudt, Peter Carl
   return( centeredcomoment(R1,R2,p1=1,p2=3,normalize=T)   )
}


###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CoMoments.R,v 1.1 2008-01-23 10:17:17 kris Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.10  2008/01/18 02:56:46  peter
# - added comments for use of centeredcomoment function
#
# Revision 1.8  2008/01/06 01:31:32  peter
# - added "first" and "Rb" to other beta co-moments
# - renamed comoment functions back from ".portfolio" until a better convention
#   occurs
#
# Revision 1.7  2008/01/03 03:47:54  peter
# - added "first" and "Rb" parameters to BetaCoSkewness to preserve pairwise
#   comparisons using sapply
#
# Revision 1.6  2007/12/27 19:21:17  brian
# - change warn to warning
#
# Revision 1.5  2007/11/06 05:24:57  peter
# - added Return.centered
# - added CoMoment.portfolio functions
# - simplified BetaCoMoment functions
#
# Revision 1.4  2007/11/05 13:59:03  peter
# - changed functions per Kris' comments
#
# Revision 1.2  2007/10/31 14:47:16  peter
# - fixed errors caused by calculating mean on zoo objects
#