# Compute co-moment matrices

Return.centered <-
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
    rows.a = nrow(R)

    if(columns.a==1){
       R.centered = zoo(NA);
       R.mean = zoo(NA);
       R.mean = mean(R[, drop=FALSE])
       R.centered = R[ , drop=FALSE] - R.mean
    }else{
       R.mean = apply(R,2,'mean', na.rm=TRUE)
       # returns a vector holding the mean return for each asset

       R.centered = R - matrix( rep(R.mean,rows.a), ncol= columns.a, byrow=TRUE)
       # return the matrix of centered returns
   }


   # RESULTS:
    return(R.centered)
}

###############################################################################

CoSkewnessMatrix <-
function (R, ...)
{ # @author Kris Boudt
    return(M3.MM(R))
}

###############################################################################

CoKurtosisMatrix <-
function (R, ...)
{ # @author Kris Boudt
    return(M4.MM(R))
}

###############################################################################


centeredmoment = function(R,power)
{# @author Kris Boudt, Peter Carl
    R = checkData(R)
    out =  apply(Return.centered(R)^power,2,FUN=mean, na.rm=TRUE)
    return(out);
}

###############################################################################

centeredcomoment = function(Ra,Rb,p1,p2,normalize=FALSE)
{# @author Kris Boudt, Peter Carl, and Brian G. Peterson

    Ra = checkData(Ra); Rb = checkData(Rb);

    out = mean( na.omit( Return.centered(Ra)^p1 * Return.centered(Rb)^p2))

    if(normalize) {
        out=out/ as.numeric(centeredmoment(Rb,power=(p1+p2))) #
    }
    return(out);
}


###############################################################################

CoVariance<- function(Ra,Rb)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    covar <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        return(centeredcomoment(R[,1],R[,2],p1=1,p2=1,normalize=FALSE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) covar(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Covariance:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

BetaCoVariance <- function(Ra,Rb)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    bcovar <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        return(centeredcomoment(R[,1],R[,2],p1=1,p2=1,normalize=TRUE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) bcovar(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Beta Covariance:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}


CoSkewness <- function(Ra,Rb)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    cosk <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        return(centeredcomoment(R[,1],R[,2],p1=1,p2=2,normalize=FALSE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) cosk(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Coskewness:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

BetaCoSkewness <- function(Ra, Rb, test=FALSE)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    bcosk <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        skew = skewness(Rb)
        # Kris notes: the output should be conditional on the value of the market skewness. 
        if(skew > -0.05 && skew < 0.05 ){
            warning("skewness is close to zero. The classical definition of the coskewness statistic is not applicable and one should normalize using the comoment without standardization.")
        }
        if(test==TRUE){
#             if(skew < 0)
#                 multiplier = -1
#             else
#                 multiplier = 1
            stop("Not implemented yet")
        }
        else
            multiplier = 1

        return(multiplier * centeredcomoment(R[,1],R[,2],p1=1,p2=2,normalize=TRUE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) bcosk(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Beta Coskewness:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

CoKurtosis <- function(Ra,Rb)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    cokurt <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        return(centeredcomoment(R[,1],R[,2],p1=1,p2=3,normalize=FALSE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) cokurt(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Cokurtosis:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

BetaCoKurtosis <- function(Ra,Rb)
{# @author Kris Boudt, Peter Carl
    Ra= checkData(Ra)
    Rb= checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    bcosk <-function (Ra, Rb)
    {
        R = na.omit(cbind(Ra, Rb)) # remove NA's
        return(centeredcomoment(R[,1],R[,2],p1=1,p2=3,normalize=TRUE))
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb) bcosk(Ra[,n[1]], Rb[,n[2]]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Beta Cokurtosis:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}


###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2011 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################