maxDrawdown <- function (R, weights=NULL, geometric = TRUE, invert=TRUE, ...)
{ # @author Peter Carl
	
	# DESCRIPTION:
	# To find the maximum drawdown in a return series, we need to first
	# calculate the cumulative returns and the maximum cumulative return to
	# that point.  Any time the cumulative returns dips below the maximum
	# cumulative returns, it's a drawdown.  Drawdowns are measured as a
	# percentage of that maximum cumulative return, in effect, measured from
	# peak equity.
	
	# FUNCTION:
	if (is.vector(R) || ncol(R)==1 ) {
		R = na.omit(R)
        drawdown = Drawdowns(R, geometric = geometric)
        result = min(drawdown)
        if (invert) result<- -result
		return(result)
	}
	else {
        if(is.null(weights)) {
            R = checkData(R, method = "matrix")
    		result = apply(R, 2, maxDrawdown, geometric=geometric, invert=invert, ...=...)
    		dim(result) = c(1,NCOL(R))
    		colnames(result) = colnames(R)
    		rownames(result) = "Worst Drawdown"
            return(result)
        } else {
            # we have weights, do the portfolio calc
            portret<-Return.portfolio(R,weights=weights,geometric=geometric)
            result<-maxDrawdown(portret, geometric=geometric, invert=invert, ...=...)
            if (invert) result<- -result
            return(result)
        }
	}
}

CDD <- function (R, weights=NULL, geometric = TRUE, invert=TRUE, p=.95 ,  ...)
{
    p=.setalphaprob(p)
    if (is.vector(R) || ncol(R)==1) {
        R = na.omit(R)
        drawdowns =     sortDrawdowns(findDrawdowns(R))
        result = quantile(drawdowns$return,p)
        if(invert) result<- -result
        return(result)
    }    
    else {
        R = checkData(R, method = "matrix")
        if(is.null(weights)) {
            result=matrix(nrow=1,ncol=ncol(R))
            for(i in 1:ncol(R) ) {
                result[i]<-CDD(R[,i,drop=FALSE],p=p, geometric=geometric, invert=invert, ...=...)
            }
            dim(result) = c(1,NCOL(R))
            colnames(result) = colnames(R)
            rownames(result) = paste("Conditional Drawdown ",p*100,"%",sep='')
        } else {
            # we have weights, do the portfolio calc
            portret<-Return.portfolio(R,weights=weights,geometric=geometric)
            result<-CDD(portret,p=p, geometric=geometric, invert=invert, ...=...)
        }
        return(result)
    }
    # TODO add modified Cornish Fisher and copula methods to this to account for small number of observations likely on real data
}

DrawdownDeviation <-
function (R, ...) {

    # Calculates a standard deviation-type statistic using individual drawdowns.
    # 
    # DD = sqrt(sum[j=1,2,...,d](D_j^2/n)) where
    # D_j = jth drawdown over the entire period
    # d = total number of drawdowns in entire period
    # n = number of observations

    R = checkData(R)

    dd <- function(R) {
        R = na.omit(R)
        n=length(R)
        Dj=findDrawdowns(as.matrix(R))$return
        result = sqrt(sum((Dj[Dj<0]^2)/n))
        return(result)
    }

    result = apply(R, MARGIN = 2, dd)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = "Drawdown Deviation"
    return (result)
}

AverageDrawdown <-
function (R, ...) {

    # Calculates the average of the observed drawdowns.
    # 
    # ADD = abs(sum[j=1,2,...,d](D_j/d)) where
    # D'_j = jth drawdown over entire period
    # d = total number of drawdowns in the entire period

    R = checkData(R)

    ad <- function(R) {
        R = na.omit(R)
        Dj = findDrawdowns(as.matrix(R))$return
        d = length(Dj[Dj<0])
        result = abs(sum(Dj[Dj<0]/d))
        return(result)
    }

    result = apply(R, MARGIN = 2, ad)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = "Average Drawdown"
    return (result)
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