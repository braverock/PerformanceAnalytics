#' caclulate the maximum drawdown from peak equity
#' 
#' To find the maximum drawdown in a return series, we need to first calculate
#' the cumulative returns and the maximum cumulative return to that point.  Any
#' time the cumulative returns dips below the maximum cumulative returns, it's
#' a drawdown.  Drawdowns are measured as a percentage of that maximum
#' cumulative return, in effect, measured from peak equity.
#' 
#' The option to \code{invert} the measure should appease both academics and
#' practitioners. The default option \code{invert=TRUE} will provide the
#' drawdown as a positive number.  This should be useful for optimization
#' (which usually seeks to minimize a value), and for tables (where having
#' negative signs in front of every number may be considered clutter).
#' Practitioners will argue that drawdowns denote losses, and should be
#' internally consistent with the quantile (a negative number), for which
#' \code{invert=FALSE} will provide the value they expect.  Individually,
#' different preferences may apply for clarity and compactness.  As such, we
#' provide the option, but make no value judgment on which approach is
#' preferable.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param weights portfolio weighting vector, default NULL, see Details
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param invert TRUE/FALSE whether to invert the drawdown measure.  see
#' Details.
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso \code{\link{findDrawdowns}} \cr \code{\link{sortDrawdowns}} \cr
#' \code{\link{table.Drawdowns}} \cr \code{\link{table.DownsideRisk}} \cr
#' \code{\link{chart.Drawdown}} \cr
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 88 \cr
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' t(round(maxDrawdown(edhec[,"Funds of Funds"]),4))
#' data(managers)
#' t(round(maxDrawdown(managers),4))
#' 
#' @export 
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





#' Calculate Uryasev's proposed Conditional Drawdown at Risk (CDD or CDaR)
#' measure
#' 
#' For some confidence level \eqn{p}, the conditional drawdown is the the mean
#' of the worst \eqn{p\%} drawdowns.
#' 
#' 
#' @aliases CDD CDaR
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param weights portfolio weighting vector, default NULL, see Details
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param invert TRUE/FALSE whether to invert the drawdown measure.  see
#' Details.
#' @param p confidence level for calculation, default p=0.95
#' @param \dots any other passthru parameters
#' @author Brian G. Peterson
#' @seealso \code{\link{ES}} \code{\link{maxDrawdown}}
#' @references Chekhlov, A., Uryasev, S., and M. Zabarankin. Portfolio
#' Optimization With Drawdown Constraints. B. Scherer (Ed.) Asset and Liability
#' Management Tools, Risk Books, London, 2003
#' http://www.ise.ufl.edu/uryasev/drawdown.pdf
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' t(round(CDD(edhec),4))
#' 
#' @export 
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

#' Calculates a standard deviation-type statistic using individual drawdowns.
#' 
#' DD = sqrt(sum[j=1,2,...,d](D_j^2/n)) where
#' D_j = jth drawdown over the entire period
#' d = total number of drawdowns in entire period
#' n = number of observations
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param \dots any other passthru parameters
#' @export
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

#' Calculates the average of the observed drawdowns.
#' 
#' ADD = abs(sum[j=1,2,...,d](D_j/d)) where
#' D'_j = jth drawdown over entire period
#' d = total number of drawdowns in the entire period
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param \dots any other passthru parameters
#' @export 
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

#' @rdname AverageDrawdown
#' @export 
AverageRecovery <-
function (R, ...) {

    # Calculates the average length (in months) of the observed recovery period.
    # 
    # ADD = abs(sum[j=1,2,...,d](D_j/d)) where
    # D'_j = jth drawdown over entire period
    # d = total number of drawdowns in the entire period

    R = checkData(R)

    ar <- function(R) {
        R = na.omit(R)
        Dj = findDrawdowns(as.matrix(R))$return
        Dr = findDrawdowns(as.matrix(R))$recovery
        d = length(Dr[Dj<0])
        result = abs(sum(Dr[Dj<0]/d))
        return(result)
    }

    result = apply(R, MARGIN = 2, ar)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = "Average Recovery"
    return (result)
}


###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
