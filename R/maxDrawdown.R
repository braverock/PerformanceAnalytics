`maxDrawdown` <-
		function (R, weights=NULL, geometric = TRUE, invert=TRUE, ...)
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
    		result = apply(R, 2, maxDrawdown)
    		dim(result) = c(1,NCOL(R))
    		colnames(result) = colnames(R)
    		rownames(result) = "Worst Drawdown"
        } else {
            # we have weights, do the portfolio calc
            portret<-Return.portfolio(R,weights=weights,geometric=geometric)
            result<-maxDrawdown(portret,p=p, geometric=geometric, invert=invert, ...=...)
        }
        return(result)
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
# Revision 1.7  2009-10-06 15:14:44  peter
# - fixed rownames
# - fixed scale = 12 replacement errors
#
# Revision 1.5  2009-09-24 02:05:53  peter
# - added multicolumn support
#
# Revision 1.4  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.3  2007/06/05 13:10:10  peter
# - fixed calculation for negative value in first month
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################