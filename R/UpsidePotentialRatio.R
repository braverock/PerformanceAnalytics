`UpsidePotentialRatio` <-
function (R, MAR = 0, method=c("subset","full"))
{ # @author Brian G. Peterson

    # Description:
    # Sortino proposed to better account for skill and excess peRformance
    # by using only downside semivariance as the measure of risk.
    # UpsidePotentialRatio is an attempted improvement over the SortinoRatio

    # Ra    return vector
    # MAR   minimum acceptable return

    # Function:

    method = method[1] 

    if (is.vector(R)) {
        if(!is.null(dim(MAR)))
            MAR = mean(checkData(MAR, method = "vector"), rm.na=TRUE)
        r = subset(R, R > MAR)
        switch(method,
            full   = {len = length(R)},
            subset = {len = length(r)} #previously length(R)
        ) # end switch
        result = (sum(r - MAR)/len)/DownsideDeviation(R, MAR=MAR , method=method)
        return(result)
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, MARGIN = 2, UpsidePotentialRatio, MAR = MAR, method = method)
        dim(result) = c(1,NCOL(R))
        colnames(result) = colnames(R)
        rownames(result) = paste("Upside Potential (MAR = ",round(MAR*100,1),"%)", sep="")
        return(result)
    }
}

`UPR`<-
function (R, MAR = 0, method=c("subset","full"))
{ # @author Brian G. Peterson
    UpsidePotentialRatio(R=R, MAR=MAR, method=method)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: UpsidePotentialRatio.R,v 1.7 2009-10-06 15:14:44 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.4  2009-10-01 01:47:53  peter
# - added multi-column support
#
# Revision 1.3  2008-09-30 21:17:24  brian
# - both DownsideDeviation and UpsidePotentialRatio now support "method argument to use full or subset of series
# - use subset as default method
# - updated documentation to reflect change
#
# Revision 1.2  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.1  2007/06/22 02:16:52  brian
# - initial commit of functions and documentation for Sortino's Upside
# Potential Ratio
#
###############################################################################