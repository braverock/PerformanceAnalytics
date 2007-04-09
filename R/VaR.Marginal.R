`VaR.Marginal` <-
function(R, p=0.99, modified = TRUE, weightingvector)
{   # @author Brian G. Peterson

    # Description:

    # Function to implement Marginal VaR
    #
    # R                 Returns of your components
    # p                 probability to calculate VaR over
    # modified          use modified VaR or traditional VaR
    # weightingvector   to calculate portfolio returns with
    #
    # @returns data frame with total VaR of the portfolio plus Marginal VaR for each component

    R = checkDataMatrix(R)

    if (ncol(weightingvector) != ncol(R)) stop ("The Weighting Vector and Return Collection do not have the same number of Columns.")

    columns = ncol(R)
    columnnames=c("PortfolioVaR",colnames(R))

    # Function

    # first, get the numbers for the whole portfolio
    portfolioR   = pfolioReturn(R,as.vector(weightingvector))
    portfolioVaR = VaR.CornishFisher(portfolioR,p,modified)
    pVaR = array (portfolioVaR)
    result=data.frame(pVaR=pVaR)

    for(column in 1:columns) {
        # calculate a multiplication factor, because the results don't seem to make sense
        # unless the weighting vector always equals the same sum
        weightfactor = sum(weightingvector)/sum(weightingvector[,-column])  # if we do need it
        # weightfactor = 1  # if we don't need it

        subportfolioR   = pfolioReturn(R[ ,-column],as.vector(weightingvector[ ,-column]*weightfactor))
        subportfolioVaR = VaR.CornishFisher(subportfolioR,p,modified)

        marginalVaR = subportfolioVaR - portfolioVaR

        mVaR = array(marginalVaR)
        mVaR = data.frame(mVaR=mVaR)

        result=cbind(result,mVaR)
    } #end columns loop

    # check our result
    # this check would be used for Incremental/Component VaR, not Marginal VaR
    # if (portfolioVaR != sum(result[,-1])) warning (paste("The VaR of the portfolio ",portfolioVaR," does not match the sum of VaR.Marginal's ",sum(result[,-1])))

    colnames(result)<-columnnames

    # Return Value:
    result
} # end function VaR.Marginal

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: VaR.Marginal.R,v 1.4 2007-04-09 12:31:27 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/03/11 17:05:53  brian
# - change to use checkDataMatrix
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################