`VaR.CornishFisher` <-
function(R, p=0.99, modified = TRUE)
{   # @author Brian G. Peterson (completed/debugged fn)
    # @author Diethelm Wuertz (prototype function)


    # Description:

    # The limitations of mean Value-at-Risk are well covered in the literature.
    # Laurent Favre and Jose-Antonio Galeano published a paper in the
    # Fall 2002, volume 5 of the Journal of Alternative Investment,
    # "Mean-Modified Value-at-Risk optimization With Hedge Funds",
    # that proposed a modified VaR calculation that takes the higher moments
    # of non-normal distributions (skewness, kurtosis) into account, and
    # collapses to standard (traditional) mean-VaR if the return stream follows a
    # standard distribution.
    # This measure is now widely cited and used in the literature,
    # and is usually referred to as "Modified VaR" or "Modified Cornish-Fisher VaR"

    # Diethelm Wuertz's original function was called monthlyVaR, but did not
    # contain the required modifications to get to a monthly or an annualized number.
    # I have converted it to VaR.CornishFisher, and made the assumption of p=0.99, with an option for p=0.95 and
    # a collapse to normal mean VaR.

    # FUNCTION:

    # compute zc for the probability we want
    if ( p >= 0.51 ) {
        # looks like p was a percent like .99
        p = 1-p
    }
    zc = qnorm(p)

    R = checkData(R, method="matrix")
    columns = ncol(R)
    columnnames=colnames(R)
    # FUNCTION:
    for(column in 1:columns) {
        r = as.vector(R[,column])
        if (!is.numeric(r)) stop("The selected column is not numeric")

        if (modified) {
            s = skewness(r) #skewness of the distribution
            k = kurtosis(r) #(excess) kurtosis
            Zcf = zc + (((zc^2-1)*s)/6) + (((zc^3-3*zc)*k)/24) + (((2*zc^3)-(5*zc)*s^2)/36)
            VaR = mean(r) - (Zcf * sqrt(var(r)))
        } else {
            # should probably add risk-free-rate skew here?
            VaR = mean(r) - (zc * sqrt(var(r)))
        }
        VaR=array(VaR)
        if (column==1) {
            #create data.frame
            result=data.frame(VaR=VaR)
        } else {
            VaR=data.frame(VaR=VaR)
            result=cbind(result,VaR)
        }
    } #end columns loop

    colnames(result)<-columnnames

    # Return Value:
    result
}

###############################################################################

`modifiedVaR` <-
function(R, p=0.99)
{   # @author Brian G. Peterson

    # Description:

    # This is a wrapper function for VaR.CornishFisher,
    # because this measure is often referred to as modifiedVaR

    # FUNCTION:
    VaR.CornishFisher(R = R, p = p, modified=TRUE)

}

###############################################################################

`VaR.mean` <-
function(R, p=0.99)
{   # @author Brian G. Peterson

    # Description:

    # This is a wrapper function for modified VaR which assumes a normal
    # distribution by discounting influence from skewness or kurtosis.

    # Wrapper should be used with metrics related to VaR, such as Beyond VaR.

    # FUNCTION:
    VaR.CornishFisher(R = R, p = p, modified=FALSE)

}

###############################################################################

`VaR.traditional` <-
function(R, p=0.99, column=1)
{   # @author Brian G. Peterson

    # Description:

    # This is a wrapper function for modified VaR which assumes a normal
    # distribution by discounting influence from skewness or kurtosis.

    # Wrapper should be used with metrics related to VaR, such as Beyond VaR.

    # FUNCTION:
    VaR.CornishFisher(R = R, p = p, modified=FALSE)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: VaR.CornishFisher.R,v 1.8 2007-03-22 12:15:25 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2007/03/22 11:54:23  brian
# - added handlnig for multicolumn data
#
# Revision 1.6  2007/03/20 03:26:12  peter
# - removed firstcolumn
#
# Revision 1.5  2007/03/19 21:55:57  peter
# - replaced data checking with checkData function
#
# Revision 1.4  2007/03/11 16:58:07  brian
# - replace as.vector() with checkDataVector()
#
# Revision 1.3  2007/03/04 20:59:27  brian
# - minor changes to pass R CMD check
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################