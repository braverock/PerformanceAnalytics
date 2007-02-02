`VaR.Beyond` <-
function (R, periods = 1)
{# @author Peter Carl
 # @author Brian G. Peterson (R-algorithm/testing)

    # Description:

    # Beyond VaR purports to estimate average loss beyond VaR.  Please note
    # that your milage will vary; expect that values obtained from the normal
    # distribution differs radically from the real situation.

    # BeyondVaR is described in theoretical detail in the paper:
    # as defined in:
    # Gaussel, N., Legras, J., Longin, F., and Rabemananjara, R.
    # "Beyond the VaR Horizon"
    # 2001, Quants Review No. 37

    # Assumes an input of regular period returns.


    # FUNCTION:
    if (class(R) == "numeric") {  #single column data

        BVaR = VaR.traditional(R) * periods^(1/(mean(R)-1))

        result = BVaR

    } else { #multi-column data

        if (class(R) == "timeSeries") {
            R = R@Data
        }
        if (class(R) == "vector") {
            R = array(R=R)
        }
        class(R)

        columns = ncol(R)
        columnnames=colnames(R)

        for(column in 1:columns) {
            r = as.vector(R[,column])
            if (!is.numeric(r)) stop("The selected column is not numeric")

            BVaR = VaR(r) * periods^(1/(mean(r)-1))

            BVaR=array(BVaR)
            if (column==1) {
                #create data.frame
                result=data.frame(BVaR=BVaR)
            } else {
                BVaR=data.frame(BVaR=BVaR)
                result=cbind(result,BVaR)
            }
        } #end columns loop

        colnames(result)<-columnnames

    } # end data type conditionals

    # Return Value:
    result
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: VaR.Beyond.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################