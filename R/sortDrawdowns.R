`sortDrawdowns` <-
function (runs) {
    # sortDrawdowns(findDrawdowns(caduceus.ts@Data[,1]))
    # gives the drawdowns in order of worst to best

    # modified with permission from function by Sankalp Upadhyay
    # <sankalp.upadhyay [at] gmail [dot] com>

    for (i in 1:length(runs$return)) {
        for (j in 1:(length(runs$return)-1)) {
            if (runs$return[j] > runs$return[j+1]) {
                tempRet = runs$return[j]
                tempFor = runs$from[j]
                tempTo = runs$to[j]
                tempLen = runs$length[j]
                runs$return[j] = runs$return[j+1]
                runs$from[j] = runs$from[j+1]
                runs$to[j] = runs$to[j+1]
                runs$length[j] = runs$length[j+1]
                runs$return[j+1] = tempRet
                runs$from[j+1] = tempFor
                runs$to[j+1] = tempTo
                runs$length[j+1] = tempLen
            }
        }
    }
    runs
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: sortDrawdowns.R,v 1.3 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.2  2007/02/06 11:58:42  brian
# - standardize attribution for Drawdown runs
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################