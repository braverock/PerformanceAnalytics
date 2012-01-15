sortDrawdowns <- function (runs) {
    # sortDrawdowns(findDrawdowns(returns))
    # gives the drawdowns in order of worst to best

    # original function modified with permission from function by Sankalp Upadhyay
    # <sankalp.upadhyay [at] gmail [dot] com>
    
    # this version provided by H. Felix Wittman < hfwittmann <at> googlemail <dot> com >
    
    index.sorted <- sort(runs$return, index=T)$ix # das kleinste zu erst
    runs.sorted <- lapply(runs, function(x) x <- x[index.sorted])
    
    return(runs.sorted)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################