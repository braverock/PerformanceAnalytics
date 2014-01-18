## rCornishFisher.r
##
## author: Eric Zivot
## created: May 28, 2008
## updated: May 28, 2009
##
## purpose: simulate observations based on Cornish-Fisher quantile expansion
##
##
rCornishFisher <- function(n, sigma, skew, ekurt, seed=NULL) {
## inputs:
## n          scalar, number of simulated values
## sigma      scalar, standard deviation
## skew       scalar, skewness
## ekurt      scalar, excess kurtosis
## outputs:
## n simulated values from Cornish-Fisher distribution
if (!is.null(seed)) set.seed(seed)
zc = rnorm(n)
z.cf = zc  + (((zc^2 - 1) * skew)/6) + (((zc^3 - 3 * zc) *
      ekurt)/24) - ((((2 * zc^3) - 5 * zc) * skew^2)/36)
ans = sigma*z.cf
ans
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