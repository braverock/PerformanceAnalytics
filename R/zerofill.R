###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

#' zerofill
#' 
#' Fill NA's with zeros in a time series to allow analysis when the data must
#' be complete.
#' 
#' Note that this function has risks, use carefully.  Complete data is
#' preferred.  Barring that, filling a small percentage of results in the
#' middle of a large set is unlikely to cause problems. Barring that, realize
#' that this will skew your results.
#' 
#' @param x time series to zero fill
#' @export
zerofill <- function (x) {
  x <- checkData(x,"xts")
  x[is.na(x)] <- 0
  return(x)
}
