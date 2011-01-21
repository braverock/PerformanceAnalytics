###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2011 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

zerofill <- function (x) {
  mat<-checkData(x,"matrix")
  for(column in 1:ncol(mat)){
    for (row in 1:nrow(mat)){
      if(is.na(mat[row,column])) mat[row,column] <- 0
    }
  }
  x<-reclass(mat,x)
  return(x)
}
