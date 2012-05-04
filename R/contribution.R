#' dissects the total portfolio returns into components
#' 
#' @aliases contibution
#' 
#' Performs simple contribution analysis by dissecting the total portoflio
#' returns into components

#' Performance calculated at the total portfolio level summarizes a lot 
#' of data within a single return number. Dissecting the total portfolio 
#' return into components allows an analyst to uncover the sources of 
#' the return. 
#'
#' @aliases contribution
#' @param Rp vector of portfolio returns
#' @param wp vector of portfolio weights
#' @author Andrii Babii
#' @seealso 
#' @references Jon A. Christopherson, David R., Wayne E. Ferson 
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 2009.
#' @examples
#' 
#'     data(managers)
#'     CalmarRatio(managers[,1,drop=FALSE])
#'     CalmarRatio(managers[,1:6]) 
#'     SterlingRatio(managers[,1,drop=FALSE])
#'     SterlingRatio(managers[,1:6])
#' 
contribution <- function (Rp, wp)
{ # @author Andrii Babii

    # DESCRIPTION:
    # Inputs:
    # Rp: portfolio returns
    # wp: portfolio weights
    # Outputs:
    # This function returns contibutions of each segment

    # FUNCTION:
    Rp = checkData(Rp)
    wp = checkData(wp)
    contr <- function(Rp, wp){
        c = Rp * wp
    }
}


#' @export 
#' @rdname contribution

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CalmarRatio.R 1905 2012-04-21 19:23:13Z braverock $
#
###############################################################################
