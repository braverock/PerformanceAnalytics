#' calculate a compounded (geometric) cumulative return
#'
#' This is a useful function for calculating cumulative return over a period of
#' time, say a calendar year.  Can produce simple or geometric return.
#'
#' product of all the individual period returns
#'
#' \deqn{(1+r_{1})(1+r_{2})(1+r_{3})\ldots(1+r_{n})-1=prod(1+R)-1}{prod(1+R)-1}
#'
#' Note that this function calculates the total return over the entire period as a
#' single scalar value. If you need the time series of cumulative wealth (i.e. a
#' wealth index), you should use \code{cumprod(1+R)} or \code{cumprod(1+R)-1}.
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @author Peter Carl
#' @seealso \code{\link{Return.annualized}}
#' @references Bacon, Carl. \emph{Practical Portfolio Performance Measurement
#' and Attribution}. Wiley. 2004. p. 6
#'
#' For time series of cumulative wealth, see:
#' \code{\link[base]{cumprod}}
#' \code{\link{chart.CumReturns}}
### keywords ts multivariate distribution models
#' @examples
#'
#' data(managers)
#' Return.cumulative(managers[, 1, drop = FALSE])
#' Return.cumulative(managers[, 1:8])
#' Return.cumulative(managers[, 1:8], geometric = FALSE)
#'
#' @export
Return.cumulative <-
  function(R, geometric = TRUE) { # @author Peter Carl

    # This is a useful function for calculating cumulative return over a period
    # of time, say a calendar year.  Can produce simple or geometric return.

    if (is.vector(R)) {
      R <- na.omit(R)
      if (!geometric) {
        return(sum(R))
      } else {
        return(prod(1 + R) - 1)
      }
    } else {
      R <- checkData(R, method = "matrix")
      result <- apply(R, 2, Return.cumulative, geometric = geometric)
      dim(result) <- c(1, NCOL(R))
      colnames(result) <- colnames(R)
      rownames(result) <- "Cumulative Return"
      return(result)
    }
  }

###############################################################################
# R (https://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2026 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
