#' Find the drawdowns and drawdown levels in a timeseries.
#' 
#' \code{findDrawdowns} will find the starting period, the ending period, and
#' the amount and length of the drawdown.
#' 
#' Often used with \code{\link{sortDrawdowns}} to get the largest drawdowns.
#' 
#' \code{Drawdowns} will calculate the drawdown levels as percentages, for use
#' in \code{\link{chart.Drawdown}}.
#' 
#' Returns an unordered list: \cr 
#' \itemize{ 
#'   \item return depth of drawdown
#'   \item from starting period 
#'   \item to ending period \item length length in periods 
#' }
#' 
#' @aliases findDrawdowns Drawdowns
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' 
#' \code{findDrawdowns} modified with permission from function by Sankalp
#' Upadhyay
#' @seealso 
#' \code{\link{sortDrawdowns}} \cr 
#' \code{\link{maxDrawdown}} \cr
#' \code{\link{sortDrawdowns}} \cr 
#' \code{\link{table.Drawdowns}} \cr
#' \code{\link{table.DownsideRisk}} \cr 
#' \code{\link{chart.Drawdown}} \cr
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 88 \cr
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' findDrawdowns(edhec[,"Funds of Funds", drop=FALSE])
#' sortDrawdowns(findDrawdowns(edhec[,"Funds of Funds", drop=FALSE]))
#' 
#' @export
findDrawdowns <-
function (R, geometric = TRUE, ...)
{ # @author Peter Carl

    # modified with permission from function by Sankalp Upadhyay
    # <sankalp.upadhyay [at] gmail [dot] com>

    # DESCRIPTION:
    # Find the drawdowns in a timeseries.
    # Find the starting period, the ending period, and the amount and length
    # of the drawdown.

    # FUNCTION:

    x = checkData(R[,1,drop=FALSE], method="matrix") # matrix?

#     Return.cumulative = cumprod(1+na.omit(x)) 
#     maxCumulativeReturn = cummax(c(1,Return.cumulative))[-1]
#     drawdowns = Return.cumulative/maxCumulativeReturn - 1
    drawdowns = Drawdowns(x, geometric = geometric)
    # if you want to see the drawdown series, plot(drawdown,type="l")

    draw = c()
    begin = c()
    end = c()
    length = c(0)
    trough = c(0)
    index = 1
    if (drawdowns[1] >= 0)
        priorSign = 1
    else
        priorSign = 0
    from = 1
    sofar = as.numeric(drawdowns[1])
    to = 1
    dmin = 1

    for (i in 1:length(drawdowns)) { #2:length()
        thisSign <- ifelse(drawdowns[i] < 0, 0, 1)
        if (thisSign == priorSign) { ###
            if(as.numeric(drawdowns[i])< as.numeric(sofar)) {
                sofar = drawdowns[i]
                dmin = i
            }
            to = i + 1 ###
        }
        else { 
            draw[index] = sofar
            begin[index] = from
            trough[index] = dmin
            end[index] = to
            #cat(sofar, from, to)
            from = i 
            sofar = drawdowns[i]
            to = i + 1 ###
            dmin = i
            index = index + 1
            priorSign = thisSign
        }
    }
    draw[index] = sofar
    begin[index] = from
    trough[index] = dmin
    end[index] = to
    list(return=draw, from=begin, trough = trough, to=end, length=(end-begin+1), peaktotrough = (trough-begin+1), recovery = (end-trough))

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
