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
    sofar = drawdowns[1]
    to = 1
    dmin = 1

    for (i in 1:length(drawdowns)) { #2:length()
        thisSign <- ifelse(drawdowns[i] < 0, 0, 1)
        if (thisSign == priorSign) { ###
            if(drawdowns[i]< sofar) {
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
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################