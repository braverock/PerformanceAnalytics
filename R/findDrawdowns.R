`findDrawdowns` <-
function (R)
{ # @author Peter Carl

    # modified with permission from function by Sankalp Upadhyay
    # <sankalp.upadhyay [at] gmail [dot] com>

    # DESCRIPTION:
    # Find the drawdowns in a timeseries.
    # Find the starting period, the ending period, and the amount and length
    # of the drawdown.

    # FUNCTION:

    x = checkData(R, method="vector") # matrix?

    cumulativeReturn = cumprod(1+x)
    maxCumulativeReturn = cummax(cumulativeReturn)
    drawdowns = cumulativeReturn/maxCumulativeReturn - 1
    # if you want to see the drawdown series, plot(drawdown,type="l")

    draw = c()
    begin = c()
    end = c()
    length = c()
    trough = c()
    index = 1
    if (drawdowns[1] >= 0)
        priorSign = 1
    else
        priorSign = 0
    from = 1
    sofar = drawdowns[1]
    to = 1
    dmin = 1
    for (i in 2:length(drawdowns)) {
        thisSign <- ifelse(drawdowns[i] < 0, 0, 1)
        if (thisSign == priorSign) {
            if(drawdowns[i]< sofar) {
                sofar = drawdowns[i]
                dmin = i
            }
            to = i
        }
        else {
    # @todo: add trough date, time to trough, recovery time
            draw[index] = sofar
            begin[index] = from
            trough[index] = dmin
            end[index] = to +1
            #cat(sofar, from, to)
            from = i 
            sofar = drawdowns[i]
            to = i
            dmin = i +1
            index = index + 1
            priorSign = thisSign
        }
    }
    draw[index] = sofar
    begin[index] = from
    trough[index] = dmin +1
    end[index] = to + 1
    list(return=draw, from=begin, trough = trough, to=end, length=end-begin, peaktotrough = trough-begin, recovery = end-trough)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: findDrawdowns.R,v 1.4 2007-03-21 14:07:38 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.2  2007/02/06 11:58:42  brian
# - standardize attribution for Drawdown runs
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################