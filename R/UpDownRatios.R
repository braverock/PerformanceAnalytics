`UpDownRatios` <-
function(R, Rb, method = "capture", side = "up", ...)
{# @author Peter Carl

    # DESCRIPTION:
    # This is a function designed to calculate several related metrics:
    #   Up (Down) Capture Ratio: this is a measure of an investment's compound
    #     return when the benchmark was up (down) divided by the benchmark's
    #     compound return when the benchmark was up (down). The greater (lower)
    #     the value, the better.
    #   Up (Down) Number Ratio: similarly, this is a measure of the number of
    #     periods that the investment was up (down) when the benchmark was up (down),
    #     divided by the number of periods that the Benchmark was up (down).
    #   Up (Down) Percentage Ratio: this is a measure of the number of periods
    #     that the investment outperformed the benchmark when the benchmark was
    #     up (down), divided by the number of periods that the benchmark was up (down).
    #     Unlike the prior two metrics, in both cases a higher value is better.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of regular period returns.
    # method: "capture", "number", or "percentage" to indicate which measure
    #     to return.
    # side: "Up" or "Down" statistic.

    # Outputs:
    # A data.table of n-period trailing calculations for each column
    # in x.

    # FUNCTION:
    assetReturns.vec = checkDataVector(R)
    benchmarkReturns.vec = checkDataVector(Rb)

    UpRi = subset(assetReturns.vec, assetReturns.vec > 0)
    DownRi = subset(assetReturns.vec, assetReturns.vec < 0)
    UpRb = subset(benchmarkReturns.vec, benchmarkReturns.vec > 0)
    DownRb = subset(benchmarkReturns.vec, benchmarkReturns.vec > 0)

    if(method == "capture" & side == "up") {
        UpRi = subset(assetReturns.vec, benchmarkReturns.vec > 0)
        UpRb = subset(benchmarkReturns.vec, benchmarkReturns.vec > 0)
        cumRi = prod(1+UpRi) - 1
        cumRb = prod(1+UpRb) - 1
        result = cumRi/cumRb
    }
    if(method == "capture" & side == "down") {
        DnRi = subset(assetReturns.vec, benchmarkReturns.vec < 0)
        DnRb = subset(benchmarkReturns.vec, benchmarkReturns.vec < 0)
        cumRi = prod(1+DnRi) - 1
        cumRb = prod(1+DnRb) - 1
        result = cumRi/cumRb
    }
    if(method == "number" & side == "up") {
        UpRi = length(subset(assetReturns.vec, benchmarkReturns.vec > 0))
        UpRb = length(subset(benchmarkReturns.vec, benchmarkReturns.vec > 0))
        result = UpRi/UpRb
    }
    if(method == "number" & side == "down") {
        DnRi = length(subset(assetReturns.vec, benchmarkReturns.vec < 0))
        DnRb = length(subset(benchmarkReturns.vec, benchmarkReturns.vec < 0))
        result = DnRi/DnRb
    }
    if(method == "percentage" & side == "up") {
        UpRi = length(subset(assetReturns.vec, (benchmarkReturns.vec > 0) && (benchmarkReturns.vec < assetReturns.vec)))
        UpRb = length(subset(benchmarkReturns.vec, benchmarkReturns.vec > 0))
        result = UpRi/UpRb
    }
    if(method == "percentage" & side == "down") {
        DnRi = length(subset(assetReturns.vec, (benchmarkReturns.vec < 0) && (benchmarkReturns.vec < assetReturns.vec)))
        DnRb = length(subset(benchmarkReturns.vec, benchmarkReturns.vec < 0))
        result = DnRi/DnRb
    }
    result
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: UpDownRatios.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################