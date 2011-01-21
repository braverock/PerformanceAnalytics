UpDownRatios <-
function(Ra, Rb, method = c("Capture","Number","Percent"), side = c("Up","Down"))
{# @author Peter Carl

    # DESCRIPTION:
    # This is a function designed to calculate several related metrics:
    #   Up (Down) Capture Ratio: this is a measure of an investment's compound
    #     return when the benchmark was up (down) divided by the benchmark's
    #     compound return when the benchmark was up (down). The greater (lower)
    #     the value, the better.
    #   Up (Down) Numbers Ratio: similarly, this is a measure of the number of
    #     periods that the investment was up (down) when the benchmark was up (down),
    #     divided by the number of periods that the Benchmark was up (down).
    #   Up (Down) Percent Ratio: this is a measure of the number of periods
    #     that the investment outpeRformed the benchmark when the benchmark was
    #     up (down), divided by the number of periods that the benchmark was up (down).
    #     Unlike the prior two metrics, in both cases a higher value is better.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of regular period returns.
    # method: "capture", "number", or "percentage" to indicate which measure
    #     to return.
    # side: "Up" or "Down" statistic.

    # Outputs:
    # A data.table 

    # FUNCTION:

    Ra = checkData(Ra)
    Rb = checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, side, 1:Rb.ncols, method) 

    # @todo: expand.grid(1:3,c("up","down"),1:2)
    # deliver a list by benchmark with 'up' and 'down' statistics together

    udr <-function (Ra, Rb, method, side)
    {
        merged.assets = na.omit(merge(Ra, Rb))

        if(method == "Capture" & side == "Up") {
            UpRa = subset(merged.assets[,1], merged.assets[,2] > 0)
            UpRb = subset(merged.assets[,2], merged.assets[,2] > 0)
            cumRa = sum(UpRa)
            cumRb = sum(UpRb)
            result = cumRa/cumRb
        }
        if(method == "Capture" & side == "Down") {
            DnRa = subset(merged.assets[,1], merged.assets[,2] <= 0)
            DnRb = subset(merged.assets[,2], merged.assets[,2] <= 0)
            cumRa = sum(DnRa)
            cumRb = sum(DnRb)
            result = cumRa/cumRb
        }

        if(method == "Number" & side == "Up") {
            UpRi = length(subset(merged.assets[,1], (merged.assets[,1] > 0) & (merged.assets[,2] > 0)))
            UpRb = length(subset(merged.assets[,2], merged.assets[,2] > 0))
            result = UpRi/UpRb
        }
        if(method == "Number" & side == "Down") {
            DnRi = length(subset(merged.assets[,1], (merged.assets[,1] < 0) & (merged.assets[,2] < 0)))
            DnRb = length(subset(merged.assets[,2], merged.assets[,2] < 0))
            result = DnRi/DnRb
        }

        if(method == "Percent" & side == "Up") {
            UpRi = length(subset(merged.assets[,1], (merged.assets[,1] > merged.assets[,2]) & (merged.assets[,2] > 0)))
            UpRb = length(subset(merged.assets[,2], merged.assets[,2] > 0))
            result = UpRi/UpRb
        }
        if(method == "Percent" & side == "Down") {
            DnRi = length(subset(merged.assets[,1], (merged.assets[,1] > merged.assets[,2]) & (merged.assets[,2] < 0)))
            DnRb = length(subset(merged.assets[,2], merged.assets[,2] < 0))
            result = DnRi/DnRb
        }
        return(result)
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb, method, side) udr(Ra[,as.numeric(n[1])], Rb[,as.numeric(n[3])], method = n[4], side= n[2]), Ra = Ra, Rb = Rb)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols*length(side)*length(method))

        rownames(result) = colnames(Ra)
        n = expand.grid(side, colnames(Rb), method)
        colnames = apply(n, 1, FUN = function(n) paste(n[2], n[1],n[3]))
        colnames(result) = colnames
        return(t(result))
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2011 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################