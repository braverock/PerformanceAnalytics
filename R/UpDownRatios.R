`UpDownRatios` <-
function(Ra, Rb, method = c("capture","number","percent"), side = c("up","down"))
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
    #     that the investment outperformed the benchmark when the benchmark was
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

    method = method[1]
    side = side[1]

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols) 
    # @todo: expand.grid(1:3,c("up","down"),1:2)
    # deliver a list by benchmark with 'up' and 'down' statistics together

    udr <-function (Ra, Rb, method, side)
    {
        merged.assets = na.omit(merge(as.xts(Ra), as.xts(Rb)))

        if(method == "capture" & side == "up") {
            UpRa = subset(merged.assets[,1], merged.assets[,2] > 0)
            UpRb = subset(merged.assets[,2], merged.assets[,2] > 0)
            cumRa = sum(UpRa)
            cumRb = sum(UpRb)
            result = cumRa/cumRb
        }
        if(method == "capture" & side == "down") {
            DnRa = subset(merged.assets[,1], merged.assets[,2] <= 0)
            DnRb = subset(merged.assets[,2], merged.assets[,2] <= 0)
            cumRa = sum(DnRa)
            cumRb = sum(DnRb)
            result = cumRa/cumRb
        }

        if(method == "number" & side == "up") {
            UpRi = length(subset(merged.assets[,1], (merged.assets[,1] > 0) & (merged.assets[,2] > 0)))
            UpRb = length(subset(merged.assets[,2], merged.assets[,2] > 0))
            result = UpRi/UpRb
        }
        if(method == "number" & side == "down") {
            DnRi = length(subset(merged.assets[,1], (merged.assets[,1] < 0) & (merged.assets[,2] < 0)))
            DnRb = length(subset(merged.assets[,2], merged.assets[,2] < 0))
            result = DnRi/DnRb
        }

        if(method == "percent" & side == "up") {
            UpRi = length(subset(merged.assets[,1], (merged.assets[,1] > merged.assets[,2]) & (merged.assets[,2] > 0)))
            UpRb = length(subset(merged.assets[,2], merged.assets[,2] > 0))
            result = UpRi/UpRb
        }
        if(method == "percent" & side == "down") {
            DnRi = length(subset(merged.assets[,1], (merged.assets[,1] > merged.assets[,2]) & (merged.assets[,2] < 0)))
            DnRb = length(subset(merged.assets[,2], merged.assets[,2] < 0))
            result = DnRi/DnRb
        }
        return(result)
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb, method, side) udr(Ra[,n[1]], Rb[,n[2]], method, side), Ra = Ra, Rb = Rb, method = method, side = side)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = colnames(Rb)
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: UpDownRatios.R,v 1.8 2009-10-01 03:07:37 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2009-04-07 22:15:25  peter
# - removed unused dot dot dot
#
# Revision 1.6  2009-04-01 14:02:49  peter
# - fixed number ratio and added percentage ratio
#
# Revision 1.5  2008-10-16 18:45:37  brian
# - use checkData with method="zoo" instead of checkDataMatrix
#
# Revision 1.4  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.3  2007/04/02 21:57:26  peter
# - modified to use checkData functions
# - uses zoo to align dates
# - changed calculations to sum returns
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################