table.UpDownRatios <-
function (Ra, Rb, digits = 4)
{# @author Peter Carl

    # FUNCTION:

    Ra = checkData(Ra)
    Rb = checkData(Rb)

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    result.df = data.frame(NULL)

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        for(column.b in 1:columns.b) { # against each asset passed in as Rb
            row.df = data.frame(NULL)
            merged.assets = merge(Ra[,column.a,drop=FALSE], Rb[,column.b,drop=FALSE])
            merged.assets = na.omit(merged.assets) 

            UpCapture = UpDownRatios(Ra=merged.assets[,1], Rb=merged.assets[,2], method="Capture", side="Up")
            DnCapture = UpDownRatios(Ra=merged.assets[,1], Rb=merged.assets[,2], method="Capture", side="Down")
            UpPeriods = UpDownRatios(Ra=merged.assets[,1], Rb=merged.assets[,2], method="Number", side="Up")
            DnPeriods = UpDownRatios(Ra=merged.assets[,1], Rb=merged.assets[,2], method="Number", side="Down")
            UpAverage = UpDownRatios(Ra=merged.assets[,1], Rb=merged.assets[,2], method="Percent", side="Up")
            DnAverage = UpDownRatios(Ra=merged.assets[,1], Rb=merged.assets[,2], method="Percent", side="Down")

            row.df = cbind(UpCapture, DnCapture, UpPeriods, DnPeriods, UpAverage, DnAverage)
            rownames(row.df) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
            result.df = rbind(result.df, row.df)
        }
    }

    colnames(result.df) = c("Up Capture", "Down Capture", "Up Number", "Down Number", "Up Percent", "Down Percent")

    result.df = base::round(result.df, digits)
    result.df
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