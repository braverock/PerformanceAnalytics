`table.Correlation` <-
function (x, y, n, trim = TRUE, na.rm = FALSE, ...)
{# @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating correlation and significance against
    # each column of the data provided.

    # Inputs:
    # Assumes that x and y are sequenced exactly the same.

    # Output:
    # A data table of correlation and corresponding p-value for each column
    # in y.

    # FUNCTION:

    # Prepare the data

    # target.vec is the vector of data we want correlations for; we'll get it
    # from x
    target.vec = checkDataVector(x)
    # data.matrix is a vector or matrix of data we want correlations against;
    # we'll take it from y
    data.matrix = checkDataMatrix(y)
    columns=ncol(y)
    columnnames = colnames(y)

    #  For each period (or row):
    for (column in 1:columns) {
        values = vector('numeric', 0)

        # Calculate correlation and significance
        htest = cor.test(x, y[,column], ...)
        values = cbind(htest$estimate, htest$p.value,htest$conf.int[1],htest$conf.int[2])

        if(column == 1) {
            result.df = data.frame(Value = values)#, row.names = columnnames[column])
        }

        else {
            nextrow = data.frame(Value = values)#, row.names = columnnames[column])
            result.df = rbind(result.df, nextrow)
        }
    }

#     if (is.timeSeries(x))
#         TS = TRUE
#     else TS = FALSE
#     if (TS) {
#         positions = x.orig@positions
#         x = x.orig@Data
#     }
#     else {
#         x = as.vector(x.orig)
#         names(x) = NULL
#     }
#     if (na.rm) {
#         if (TS)
#             positions = positions[!is.na(x)]
#         x = as.vector(na.omit(x))
#     }

#     if (!trim) {
#         result.df = c(rep(NA, (n - 1)), result.df)
#         rownames(result.df) = rownames
#     }
#     else
#    rownames(result.df) = rownames(n:rows)
    colnames(result.df) = c("Correlation","p-value","Lower CI","Upper CI")
    rownames(result.df) = columnnames

#     if (trim & TS)
#         positions = positions[-(1:(n - 1))]
#     if (TS) {
#         resultingtable = timeSeries(as.matrix(ans), positions, units = x.orig@units,
#             FinCenter = x.orig@FinCenter)
#     }
    result.df

    # Examples:
    # > table.Correlation(manager.ts@Data[,1],edhec.ts@Data,conf.level=.99)
    #                        Correlation      p-value    Lower CI  Upper CI
    # Convertible Arbitrage    0.3009975 0.0663078578 -0.12413495 0.6327630
    # CTA Global               0.2332791 0.1586799386 -0.19520250 0.5869820
    # Distressed Securities    0.4377104 0.0059910297  0.03398733 0.7186220
    # Emerging Markets         0.3847570 0.0170696005 -0.02975455 0.6863521
    # Equity Market Neutral    0.3453960 0.0336792979 -0.07504668 0.6615704
    # Event Driven             0.3985684 0.0131924564 -0.01344803 0.6948848
    # Fixed Income Arbitrage   0.5632395 0.0002317285  0.19946053 0.7905738
    # Funds of Funds           0.3558564 0.0283286932 -0.06317688 0.6682244
    # Global Macro             0.4470318 0.0048966026  0.04555842 0.7241800
    # Long/Short Equity        0.3330872 0.0410066948 -0.08886284 0.6536764
    # Merger Arbitrage         0.3385546 0.0376066332 -0.08274587 0.6571915
    # Relative Value           0.4781920 0.0023928191  0.08503719 0.7425018
    # Short Selling           -0.2585501 0.1170322982 -0.60433520 0.1691977

    # > ctable = table.Correlation(manager.ts@Data[,1],edhec.ts@Data,conf.level=.99)
    # > dotchart(ctable[,1],labels=rownames(ctable),xlim=c(-1,1))

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.Correlation.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################