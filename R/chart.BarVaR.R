`chart.BarVaR` <-
function (R, n = 0, risk.line = TRUE, method = "ModifiedVaR", reference.grid = TRUE, xaxis = TRUE, main = "Title", ylab="Value", xlab="Date", date.format = "%m/%y", xlim = NA, ylim = NA, lwd = 1, colorset =(1:12), p=.99,...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of monthly returns in a bar chart.  This is
    # a difficult enough graph to read that it doesn't get much use.  Still,
    # it is useful for viewing a single set of data.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # n: periods specified for rolling-period calculations

    # Outputs:
    # A timeseries bar chart of the data series
    # VaR and Std Dev with n=0 are calculated from the start of the timeseries

    # FUNCTION:

    # Transform input data to a matrix
    x = checkDataMatrix(R)

    # Set up dimensions and labels
    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = rownames(x)

    # Assume we're passed in a series of monthly returns.  First, we'll
    # calculate VaR or modified VaR
    #risk = as.matrix(rollingFunction(y, n = n, trim = FALSE, FUN = "VaR.CornishFisher"))

    if(risk.line){
        if(method == "StdDev"){
            # @todo: fix this nasty coersion
            #risk = as.matrix(rollingFunction(as.data.frame(y[,1]), n = n, trim = FALSE, FUN = "stdev"))
            risk = as.matrix(rollingFunction(as.data.frame(x[,1]), n = n, trim = FALSE, FUN = "sd"))
            symmetric = TRUE
            if(n>0)
                legend.txt = paste("Rolling ",n,"-month Std Dev",sep="")
            else
                legend.txt = "Std Dev"
        }
        if(method == "VaR"){
            risk = as.matrix(rollingFunction(as.data.frame(x[,1]), n = n, trim = FALSE, FUN = "VaR", p = p, modified = FALSE))
            symmetric = TRUE
            if(n>0)
                legend.txt = paste("Rolling ",n,"-Month VaR (1 Mo, ",p*100,"%)",sep="")
            else
                legend.txt = paste("Traditional VaR (1 Mo, ",p*100,"%)",sep="")
        }
        if(method == "ModifiedVaR"){
            risk = as.matrix(rollingFunction(as.data.frame(x[,1]), n = n, trim = FALSE, FUN = "VaR.CornishFisher", p = p))
            symmetric = FALSE
            if(n>0)
                legend.txt = paste("Rolling ",n,"-Month Modified VaR (1 Mo, ",p*100,"%)",sep="")
            else
                legend.txt = paste("Modified VaR (1 Mo, ",p*100,"%)",sep="")
        }
    }
    else {
        risk = 0
        legend.txt = ""
    }
    if(is.na(ylim[1])){
        ylim = range(c(x[!is.na(x[,1]),1],risk[!is.na(risk)],-risk[!is.na(risk)]))
    }

    chart.TimeSeries(x[,1], type = "h", col = colorset, legend.loc = NULL, ylim = ylim, reference.grid = reference.grid, xaxis = xaxis, main = main, ylab = ylab, xlab = xlab, lwd = lwd, lend="butt", ...)

    if(risk.line){
        if (symmetric)
            lines(1:rows, risk, col = colorset[1], lwd = 1, type = "l", lty="13")
        lines(1:rows, -risk, col = colorset[1], lwd = 1, type = "l", lty="13")
    }

    if(legend.txt != "")
        legend("bottomleft", inset = 0.02, text.col = colorset[1], col = colorset[1], cex = .8, border.col = "grey", lwd = 1, lty="13", bty = "n", legend = legend.txt)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.BarVaR.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################