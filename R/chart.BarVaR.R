`chart.BarVaR` <-
function (R, width = 0, gap = 1, risk.line = TRUE, method = "ModifiedVaR", reference.grid = TRUE, xaxis = TRUE, main = "Title", ylab="Value", xlab="Date", date.format = "%m/%y", xlim = NA, ylim = NA, lwd = 1, colorset =(1:12), p=.99,...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of monthly returns in a bar chart.  This is
    # a difficult enough graph to read that it doesn't get much use.  Still,
    # it is useful for viewing a single set of data.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # width: periods specified for rolling-period calculations

    # Outputs:
    # A timeseries bar chart of the data series
    # Metrics with width=0 are calculated from the start of the timeseries

    # FUNCTION:

    # Transform input data to a timeseries object
    x = checkData(R, method = "zoo")

    # Set up dimensions and labels
    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = rownames(x)

    if(risk.line){
        if(method == "StdDev"){
            symmetric = TRUE
            if(width > 0){
                risk = apply.rolling(x[,1,drop=FALSE], width = width, FUN = "sd")
                legend.txt = paste("Rolling ",width,"-month Std Dev",sep="")
            }
            else {
                risk = apply.fromstart(x[,1,drop=FALSE], gap = gap, FUN = "sd")
                legend.txt = "Std Dev"
            }
        }
        else if(method == "VaR"){
            symmetric = TRUE
            if(width > 0) {
                risk = apply.rolling(x[,1,drop=FALSE], width = width, FUN = "VaR.CornishFisher", p = p, modified = FALSE)
                legend.txt = paste("Rolling ",width,"-Month VaR (1 Mo, ",p*100,"%)",sep="")
            }
            else {
                risk = apply.fromstart(x[,1,drop=FALSE], gap = gap, FUN = "VaR.CornishFisher", p = p, modified = FALSE)
                legend.txt = paste("Traditional VaR (1 Mo, ",p*100,"%)",sep="")
            }
        }
        else if(method == "ModifiedVaR"){
            symmetric = FALSE
            if(width > 0) {
                risk = apply.rolling(x[,1,drop=FALSE], width = width, FUN = "VaR.CornishFisher", p = p, modified = TRUE)
                legend.txt = paste("Rolling ",width,"-Month Modified VaR (1 Mo, ",p*100,"%)",sep="")
            }
            else {
                risk = apply.fromstart(x[,1,drop=FALSE], gap = gap, FUN = "VaR.CornishFisher", p = p, modified = TRUE)
                legend.txt = paste("Modified VaR (1 Mo, ",p*100,"%)",sep="")
            }
        }
        else
            stop("Did not recognize the method, which should be one of \"StdDev\", \"VaR\", or \"ModifiedVaR\".")
    }
    else {
        risk = 0
        legend.txt = ""
    }
    if(is.na(ylim[1])){
        ylim = range(c(na.omit(as.vector(x[,1])), na.omit(as.vector(risk)), -na.omit(as.vector(risk))))
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
# $Id: chart.BarVaR.R,v 1.6 2007-03-21 20:49:04 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2007/03/20 10:44:46  brian
# - change F to FALSE to pass R CMD check
#
# Revision 1.4  2007/03/20 03:27:00  peter
# - uses apply.rolling and apply.fromstart to calculate risk lines
#
# Revision 1.3  2007/02/26 13:32:37  brian
# - change method VaR to pass "R CMD check"
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################