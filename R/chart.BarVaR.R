`chart.BarVaR` <-
function (R, width = 0, gap = 12, risk.line = TRUE, method = c("ModifiedVaR","VaR","StdDev"), reference.grid = TRUE, xaxis = TRUE, main = "Title", ylab="Value", xlab="Date", date.format = "%m/%y", xlim = NA, ylim = NA, lwd = 1, colorset =(1:12), p=.99, lty = "13", all = FALSE, ...)
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
    #x = na.omit(x)

    # Set up dimensions and labels
    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = rownames(x)
    method = method[1] # grab the first value if this is still a vector, to avoid varnings

    risk = zoo(0)

    if (!all)
        columns = 1

    if(risk.line){
        for(column in 1:columns) {
            switch(method,
                StdDev = {
                    symmetric = TRUE
                    if(width > 0){
                        column.risk = apply.rolling(na.omit(x[,column,drop=FALSE]), width = width, FUN = "sd")
                        legend.txt = paste("Rolling ",width,"-month Std Dev",sep="")
                    }
                    else {
                        column.risk = apply.fromstart(na.omit(x[,column,drop=FALSE]), gap = gap, FUN = "sd")
                        legend.txt = "Std Dev"
                    }
                },
                VaR = {
                    symmetric = TRUE
                    if(width > 0) {
                        column.risk = apply.rolling(na.omit(x[,column,drop=FALSE]), width = width, FUN = "VaR.CornishFisher", p = p, modified = FALSE)
                        legend.txt = paste("Rolling ",width,"-Month VaR (1 Mo, ",p*100,"%)",sep="")
                    }
                    else {
                        column.risk = apply.fromstart(na.omit(x[,column,drop=FALSE]), gap = gap, FUN = "VaR.CornishFisher", p = p, modified = FALSE)
                        legend.txt = paste("Traditional VaR (1 Mo, ",p*100,"%)",sep="")
                    }
                },
                ModifiedVaR = {
                    symmetric = FALSE
                    if(width > 0) {
                        column.risk = apply.rolling(na.omit(x[,column,drop=FALSE]), width = width, FUN = "VaR.CornishFisher", p = p, modified = TRUE)
                        legend.txt = paste("Rolling ",width,"-Month Modified VaR (1 Mo, ",p*100,"%)",sep="")
                    }
                    else {
                        column.risk = apply.fromstart(na.omit(x[,column,drop=FALSE]), gap = gap, FUN = "VaR.CornishFisher", p = p, modified = TRUE)
                        legend.txt = paste("Modified VaR (1 Mo, ",p*100,"%)",sep="")
                    }
                }
            ) # end switch
        if(column == 1)
            risk = merge(x[,1],column.risk)
        else
            risk = merge(risk,column.risk)
        }
    }
    else {
        risk = 0
        legend.txt = ""
    }
    if(is.na(ylim[1])){
        ylim = range(c(na.omit(as.vector(x[,1])), na.omit(as.vector(risk)), -na.omit(as.vector(risk))))
    }

    chart.TimeSeries(x[,1, drop=FALSE], type = "h", col = colorset, legend.loc = NULL, ylim = ylim, reference.grid = reference.grid, xaxis = xaxis, main = main, ylab = ylab, xlab = xlab, lwd = lwd, lend="butt", ...)

    if(risk.line){
        if (symmetric){
            for(column in (columns+1):2) {
                lines(1:rows, risk[,column], col = colorset[column-1], lwd = 1, type = "l", lty=lty)
            }
        }
        for(column in (columns+1):2) {
            lines(1:rows, -risk[,column], col = colorset[column-1], lwd = 1, type = "l", lty=lty)
        }
    }

    if(legend.txt != "")
        legend("bottomleft", inset = 0.02, text.col = colorset, col = colorset, cex = .8, border.col = "grey", lwd = 1, lty=lty, bty = "n", legend = legend.txt)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.BarVaR.R,v 1.12 2007-06-25 04:15:59 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.11  2007/06/18 03:33:07  brian
# - use switch for method argument, more efficient
#
# Revision 1.10  2007/04/21 14:07:25  peter
# - added 'all' flag: when F only draws first column
#
# Revision 1.9  2007/04/21 01:06:01  peter
# - creates risk lines for each column of data
#
# Revision 1.8  2007/04/13 22:45:18  peter
# - changed how na.omit is applied
#
# Revision 1.7  2007/04/02 21:52:46  peter
# - added removal of NA's
#
# Revision 1.6  2007/03/21 20:49:04  peter
# - fixed issue with ylim when passed a matrix-like object
#
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
