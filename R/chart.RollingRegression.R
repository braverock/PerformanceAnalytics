`chart.RollingRegression` <-
function (R, Rb, n, attribute = "Beta", main = paste("Rolling ",n,"-Month ",attribute,sep=""), xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # Rb: a matrix, data frame, or timeSeries of returns for a benchmark

    # Outputs:
    # A timeseries line chart of the calculated series

    # FUNCTION:

    # Transform input data to a data frame

    # @todo: These should be excess returns
    x = checkDataMatrix(R)
    y = checkDataMatrix(Rb)

    xcolname = colnames(x)
    colnames = colnames(y)

    columns = ncol(y)
    rows = nrow(x)
    rownames = rownames(x)

    data = as.data.frame(cbind(x[,1],y),row.names = rownames)

    if(attribute == "Beta")
        slot = 2
    if(attribute == "Alpha")
        slot = 1
    if(attribute == "R-Squared")
        slot = 2

    for(column in 2:(columns+1)) {
        result = rollingRegression(data[,1]~data[,column], data = data, width = n)
        if(column == 2) {
            if(attribute == "Beta")
                betas.df = data.frame(result$coefficients[2,],row.names = rownames[n:rows])
            if(attribute == "Alpha")
                betas.df = data.frame(result$coefficients[1,],row.names = rownames[n:rows])
            if(attribute == "R-Squared")
                betas.df = data.frame(result$r.squared,row.names = rownames[n:rows])
        }
        else {
            if(attribute == "Beta")
                nextcolumn = as.data.frame(result$coefficients[2,])
            if(attribute == "Alpha")
                nextcolumn = as.data.frame(result$coefficients[1,])
            if(attribute == "R-Squared")
                nextcolumn = as.data.frame(result$r.squared)
            betas.df = cbind(betas.df, nextcolumn)
        }

    }

    colnames(betas.df) = colnames

    chart.TimeSeries(betas.df, xaxis = xaxis, main = main, legend.loc = legend.loc, col = colorset, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.RollingRegression.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################