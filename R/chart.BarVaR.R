chart.BarVaR <- function (R, width = 0, gap = 12, methods = c("none", "ModifiedVaR", "GaussianVaR", "HistoricalVaR", "StdDev", "ModifiedES", "GaussianES", "HistoricalES"), p=0.95, clean = c("none", "boudt","geltner"), all = FALSE, ..., show.clean = FALSE, show.horizontal = FALSE, show.symmetric = FALSE, show.endvalue = FALSE, show.greenredbars = FALSE, legend.loc="bottomleft", ylim = NA, lwd = 2, colorset = 1:12, lty = c(1,2,4,5,6), ypad=0, legend.cex = 0.8 )
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
    x = checkData(R)

    # Set up dimensions and labels
    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = time(x)
    legend.txt = NULL
    symmetric = NULL
    risk.line=TRUE

    freq = periodicity(x)

    switch(freq$scale,
        minute = {freq.lab = "minute"},
        hourly = {freq.lab = "hour"},
        daily = {freq.lab = "day"},
        weekly = {freq.lab = "week"},
        monthly = {freq.lab = "month"},
        quarterly = {freq.lab = "quarter"},
        yearly = {freq.lab = "year"}
    )

    time(x) = as.Date(time(x)) # this is here because merge.zoo is not behaving as expected when date formats are not consistent
    if(methods[1]=="none"){
        methods=NULL
        risk.line=FALSE
    }

    colors = colorRamp(c(colorset[1],"white"))
    if(length(methods)>1){
        columns = 1 # if there's more than one method specified, then we'll ignore columns other than the first
        colorset = c(colorset[1], rgb(colors(.25),max=255), rgb(colors(.5),max=255), rgb(colors(.75),max=255))
    }
    clean = clean[1]

    risk = xts(rep(NA,length(time(x))),order.by=time(x))
    column.risk = xts(rep(0,length(time(x))),order.by=time(x))

    if (!all)
        columns = 1

    bar.color = colorset[1]
    if (show.clean){
        bar.color = rgb(colors(.75),max=255)
    }

    x.orig = x

    if(show.clean){
        x = na.skip(x, Return.clean, method=clean) 
    }

    if(risk.line){
        for(column in 1:columns) {
            for(method in methods) {
                switch(method,
                    StdDev = {
                        symmetric = c(symmetric, TRUE)
                        if(width > 0){
                            column.risk = -1 * apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "sd")
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-",freq.lab," Std Dev",sep=""))
                        }
                        else {
                            column.risk = -1 * apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "sd")
                            if(column==1)
                                legend.txt = c(legend.txt, "Std Dev")
                        }
                    },
                    GaussianVaR = {
                        symmetric = c(symmetric, TRUE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "VaR", p = p, method="gaussian", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab," Gaussian VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "VaR", p = p, method="gaussian", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Gaussian VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    },
                    ModifiedVaR = {
                        symmetric = c(symmetric, FALSE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "VaR", p = p, method="modified", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab, " Modified VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "VaR", p = p, method="modified", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Modified VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    },
                    HistoricalVaR = {
                        symmetric = c(symmetric, FALSE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "VaR", p = p, method="historical") #hVaR = quantile(x,probs=.01)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab," Historical VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "VaR", p = p, method="historical")
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Historical VaR (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    },
                    GaussianES = {
                        symmetric = c(symmetric, TRUE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "ES", p = p, method="gaussian", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab," Gaussian ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "ES", p = p, method="gaussian", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Gaussian ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    },
                    ModifiedES = {
                        symmetric = c(symmetric, FALSE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "ES", p = p, method="modified", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab, " Modified ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "ES", p = p, method="modified", clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Modified ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    },
                    HistoricalES = {
                        symmetric = c(symmetric, FALSE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "ES", p = p, method="historical") #hES = quantile(x,probs=.01)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-", freq.lab," Historical ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "ES", p = p, method="historical")
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Historical ES (1 ", freq.lab, ", ",base::round(p,4)*100,"%)",sep=""))
                        }
                    }
                ) # end switch

                risk = merge(risk,column.risk)
            } # end method loop
        } # end column loop
    }
    else {
        risk = 0
        legend.txt = ""
    }
    if(is.na(ylim[1])){
        ylim = range(c(na.omit(as.vector(x.orig[,1])), na.omit(as.vector(risk)), -na.omit(as.vector(risk))))
        ylim = c(ylim[1]-ypad,ylim[2]) # pad the bottom of the chart for the legend
    }
    if(!show.greenredbars){
	chart.TimeSeries(x.orig[,1, drop=FALSE], type = "h", col = bar.color, legend.loc = NULL, ylim = ylim, lwd = lwd, lend="butt", ...)
    }
    else {
        positives = x.orig[,1,drop=FALSE]
        for(row in 1:length(x.orig[,1,drop=FALSE])){ 
            positives[row,]=max(0,x.orig[row,1])
        }
        negatives = x.orig[,1,drop=FALSE]
        for(row in 1:length(x.orig[,1,drop=FALSE])){ 
            negatives[row,]=min(0,x.orig[row,1])
        }
        chart.TimeSeries(positives, type = "h", legend.loc = NULL, ylim = ylim, lwd = lwd, lend="butt", col="darkgreen", ...)
        lines(1:length(x.orig[,1]), negatives, type="h", lend="butt", col="darkred", lwd=lwd)
    }

    if(show.clean) {
        lines(1:rows, x[,1, drop=FALSE], type="h", col=colorset[1], lwd = lwd, lend="butt")
    }

#     symmetric = symmetric[-1]
# add risk.columns here, since we'll want multiple lines when we have multiple methods
# if we have multiple methods, we'll want multiple lty's

    if(risk.line){
        risk.columns = ncol(risk)
        if(length(lty)==1)
            lty = rep(lty, risk.columns)
        for(column in (risk.columns):2) {
            if (show.symmetric && symmetric[column-1]){
                lines(1:rows, -risk[,column], col = colorset[column-1], lwd = 1, type = "l", lty=lty[column-1])
            }
        }
        for(column in (risk.columns):2) {
            lines(1:rows, risk[,column], col = colorset[column-1], lwd = 1, type = "l", lty=lty[column-1])
            if(show.horizontal)
                lines(1:rows, rep(tail(risk[,2],1),rows), col = colorset[1], lwd=1, type="l", lty=1)
	    if(show.endvalue){
		points(rows, tail(risk[,2],1), col = colorset[1], pch=20, cex=.7)
		mtext(paste(round(100*tail(risk[,2],1),2),"%", sep=""), line=.5, side = 4, at=tail(risk[,2],1), adj=0, las=2, cex = 0.7, col = colorset[1])
	    }
        }
    }

    if(legend.txt[1] != "" & !is.null(legend.loc))
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = legend.cex, border.col = "grey", lwd = 1, lty=lty, bty = "n", legend = legend.txt, horiz=TRUE)

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