#' show the sensitivity of Value-at-Risk or Expected Shortfall estimates
#' 
#' Creates a chart of Value-at-Risk and/or Expected Shortfall estimates by
#' confidence interval for multiple methods.
#' 
#' This chart shows estimated VaR along a series of confidence intervals for
#' selected calculation methods.  Useful for comparing a method to the
#' historical VaR calculation.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param methods one or more calculation methods indicated "GaussianVaR",
#' "ModifiedVaR", "HistoricalVaR", "GaussianES", "ModifiedES", "HistoricalES".
#' See \code{\link{VaR}} or \code{\link{ES}} for more detail.
#' @param clean method for data cleaning through \code{\link{Return.clean}}.
#' Current options are "none" or "boudt" or "geltner".
#' @param elementcolor the color used to draw chart elements. The default is
#' "darkgray"
#' @param reference.grid if true, draws a grid aligned with the points on the x
#' and y axes
#' @param ylab set the y-axis label, same as in \code{\link{plot}}
#' @param xlab set the x-axis label, same as in \code{\link{plot}}
#' @param ylim set the y-axis dimensions, same as in \code{\link{plot}}
#' @param type set the chart type, same as in \code{\link{plot}}
#' @param lty set the line type, same as in \code{\link{plot}}
#' @param lwd set the line width, same as in \code{\link{plot}}
#' @param colorset color palette to use, set by default to rational choices
#' @param pch symbols to use, see also \code{\link{plot}}
#' @param legend.loc places a legend into one of nine locations on the chart:
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#' center.
#' @param cex.legend The magnification to be used for sizing the legend
#' relative to the current setting of 'cex'.
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso \code{\link{VaR}} \cr \code{\link{ES}}
#' @references Boudt, K., Peterson, B. G., Croux, C., 2008. Estimation and
#' Decomposition of Downside Risk for Portfolios with Non-Normal Returns.
#' Journal of Risk, forthcoming.
###keywords ts multivariate distribution
#' @examples
#' 
#' data(managers)
#' chart.VaRSensitivity(managers[,1,drop=FALSE], 
#' 		methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"), 
#' 		colorset=bluefocus, lwd=2)
#' 
#' @export
chart.VaRSensitivity <-
function (R, methods = c("GaussianVaR", "ModifiedVaR", "HistoricalVaR","GaussianES", "ModifiedES", "HistoricalES"), clean=c("none", "boudt", "geltner"), elementcolor="darkgray", reference.grid=TRUE, xlab = "Confidence Level", ylab="Value at Risk", type = "l", lty = c(1,2,4), lwd = 1, colorset = (1:12), pch = (1:12), legend.loc = "bottomleft", cex.legend = 0.8, main=NULL, ylim=NULL, ...)
{ # @author Peter Carl

    R = checkData(R)
    columnnames = colnames(R)
    clean = clean[1]
    legend.txt = NULL
    if(length(methods) > 1){
        columns=1
    }
    p = seq(0.99,0.89,by=-0.005)
    risk = matrix(nrow=length(p), ncol=length(methods),dimnames=list(p,methods))

    for(column in 1:columns) {
         for(j in 1:length(methods)) {
            for(i in 1:length(p)){
                switch(methods[j],
                    GaussianVaR = {
                        risk[i, j] =  as.numeric(VaR(na.omit(R[,column,drop=FALSE]), p = p[i], method="gaussian", clean=clean))
                        if(i==1)
                            legend.txt = c(legend.txt, "Gaussian VaR")

                    },
                    ModifiedVaR = {
                        risk[i, j] =  as.numeric(VaR(na.omit(R[,column,drop=FALSE]), p = p[i], method="modified", clean=clean))
                        if(i==1)
                            legend.txt = c(legend.txt, "Modified VaR")
                    },
                    HistoricalVaR = {
                        risk[i,j] =   as.numeric(VaR(na.omit(R[,column,drop=FALSE]), p = p[i], method="historical", clean=clean)) #hVaR = quantile(x,probs=.01)
                        if(i==1)
                            legend.txt = c(legend.txt, "Historical VaR")

                    },
                    GaussianES = {
                        risk[i, j] =  as.numeric(ES(na.omit(R[,column,drop=FALSE]), p = p[i], method="gaussian", clean=clean))
                        if(i==1)
                            legend.txt = c(legend.txt, "Gaussian ES")

                    },
                    ModifiedES = {
                        risk[i, j] =  as.numeric(ES(na.omit(R[,column,drop=FALSE]), p = p[i], method="modified", clean=clean))
                        if(i==1)
                            legend.txt = c(legend.txt, "Modified ES")
                    },
                    HistoricalES = {
                        risk[i,j] =   as.numeric(ES(na.omit(R[,column,drop=FALSE]), p = p[i], method="historical", clean=clean)) 
                        if(i==1)
                            legend.txt = c(legend.txt, "Historical ES")

                    }
                ) # end switch
            }

         } # end method loop
    } # end column loop
# print(risk)

    risk.columns = ncol(risk)
    if(is.null(ylim))
        ylim=c(min(risk),max(risk))
    xlim=c(min(p), max(p))
    if(is.null(main))
        main=paste("Risk Confidence Sensitivity of ", columnnames[1], sep="")
    if(length(lwd) < risk.columns)
        lwd = rep(lwd,risk.columns)
    if(length(lty) < risk.columns)
        lty = rep(lty,risk.columns)
    if(length(pch) < risk.columns)
        pch = rep(pch,risk.columns)
    plot.new()
    plot.window(xlim, ylim, xaxs = "r")
    if (reference.grid) {
        grid(col = elementcolor)
    }
    for(risk.column in risk.columns:1) {
        lines(p,risk[,risk.column], col = colorset[risk.column], lwd = lwd[risk.column], pch = pch[risk.column], lty = lty[risk.column], type = type, ...)
    }

    # draw x-axis
    axis(1, labels=p, at=p, col = elementcolor) # at= 1/(1:length(p))
    title(xlab = xlab)

    # set up y-axis
    axis(2, col=elementcolor)
    box(col = elementcolor)

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = cex.legend, border.col = elementcolor, lty = lty, lwd = 2, bg = "white", legend = legend.txt)
    }

    # Add the other titles
    if(is.null(main))
        main=columnnames[1]
    title(ylab = ylab)
    title(main = main)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
