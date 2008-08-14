`chart.VaRSensitivity` <-
function (R, methods = c("GaussianVaR", "ModifiedVaR", "HistoricalVaR"), clean=c("none", "boudt"), elementcolor="darkgray", reference.grid=TRUE, xlab = "Confidence Level", ylab="Value at Risk", type = "l", lty = c(1,2,4), lwd = 1, colorset = (1:12), pch = (1:12), legend.loc = "bottomleft", cex.legend = 0.8, main=NULL,...)
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
                        risk[i, j] = -1* as.numeric(VaR.CornishFisher(na.omit(R[,column,drop=FALSE]), p = p[i], modified = FALSE, clean=clean))
                        if(i==1)
                            legend.txt = c(legend.txt, "Gaussian VaR")

                    },
                    ModifiedVaR = {
                        risk[i, j] = -1* as.numeric(VaR.CornishFisher(na.omit(R[,column,drop=FALSE]), p = p[i], modified = TRUE, clean=clean))
                        if(i==1)
                            legend.txt = c(legend.txt, "Modified VaR")
                    },
                    HistoricalVaR = {
                        risk[i,j] =  quantile(na.omit(as.vector(R[,column,drop=FALSE])), probs = (1-p[i])) #hVaR = quantile(x,probs=.01)
                        if(i==1)
                            legend.txt = c(legend.txt, "Historical VaR")

                    }
                ) # end switch
            }

         } # end method loop
    } # end column loop
# print(risk)

    risk.columns = ncol(risk)
    ylim=c(min(risk),max(risk))
    xlim=c(min(p), max(p))
    if(is.null(main))
        main=paste("VaR Sensitivity of ", columnnames[1], sep="")
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
    axis(1, lab=p, at=p, col = elementcolor) # at= 1/(1:length(p))
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
