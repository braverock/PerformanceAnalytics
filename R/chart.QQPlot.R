`chart.QQPlot` <-
function(R, distribution="norm", ylab=NULL,
        xlab=paste(distribution, "Quantiles"), main=NULL, las=par("las"),
        envelope=FALSE, labels=FALSE, col=c(1,4), lwd=2, pch=1, cex=1,
        line=c("quartiles", "robust", "none"), elementcolor = "lightgray", ...)
#function (R, colorset = c("#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000"), symbolset = 1, xlab = NULL, ylab = NULL, main = NULL, darken = FALSE, distribution = "normal", line = TRUE, elementcolor = "lightgray", ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns

    # Outputs:
    # A Normal Q-Q Plot

    # FUNCTION:

    x = checkData(R, method = "vector", na.rm = TRUE)
#     n = length(x)

    if(is.null(main)){ 
        if(!is.null(colnames(R)[1])) 
            main=colnames(R)[1]
        else
            main = "QQ Plot"
    }
    if(is.null(ylab)) ylab = "Empirical Quantiles"
    # the core of this function is taken from John Fox's qq.plot, which is part of the car package
    result <- NULL
    line <- match.arg(line)
    good <- !is.na(x)
    ord <- order(x[good])
    ord.x <- x[good][ord]
    q.function <- eval(parse(text=paste("q",distribution, sep="")))
    d.function <- eval(parse(text=paste("d",distribution, sep="")))
    n <- length(ord.x)
    P <- ppoints(n)
    z <- q.function(P, ...)
    plot(z, ord.x, xlab=xlab, ylab=ylab, main=main, las=las, col=col[1], pch=pch,
        cex=cex, axes=FALSE)
    if (line=="quartiles"){
        Q.x<-quantile(ord.x, c(.25,.75))
        Q.z<-q.function(c(.25,.75), ...)
        b<-(Q.x[2]-Q.x[1])/(Q.z[2]-Q.z[1])
        a<-Q.x[1]-b*Q.z[1]
        abline(a, b, col=col[2], lwd=lwd)
        }
    if (line=="robust"){
        if (!require("MASS")) stop("MASS package not available")
        coef<-coefficients(rlm(ord.x~z))
        a<-coef[1]
        b<-coef[2]
        abline(a,b, col=col[2])
        }
    if (line != 'none' & envelope != FALSE) {
        zz<-qnorm(1-(1-envelope)/2)
        SE<-(b/d.function(z, ...))*sqrt(P*(1-P)/n)
        fit.value<-a+b*z
        upper<-fit.value+zz*SE
        lower<-fit.value-zz*SE
        lines(z, upper, lty=2, lwd=lwd/2, col=col[2])
        lines(z, lower, lty=2, lwd=lwd/2, col=col[2])
        }
    if (labels[1]==TRUE & length(labels)==1) labels<-seq(along=z)
    if (labels[1] != FALSE) {
        selected<-identify(z, ord.x, labels[good][ord])
        result <- seq(along=x)[good][ord][selected]
        }
    if (is.null(result)) invisible(result) else sort(result)

#     if(distribution == "normal") {
#         if(is.null(xlab)) xlab = "Normal Quantiles"
#         if(is.null(ylab)) ylab = "Empirical Quantiles"
#         if(is.null(main)) main = "Normal QQ-Plot"
# 
#         # Normal Quantile-Quantile Plot:
#         qqnorm(x, xlab = xlab, ylab = ylab, main = main, pch = symbolset, axes = FALSE, ...)
# #         qqline(x, col = colorset[2], lwd = 2)
#         q.theo = qnorm(c(0.25,0.75))
#     }
#     if(distribution == "sst") {
#         library("sn")
#         if(is.null(xlab)) xlab = "Skew-T Quantiles"
#         if(is.null(ylab)) ylab = "Empirical Quantiles"
#         if(is.null(main)) main = "Skew-T QQ-Plot"
# 
#         # Skew Student-T Quantile-Quantile Plot:
#         y = qst(c(1:n)/(n+1))
#         qqplot(y, x, xlab = xlab, ylab = ylab, axes=FALSE, main=main, ...)
#         q.theo = qst(c(0.25,0.75))
#     }
#     if(distribution == "cauchy") {
#         if(is.null(xlab)) xlab = "Cauchy Quantiles"
#         if(is.null(ylab)) ylab = "Empirical Quantiles"
#         if(is.null(main)) main = "Cauchy QQ-Plot"
# 
#         # Skew Student-T Quantile-Quantile Plot:
#         y = qcauchy(c(1:n)/(n+1))
#         qqplot(y, x, xlab = xlab, ylab = ylab, axes=FALSE, main=main, ...)
#         q.theo = qcauchy(c(0.25,0.75))
#     }
#     if(distribution == "lnorm") {
#         if(is.null(xlab)) xlab = "Log Normal Quantiles"
#         if(is.null(ylab)) ylab = "Empirical Quantiles"
#         if(is.null(main)) main = "Log Normal QQ-Plot"
# 
#         # Skew Student-T Quantile-Quantile Plot:
#         y = qlnorm(c(1:n)/(n+1))
#         qqplot(y, x, xlab = xlab, ylab = ylab, axes=FALSE, main=main, ...)
#         q.theo = qlnorm(c(0.25,0.75))
#     }
# 
#     q.data=quantile(x,c(0.25,0.75))
#     slope = diff(q.data)/diff(q.theo)
#     int = q.data[1] - slope* q.theo[1]
# 
#     if(line) abline(int, slope, col = colorset[2], lwd = 2)

    axis(1, cex.axis = 0.8, col = elementcolor)
    axis(2, cex.axis = 0.8, col = elementcolor)

    box(col=elementcolor)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.QQPlot.R,v 1.6 2007-12-06 21:38:56 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2007/11/21 05:32:13  peter
# - fixed title
#
# Revision 1.3  2007/09/26 02:59:23  peter
# - changed chart elements to be consistent with other chart
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################