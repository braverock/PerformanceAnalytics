#' Plot a QQ chart
#' 
#' Plot the return data against any theoretical distribution.
#' 
#' A Quantile-Quantile (QQ) plot is a scatter plot designed to compare the data
#' to the theoretical distributions to visually determine if the observations
#' are likely to have come from a known population. The empirical quantiles are
#' plotted to the y-axis, and the x-axis contains the values of the theorical
#' model.  A 45-degree reference line is also plotted. If the empirical data
#' come from the population with the choosen distribution, the points should
#' fall approximately along this reference line. The larger the departure from
#' the reference line, the greater the evidence that the data set have come
#' from a population with a different distribution.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param distribution root name of comparison distribution - e.g., 'norm' for
#' the normal distribution; 't' for the t-distribution. See examples for other
#' ideas.
#' @param distributionParameter a string of the parameters of the distribution
#'  e.g., distributionParameter = 'location = 1, scale = 2, shape = 3, df = 4' 
#' for skew-T distribution 
#' @param xlab set the x-axis label, as in \code{\link{plot}}
#' @param ylab set the y-axis label, as in \code{\link{plot}}
#' @param xaxis if true, draws the x axis
#' @param yaxis if true, draws the y axis
#' @param ylim set the y-axis limits, same as in \code{\link{plot}}
#' @param main set the chart title, same as in \code{plot}
#' @param las set the direction of axis labels, same as in \code{plot}
#' @param envelope confidence level for point-wise confidence envelope, or
#' FALSE for no envelope.
#' @param labels vector of point labels for interactive point identification,
#' or FALSE for no labels.
#' @param col color for points and lines; the default is the \emph{second}
#' entry in the current color palette (see 'palette' and 'par').
#' @param lwd set the line width, as in \code{\link{plot}}
#' @param pch symbols to use, see also \code{\link{plot}}
#' @param cex symbols to use, see also \code{\link{plot}}
#' @param line 'quartiles' to pass a line through the quartile-pairs, or
#' 'robust' for a robust-regression line; the latter uses the 'rlm' function
#' in the 'MASS' package. Specifying 'line = "none"' suppresses the line.
#' @param element.color provides the color for drawing chart elements, such as
#' the box lines, axis lines, etc. Default is "darkgray"
#' @param cex.legend The magnification to be used for sizing the legend
#' relative to the current setting of 'cex'
#' @param cex.axis The magnification to be used for axis annotation relative to
#' the current setting of 'cex'
#' @param cex.lab The magnification to be used for x- and y-axis labels
#' relative to the current setting of 'cex'
#' @param cex.main The magnification to be used for the main title relative to
#' the current setting of 'cex'.
#' @param \dots any other passthru parameters to the distribution function
#' 
#' @author John Fox, ported by Peter Carl
#' @seealso 
#' \code{\link[stats]{qqplot}} \cr 
#' \code{\link[car]{qq.plot}} \cr
#' \code{\link{plot}}
#' @references main code forked/borrowed/ported from the excellent: \cr Fox,
#' John (2007) \emph{car: Companion to Applied Regression} \cr
#' \url{https://socserv.socsci.mcmaster.ca/jfox/}
###keywords ts multivariate distribution models hplot
#' 
#' @examples
#' # you'll need lots of extra packages to run these examples of different distributions
#' \dontrun{ # these examples require multiple packages from 'Suggests', so don't test on CRAN
#' library(MASS) 
#' library(PerformanceAnalytics)
#' data(managers)
#' x = checkData(managers[,2, drop = FALSE], na.rm = TRUE, method = "vector")
#' 
#' # Panel 1: Normal distribution
#' chart.QQPlot(x, main = "Normal Distribution",
#' 		line=c("quartiles"), distribution = 'norm',  
#' 		envelope=0.95)
#' 
#' 
#' # Panel 2, Log-Normal distribution
#' fit = fitdistr(1+x, 'lognormal')
#' chart.QQPlot(1+x, main = "Log-Normal Distribution", envelope=0.95, 
#'     distribution='lnorm',distributionParameter='meanlog = fit$estimate[[1]], 
#'     sdlog = fit$estimate[[2]]')
#'  
#' # Panel 3: Mixture Normal distribution
#' # library(nor1mix)
#' obj = norMixEM(x,m=2)
#' chart.QQPlot(x, main = "Normal Mixture Distribution",
#' 		line=c("quartiles"), distribution = 'norMix',  distributionParameter='obj',
#' 		envelope=0.95)
#' 
#' 
#' # Panel 4: Symmetric t distribution
#' library(sn)
#' n = length(x)
#' fit.tSN = st.mple(as.matrix(rep(1,n)),x,symmetr = TRUE)
#' names(fit.tSN$dp) = c("location","scale","dof")
#' round(fit.tSN$dp,3)
#' 
#' chart.QQPlot(x, main = "MO Symmetric t-Distribution QQPlot",
#' 		xlab = "quantilesSymmetricTdistEst",line = c("quartiles"),
#' 		envelope = .95, distribution = 't', 
#' 		distributionParameter='df=fit.tSN$dp[3]',pch = 20)
#' 
#' # Panel 5: Skewed t distribution
#' fit.st = st.mple(as.matrix(rep(1,n)),x)
#' # fit.st = st.mple(y=x)  Produces same result as line above
#' names(fit.st$dp) = c("location","scale","skew","dof")
#' round(fit.st$dp,3)
#' 
#' chart.QQPlot(x, main = "MO Returns Skewed t-Distribution QQPlot",
#' 		xlab = "quantilesSkewedTdistEst",line = c("quartiles"),
#' 		envelope = .95, distribution = 'st',
#' 		distributionParameter = 'xi = fit.st$dp[1],
#' 				omega = fit.st$dp[2],alpha = fit.st$dp[3],
#' 				nu=fit.st$dp[4]',
#' 		pch = 20)
#' 
#' # Panel 6: Stable Parietian
#' library(fBasics)
#' fit.stable = stableFit(x,doplot=FALSE)
#' chart.QQPlot(x, main = "Stable Paretian Distribution", envelope=0.95, 
#'              distribution = 'stable', 
#'              distributionParameter = 'alpha = fit(stable.fit)$estimate[[1]], 
#'                  beta = fit(stable.fit)$estimate[[2]], 
#'                  gamma = fit(stable.fit)$estimate[[3]], 
#'                  delta = fit(stable.fit)$estimate[[4]], pm = 0')
#' }

#' #end examples
#' 
#' @export 
chart.QQPlot <- 
		function (R, distribution = "norm", ylab = NULL, xlab = paste(distribution, 
						"Quantiles"), main = NULL, las = par("las"), envelope = FALSE, 
				labels = FALSE, col = c(1, 4), lwd = 2, pch = 1, cex = 1, 
				line = c("quartiles", "robust", "none"), element.color = "darkgray", 
				cex.axis = 0.8, cex.legend = 0.8, cex.lab = 1, cex.main = 1, 
				xaxis = TRUE, yaxis = TRUE, ylim = NULL, distributionParameter=NULL,...) 
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
	
	if (is.null(main)) {
		if (!is.null(colnames(R)[1])) 
			main = colnames(R)[1]
		else main = "QQ Plot"
	}
	if (is.null(ylab)) 
		ylab = "Empirical Quantiles"
	# the core of this function is taken from John Fox's qq.plot, which is part of the car package
	
	result <- NULL
	line <- match.arg(line)
	good <- !is.na(x)
	ord <- order(x[good])
	ord.x <- x[good][ord]
	
	q.function <- eval(parse(text = paste("q", distribution, 
							sep = "")))
	d.function <- eval(parse(text = paste("d", distribution, 
							sep = "")))
	n <- length(ord.x)
	P <- ppoints(n)
#	z <- q.function(P, ...)
	z <- NULL

	eval(parse(text=paste("	z <- q.function(P,", distributionParameter,",...)")))
	
	plot(z, ord.x, xlab = xlab, ylab = ylab, main = main, las = las, 
			col = col[1], pch = pch, cex = cex, cex.main = cex.main, 
			cex.lab = cex.lab, axes = FALSE, ylim = ylim, ...)
	if (line == "quartiles") {
		Q.x <- quantile(ord.x, c(0.25, 0.75))
		
		Q.z<-NULL
		eval(parse(text=paste("	Q.z <- q.function(c(0.25, 0.75),", distributionParameter,",...)")))
#		Q.z <- q.function(c(0.25, 0.75), ...)
		b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
		a <- Q.x[1] - b * Q.z[1]
		abline(a, b, col = col[2], lwd = lwd)
	}
	if (line == "robust") {
		stopifnot(requireNamespace("MASS", quietly = TRUE))
		coef <- coefficients(MASS::rlm(ord.x ~ z))
		a <- coef[1]
		b <- coef[2]
		abline(a, b, col = col[2])
	}
	if (line != "none" & envelope != FALSE) {
		zz <- qnorm(1 - (1 - envelope)/2)
		
		SE<-NULL
		eval(parse(text=paste("	SE <- (b/d.function(z,", distributionParameter,",...))* sqrt(P * (1 - P)/n)")))
		
#		SE <- (b/d.function(z, ...)) * sqrt(P * (1 - P)/n)
		fit.value <- a + b * z
		upper <- fit.value + zz * SE
		lower <- fit.value - zz * SE
		lines(z, upper, lty = 2, lwd = lwd/2, col = col[2])
		lines(z, lower, lty = 2, lwd = lwd/2, col = col[2])
	}
	if (labels[1] == TRUE & length(labels) == 1) 
		labels <- seq(along = z)
	if (labels[1] != FALSE) {
		selected <- identify(z, ord.x, labels[good][ord])
		result <- seq(along = x)[good][ord][selected]
	}
	if (is.null(result)) 
		invisible(result)
	else sort(result)
	if (xaxis) 
		axis(1, cex.axis = cex.axis, col = element.color)
	if (yaxis) 
		axis(2, cex.axis = cex.axis, col = element.color)
	box(col = element.color)
}

###############################################################################
# R (https://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2020 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
