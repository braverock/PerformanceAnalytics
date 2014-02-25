#' box whiskers plot wrapper
#' 
#' A wrapper to create box and whiskers plot with some defaults useful for
#' comparing distributions.
#' 
#' We have also provided controls for all the symbols and lines in the chart.
#' One default, set by \code{as.Tufte=TRUE}, will strip chartjunk and draw a
#' Boxplot per recommendations by Edward Tufte. Another default, set by \code{as.Notch=TRUE}, will draw a notch in each side of the boxes.  It can also be useful when comparing several series to sort them in the order of ascending or descending return or risk measurement by use of \code{sort.by} and  \code{sort.ascending=TRUE}. In addition, one can compare this with another user specified order, called base order, e.g., to see the relative change of the orders of the series between two measurements of interest.
#' 
#'   
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param horizontal TRUE/FALSE plot horizontal (TRUE) or vertical (FALSE)
#' @param names logical. if TRUE, show the names of each series
#' @param as.Tufte logical. default FALSE. if TRUE use method derived for Tufte
#' for limiting chartjunk
#' @param as.Notch logical. default FALSE. if TRUE a notch is drawn in each side of the boxes. 
#' See \code{\link[graphics]{boxplot}}
#' @param sort.by one of the return or risk measure c("NULL", "mean", "median", "variance", "sharp ratio", "mean absolute deviation", "std dev", "sterling ratio", "calmar ratio", "burke ratio", "pain index", "ulcer index","martin ratio", "downside risk", "omega ratio", "sortino ratio", "upside risk","upside potential ratio", "omega sharpe ratio"). default is "NULL".
#' @param sort.base one of the return or risk measure as listed in \code{sort.by},
#' add the base order number next to the labels sorted by \code{sort.by}
#' @param sort.ascending logical.  If TRUE sort the distributions by ascending
#' \code{sort.by} and \code{sort.base}
#' @param colorset color palette to use, set by default to rational choices
#' @param symbol.color draws the symbols described in
#' \code{mean.symbol},\code{median.symbol},\code{outlier.symbol} in the color
#' specified
#' @param mean.symbol symbol to use for the mean of the distribution
#' @param median.symbol symbol to use for the median of the distribution
#' @param outlier.symbol symbol to use for the outliers of the distribution
#' @param show.data numerical vector of column numbers to display on top of
#' boxplot, default NULL
#' @param add.mean logical. if TRUE, show a line for the mean of all
#' distributions plotted
#' @param xlab set the x-axis label, same as in \code{\link{plot}}
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param element.color specify the color of chart elements.  Default is
#' "darkgray"
#' @param \dots any other passthru parameters
#' @return box plot of returns
#' @author Peter Carl
#' @author Ke Li \email{kirkli@@stat.washington.edu}
#' @seealso \code{\link[graphics]{boxplot}}
#' @references Tufte, Edward R.  \emph{The Visual Display of Quantitative
#' Information}. Graphics Press. 1983. p. 124-129
#' @keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(edhec)
#' chart.Boxplot(edhec)
#' chart.Boxplot(edhec,as.Tufte=TRUE)
#' chart.Boxplot(R=edhec,sort.by="upside risk", 
#' 		horizontal=TRUE, sort.base="std dev", 
#' 		sort.ascending=TRUE)
#' @export 
chart.Boxplot <-
		function (R, horizontal = TRUE, names = TRUE, as.Tufte = FALSE, as.Notch=FALSE, sort.by = NULL, sort.base=NULL, sort.ascending = FALSE, colorset = "black", symbol.color = "red", mean.symbol = 1, median.symbol = "|", outlier.symbol = 1, show.data = NULL, add.mean = TRUE, xlab="Return", main = "Return Distribution Comparison", element.color = "darkgray",  ...)
{ # @ author Peter Carl, updated by Kirk Li. 
	# DESCRIPTION:
	# A wrapper to create box and whiskers plot, but with some sensible defaults
	# useful for comparing distributions.
	
	# mar: a numerical vector of the form c(bottom, left, top, right) which
	# gives the number of lines of margin to be specified on the four sides
	# of the plot. The default is c(5, 4, 4, 2) + 0.1
	
	sort.by <- tolower(sort.by)
	
	sort.by = sort.by[1]
	
	pool <- c("NA","mean", "median", "variance", "sharp ratio", "mean absolute deviation", "std dev", "sterling ratio", "calmar ratio", "burke ratio", "pain index", "ulcer index","martin ratio", "downside risk", "omega ratio", "sortino ratio", "upside risk","upside potential ratio", "omega sharpe ratio")

	sort.by <- match.arg(sort.by, pool, several.ok = FALSE)
	
	R = checkData(R, method="data.frame")
	R.xts = checkData(R,method="xts")
	columns = ncol(R)
	rows = nrow(R)
	columnnames = colnames(R)
	
	column.order = NULL
	
	sort.by <- tolower(sort.by)
	
	sort.by = sort.by[1]
	
	op <- par(no.readonly=TRUE)
	
	if(names){
		par(mar=c(5,12,4,2) + 0.2)
	}
	
	if(length(colorset) < columns)
		colorset = rep(colorset, length.out = columns)
	
	if(length(symbol.color) < columns)
		symbol.color = rep(symbol.color, length.out = columns)
	
	if(length(mean.symbol) < columns)
		mean.symbol = rep(mean.symbol, length.out = columns)
	
	means = sapply(R, mean, na.rm = TRUE)
	
	
	asc.desc <- ifelse(sort.ascending,"ascending","descending")
	
	switch(sort.by,
			mean = {
				column.order = order(means, decreasing=!sort.ascending)
				sort.by = paste("Mean", sep="")
			},
			median = {
				medians = sapply(R, median, na.rm = TRUE)
				column.order = order(medians, decreasing=!sort.ascending)
				sort.by = paste("Median", sep="")
			},
			variance = {
				variances = sapply(R, var, na.rm = TRUE)
				column.order = order(variances, decreasing=!sort.ascending)
				sort.by = paste("Variance", sep="")
			},
			"sharp ratio" = {
				sharpratio = sapply(R,function(x)mean(x,na.rm = TRUE)/sd(x,na.rm = TRUE))					
				column.order = order(sharpratio, decreasing=!sort.ascending)
				sort.by = paste("Sharp Ratio",sep="")
			},
			"mean absolute deviation" = {
				MeanAbsoluteDeviation = sapply(R,MeanAbsoluteDeviation)					
				column.order = order(MeanAbsoluteDeviation, decreasing=!sort.ascending)
				sort.by = paste("Mean Absolute Dev",sep="")
			},
			"std dev" = {
				StdDev.annualized = sapply(R.xts,function(x)StdDev.annualized(x))					
				column.order = order(StdDev.annualized, decreasing=!sort.ascending)
				sort.by = paste("Std Dev",sep="")
			},
			"sterling ratio" = {
				SterlingRatio = sapply(R.xts,SterlingRatio)
				column.order = order(SterlingRatio, decreasing=!sort.ascending)
				sort.by = paste("Sterling Ratio",sep="")
			},
			"calmar ratio" = {
				CalmarRatio = sapply(R.xts,CalmarRatio)					
				column.order = order(CalmarRatio, decreasing=!sort.ascending)
				sort.by = paste("Calmar Ratio",sep="")
			},
			"burke ratio" = {
				BurkeRatio = sapply(R.xts,BurkeRatio)					
				column.order = order(BurkeRatio, decreasing=!sort.ascending)
				sort.by = paste("Burke Ratio",sep="")
			},
			"pain index" = {
				PainIndex = sapply(R,PainIndex)					
				column.order = order(PainIndex, decreasing=!sort.ascending)
				sort.by = paste("Pain Index",sep="")
			},
			"ulcer index" = {
				UlcerIndex = sapply(R,UlcerIndex)					
				column.order = order(UlcerIndex, decreasing=!sort.ascending)
				sort.by = paste("Ulcer Index",sep="")
			},
			"martin ratio" = {
				MartinRatio = sapply(R.xts,MartinRatio)					
				column.order = order(MartinRatio, decreasing=!sort.ascending)
				sort.by = paste("Martin Ratio",sep="")
			},
			"downside risk" = {
				DownsideDeviation = sapply(R,DownsideDeviation)					
				column.order = order(DownsideDeviation, decreasing=!sort.ascending)
				sort.by = paste("Downside Risk",sep="")
			},
			"omega ratio" = {
				Omega = sapply(R,Omega)					
				column.order = order(Omega, decreasing=!sort.ascending)
				sort.by = paste("Omega Ratio",sep="")
			},
			"sortino ratio" = {
				SortinoRatio = sapply(R,SortinoRatio)					
				column.order = order(SortinoRatio, decreasing=!sort.ascending)
				sort.by = paste("Sortino Ratio",sep="")
			},
			"upside risk" = {
				UpsideRisk = sapply(R,UpsideRisk)					
				column.order = order(UpsideRisk, decreasing=!sort.ascending)
				sort.by = paste("Upside Risk",sep="")
			},
			"upside potential ratio" = {
				UpsidePotentialRatio = sapply(R,UpsidePotentialRatio)					
				column.order = order(UpsidePotentialRatio, decreasing=!sort.ascending)
				sort.by = paste("Upside Potential Ratio",sep="")
			},
			"omega sharpe ratio" = {
				OmegaSharpeRatio = sapply(R,OmegaSharpeRatio)					
				column.order = order(OmegaSharpeRatio, decreasing=!sort.ascending)
				sort.by = paste("Omega Sharpe Ratio",sep="")
			},
			{
				column.order = 1:columns
				sort.by = paste("Unsorted", sep="")
			}
	) # end switch
	
	ylab=paste("Sorted by:",asc.desc,sort.by)
	
	
	# base order
	if(!is.null(sort.base)){
		colum.order.base = NULL
		
		sort.base <- tolower(sort.base)
		
		sort.base <- match.arg(sort.base, pool, several.ok = FALSE)
		
		switch(sort.base,
				mean = {
					means = sapply(R, mean, na.rm = TRUE)
					column.order.base = order(means, decreasing=!sort.ascending)
					sort.base = paste("Mean", sep="")
				},
				median = {
					medians = sapply(R, median, na.rm = TRUE)
					column.order.base = order(medians, decreasing=!sort.ascending)
					sort.base = paste("Median", sep="")
				},
				variance = {
					variances = sapply(R, var, na.rm = TRUE)
					column.order.base = order(variances, decreasing=!sort.ascending)
					sort.base = paste("Variance", sep="")
				},
				"sharp ratio" = {
					sharpratio = sapply(R,function(x)mean(x,na.rm = TRUE)/sd(x,na.rm = TRUE))					
					column.order.base = order(sharpratio, decreasing=!sort.ascending)
					sort.base = paste("Sharp Ratio",sep="")
				},
				"mean absolute deviation" = {
					MeanAbsoluteDeviation = sapply(R,MeanAbsoluteDeviation)					
					column.order.base = order(MeanAbsoluteDeviation, decreasing=!sort.ascending)
					sort.base = paste("Mean Absolute Dev",sep="")
				},
				"std dev" = {
					StdDev.annualized = sapply(R.xts,StdDev.annualized)					
					column.order.base = order(StdDev.annualized, decreasing=!sort.ascending)
					sort.base = paste("Std Dev",sep="")
				},
				"sterling ratio" = {
					SterlingRatio = sapply(R.xts,SterlingRatio)					
					column.order.base = order(SterlingRatio, decreasing=!sort.ascending)
					sort.base = paste("Sterling Ratio",sep="")
				},
				"calmar ratio" = {
					CalmarRatio = sapply(R.xts,function(x)mean(x,na.rm = TRUE)/sd(x,na.rm = TRUE))					
					column.order.base = order(CalmarRatio, decreasing=!sort.ascending)
					sort.base = paste("Calmar Ratio",sep="")
				},
				"burke ratio" = {
					BurkeRatio = sapply(R.xts,BurkeRatio)					
					column.order.base = order(BurkeRatio, decreasing=!sort.ascending)
					sort.base = paste("Burke Ratio",sep="")
				},
				"ulcer index" = {
					UlcerIndex = sapply(R,UlcerIndex)					
					column.order.base = order(UlcerIndex, decreasing=!sort.ascending)
					sort.base = paste("Ulcer Index",sep="")
				},
				"pain index" = {
					PainRatio = sapply(R.xts,PainRatio)					
					column.order.base = order(PainRatio, decreasing=!sort.ascending)
					sort.base = paste("Pain Index",sep="")
				},
				"martin ratio" = {
					MartinRatio = sapply(R.xts,MartinRatio)					
					column.order.base = order(MartinRatio, decreasing=!sort.ascending)
					sort.base = paste("Martin Ratio",sep="")
				},
				"downside risk" = {
					DownsideDeviation = sapply(R,DownsideDeviation)					
					column.order.base = order(DownsideDeviation, decreasing=!sort.ascending)
					sort.base = paste("Downside Risk",sep="")
				},
				"omega ratio" = {
					Omega = sapply(R,Omega)					
					column.order.base = order(Omega, decreasing=!sort.ascending)
					sort.base = paste("Omega Ratio",sep="")
				},
				"sortino ratio" = {
					SortinoRatio = sapply(R,SortinoRatio)					
					column.order.base = order(SortinoRatio, decreasing=!sort.ascending)
					sort.base = paste("Sortino Ratio",sep="")
				},
				"upside risk" = {
					UpsideRisk = sapply(R,UpsideRisk)					
					column.order.base = order(UpsideRisk, decreasing=!sort.ascending)
					sort.base = paste("Upside Risk",sep="")
				},
				"upside potential ratio" = {
					UpsidePotentialRatio = sapply(R,UpsidePotentialRatio)					
					column.order.base = order(UpsidePotentialRatio, decreasing=!sort.ascending)
					sort.base = paste("Upside Potential Ratio",sep="")
				},
				"omega sharpe ratio" = {
					OmegaSharpeRatio = sapply(R,OmegaSharpeRatio)					
					column.order.base = order(OmegaSharpeRatio, decreasing=!sort.ascending)
					sort.base = paste("Omega Sharpe Ratio",sep="")
				},
				{
					column.order.base = 1:columns
					sort.base = paste("Unsorted", sep="")
				}
		) # end switch
		
		ylab.base=paste(asc.desc,sort.base)
	}
	
	if(horizontal) {
		par(mar=c(5,8,4,2)+1) 	
		column.order.box <- rev(column.order)
		if(!is.null(sort.base)) 
			column.order.base.box <- rev(column.order.base)
	} 	else {
		par(mar=c(8,4,4,2)+1)
		column.order.box <- column.order
		if(!is.null(sort.base)) 
			column.order.base.box <- column.order.base
		
	}
	
	
	if(as.Tufte){
		boxplot(R[,column.order.box], horizontal = horizontal, names = names, main = main, xlab = ifelse(horizontal,xlab,""), ylab = ifelse(horizontal,"",xlab), pars = list(boxcol = "white", medlty = "blank", medpch = median.symbol, medlwd = 2, medcex = .8, medcol = colorset[column.order.box], whisklty = c(1,1), whiskcol = colorset[column.order.box], staplelty = "blank", outpch = outlier.symbol, outcex = .5, outcol = colorset[column.order.box] ), axes = FALSE, cex.lab=0.7,...)
		mtext(side=3,text=ylab,cex=0.7)
		if(!is.null(sort.base)) 
			mtext(side=3,
					text=paste("Base order: ",ylab.base,sep=" "),line=1,cex=0.7)
	} 	else if(as.Notch){
		
		boxplot(R[,column.order.box], horizontal = horizontal, names = names, main = main, xlab = ifelse(horizontal,xlab,""), ylab = ifelse(horizontal,"",xlab), pars = list(boxcol = colorset[column.order.box], medlwd = 1, medcol = colorset[column.order.box], whisklty = c(1,1), whiskcol = colorset[column.order.box], staplelty = 1, staplecol = colorset[column.order.box], staplecex = .5, outpch = outlier.symbol, outcex = .5, outcol = colorset[column.order.box] ), axes = FALSE, boxwex=.6, cex.lab=0.7, notch=TRUE,...)
		mtext(side=3,text=ylab,cex=0.7)
		
		if(!is.null(sort.base)) 
			mtext(side=3,
					text=paste("Base order: ",ylab.base,sep=" "),line=1,cex=0.7)
	} 	else{
		
		boxplot(R[,column.order.box], horizontal = horizontal, names = names, main = main, xlab = ifelse(horizontal,xlab,""), ylab = ifelse(horizontal,"",xlab), pars = list(boxcol = colorset[column.order.box], medlwd = 1, medcol = colorset[column.order.box], whisklty = c(1,1), whiskcol = colorset[column.order.box], staplelty = 1, staplecol = colorset[column.order.box], staplecex = .5, outpch = outlier.symbol, outcex = .5, outcol = colorset[column.order.box] ), axes = FALSE, boxwex=.6, cex.lab=0.7,...)
		mtext(side=3,text=ylab,cex=0.7)
		if(!is.null(sort.base)) 
			mtext(side=3,
					text=paste("Base order: ", ylab.base,sep=" "),line=1,cex=0.7)
	} # end else
	
	if(!is.null(show.data)) {
		highlight.color=1:24
		for (item in show.data) {
			points(as.vector(R[item,column.order]), 1:columns, col=highlight.color[item]) #, pch = mean.symbol[column.order], col=symbol.color[column.order])
		}
	}
	
	if(add.mean){
		if(horizontal)
			points(means[column.order], columns:1, pch = mean.symbol[column.order], col=symbol.color[column.order],cex=0.5)  	else 
			points(1:columns, means[column.order],  pch = mean.symbol[column.order], col=symbol.color[column.order],cex=0.5)
	}
	
	if(names){
		if(!is.null(sort.base)){
			if(horizontal){
				labels = paste(columnnames[column.order],"    ",sep="")
				labels.sec =paste("(",(match(column.order,column.order.base)),")",sep="")
				labels=rev(labels)
			} else{
				labels = paste(columnnames[column.order],"    ",sep="")
				labels.sec = paste("(",match(column.order,column.order.base),")",sep="")
			}
		} else 	labels = columnnames[column.order]
		
		if(!horizontal){
#			axis(1,labels=FALSE)
			text(1:length(labels), par("usr")[1] - 0.2, srt = 45, adj = 1,
					labels = labels, xpd = TRUE, cex=0.7)
			if(!is.null(sort.base)) 
				text(1:length(labels), par("usr")[1] - 0.2, srt = 0, adj = 1,
						labels = labels.sec, xpd = TRUE, cex=0.5)
			## Plot x axis label at line 6 (of 7)
		}else{
#			axis(2, cex.axis = 0.9, col = element.color, labels = labels, at = 1:columns, las = 2)
			text(par("usr")[3] - 0.24, 1:length(labels),  srt = 0, adj = 1,
					labels = labels, xpd = TRUE, cex=0.7)	
			if(!is.null(sort.base)) 
				text(par("usr")[3] - 0.24, 1:length(labels), srt = 0, adj = 0,
						labels = labels.sec, xpd = TRUE, cex=0.5)
		}
	} 	else{
		labels = ""
		axis(2, cex.axis = 0.8, col = element.color, labels = labels, at = 1:columns, las = 1, tick = FALSE)
	}
	
#     if(names)
#         title(sub=ylab)
#     else
#         title(sub=ylab)
	box(col=element.color)
	
	if(horizontal) {
		abline(v=0, lty="solid",col=element.color)
	} 	else {
		abline(h=0, lty="solid",col=element.color)
	}
	
	
	par(op)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Boxplot.R 2621 2013-07-22 19:36:44Z peter_carl $
#
###############################################################################
