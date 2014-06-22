# TODO: Add comment
# 
# Author: KirkLi
###############################################################################

###################################Appendix###################################

#data(edhec)
#R=edhec
#
## extract every functions from PA help doc, and save into a txt
#fun.list <- read.table("C:/Dropbox/doug/pa_functions.txt")
##library(PAKK)
## find fake functions that exist in doc but not in real
#save1 <- sapply(fun.list[,1],function(x){
#			s1 <- try(eval(parse(text=paste0("formals(",x,")"))))
#			if(class(s1)=="try-error"){}
#			else(return(s1))})
#l1 <- lapply(save1,"[",1)
## find functions that require input args
#save2 <- lapply(l1,function(x){
#			if(!is.null(x)){
#				names(x)
#			} else {"N"}
#		})
## find  functions whose first arg is either R or x
#save3 <- fun.list[save2 =="R"|save2 =="x",1]
## delete functions that too generic
#save3 <- save3[!c(grepl("table.",save3))]
#save3 <- save3[!c(grepl("charts.",save3))]
#save3 <- save3[!c(grepl("chart.",save3))]
#metrics.can.use <- as.character(save3)
#y = checkData(R, method = "zoo")
#
## test rest of the funs
#SS <- list()
#for(i in 1:length(metrics.can.use)){
#	ss <- try(apply(as.matrix(y[,1]),MARGIN = 2,FUN=metrics.can.use[i]))
#	SS[[i]] <- ss
#}
## record functions that are metrics
#metrics.can.use.2 <- as.character(save3[unlist(lapply(SS,class))=="numeric"])
## output the list of functions into a string
#edit(metrics.can.use.2)

######################################################################
table.Performance.pool <- function(...){
	c(		"AdjustedSharpeRatio", 
			"AverageDrawdown", 
			"BernardoLedoitRatio", 
			"BurkeRatio", 
			"CalmarRatio", 
			"CVaR", 
			"DownsideDeviation", 
			"DownsideFrequency", 
			"DownsidePotential", 
			"DRatio", 
			"DrawdownDeviation", 
			"ES", 
			"ETL", 
			"Frequency", 
#			"KellyRatio", problem occurs due to only one method in KellyRatio, will fix
			"kurtosis", 
			"MartinRatio", 
			"maxDrawdown", 
			"mean.geometric", 
			"mean.LCL", 
			"mean.stderr", 
			"mean.UCL", 
			"MeanAbsoluteDeviation", 
			"Omega", 
			"OmegaSharpeRatio", 
			"PainIndex", 
			"PainRatio", 
			"Return.annualized", 
			"Return.cumulative", 
			"sd.annualized", 
			"sd.multiperiod", 
			"SemiDeviation", 
			"SemiVariance", 
			"SharpeRatio.annualized", 
			"skewness", 
			"SkewnessKurtosisRatio", 
			"SmoothingIndex", 
			"SortinoRatio", 
			"StdDev", 
			"StdDev.annualized", 
			"SterlingRatio", 
			"UPR", 
			"UpsideFrequency", 
			"UpsidePotentialRatio", 
			"UpsideRisk", 
			"VaR", 
			"VolatilitySkewness")
	
}

#
#table.Performance <- function(R,metrics=NULL,metricsNames=NULL, verbose=FALSE,...){
#	# FUNCTION: 47-1 different metrics
#	pool <- c(
#			"AdjustedSharpeRatio", 
#			"AverageDrawdown", 
#			"BernardoLedoitRatio", 
#			"BurkeRatio", 
#			"CalmarRatio", 
#			"CVaR", 
#			"DownsideDeviation", 
#			"DownsideFrequency", 
#			"DownsidePotential", 
#			"DRatio", 
#			"DrawdownDeviation", 
#			"ES", 
#			"ETL", 
#			"Frequency", 
##			"KellyRatio", problem occurs due to only one method in KellyRatio, will fix
#			"kurtosis", 
#			"MartinRatio", 
#			"maxDrawdown", 
#			"mean.geometric", 
#			"mean.LCL", 
#			"mean.stderr", 
#			"mean.UCL", 
#			"MeanAbsoluteDeviation", 
#			"Omega", 
#			"OmegaSharpeRatio", 
#			"PainIndex", 
#			"PainRatio", 
#			"Return.annualized", 
#			"Return.cumulative", 
#			"sd.annualized", 
#			"sd.multiperiod", 
#			"SemiDeviation", 
#			"SemiVariance", 
#			"SharpeRatio.annualized", 
#			"skewness", 
#			"SkewnessKurtosisRatio", 
#			"SmoothingIndex", 
#			"SortinoRatio", 
#			"StdDev", 
#			"StdDev.annualized", 
#			"SterlingRatio", 
#			"UPR", 
#			"UpsideFrequency", 
#			"UpsidePotentialRatio", 
#			"UpsideRisk", 
#			"VaR", 
#			"VolatilitySkewness")
#	
##	extract metric functions' arguments
#	ArgFormals <- lapply(pool,function(x)formals(x))
#	ArgNames <- lapply(ArgFormals,function(x)names(x))
#	ArgString.temp <- unique(unlist(ArgNames))
#	ArgString <- sort(ArgString.temp[-which(ArgString.temp%in%c("R","x","..."))])	
##	ArgNames.use <- lapply(ArgNames,function(x)ArgString.temp[ArgString.temp%in%x])
##	ArgValue <- lapply(ArgNames,function(x)ArgString[ArgString%in%x])
#	
#	metrics.vec <- data.frame(
#			metrics=pool,
#			include=rep(0,length(pool)),
#			metricsNames=pool,
#			stringsAsFactors=FALSE)
#	
#	
##	loop through each metric and input the default values of args
#	for (i in paste0("arg_",ArgString))
#		eval(parse(text=paste0("metrics.vec$",i,"<- '#'")))
#	
#	
#	for (i in 1:length(pool)){
##		i=1
#		ArgFormals.i <- ArgFormals[[i]]
#		ArgNames.use.i <- names(ArgFormals.i)
#		
#		for (ii in ArgString){
##		ii=ArgString[1]
#			if(any(ArgNames.use.i%in%ii)){
#				temp <- ArgFormals.i[which(ArgNames.use.i==ii)]
#				temp <- ifelse(class(temp[[1]])%in%c("call","NULL"),as.character(temp),temp) 
#				metrics.vec[i,paste0("arg_",ii)] <- temp
#			}  
#		}
#	}
##	promote the order of pre-specified metric
#	if(length(metrics)>0){
#		metrics.vec$include[match(metrics,metrics.vec$metrics)] <- 1
#		metrics.vec$metricsNames[match(metrics,metrics.vec$metrics)] <- metricsNames
#		metrics.vec <- metrics.vec[order(metrics.vec$include,decreasing=T),]
#	}
##	open data editor	
#	metrics.vec <- fix(metrics.vec) #allow user to change the input
#	
##   process the selected metrics and args	
#	metrics.choose <- subset(metrics.vec,include==1)
#	if(nrow(metrics.choose)==0) stop("please specify as least one metric")
#	colnames(metrics.choose) <- gsub("arg_","",colnames(metrics.choose))
#	metrics <- as.character(metrics.choose$metrics)
#	metricsNames <-  as.character(metrics.choose$metricsNames)
#
#	metricsOptArgVal <- 
#			lapply(1:nrow(metrics.choose[,-c(1:3),drop=FALSE]),function(x){
#						xx <- metrics.choose[x,-c(1:3),drop=FALSE]
#						xx[is.na(xx)] <- "NA"
#						xy <- as.vector(xx[xx!='#'])
#						names(xy) <-  names(xx)[xx!='#']
#						xy})
#	
#	
#	names(metricsOptArgVal) <- metrics
#	
#	
##   functions to call each metric function with user input args	
#	table.Arbitrary.m <- function(...){
#		y = checkData(R, method = "zoo")
#		columns = ncol(y)
#		rows = nrow(y)
#		columnnames = colnames(y)
#		rownames = rownames(y)
#		Arg.mat <- list()
#		for (column in 1:columns) {
##			 column=1
#			x = as.matrix(y[, column])
#			values = vector("numeric", 0)
#			for (metric in metrics) {
##			 metric=metrics[1]
#				ArgString.i <- paste(names(metricsOptArgVal[[metric]]),metricsOptArgVal[[metric]],sep=" = ")
#				Arg.mat[[metric]] <- ArgString.i
#				ArgString.i <- paste(ArgString.i,collapse =", ")
#				if(length(ArgString.i)>0){
#				if(nchar(ArgString.i>0))
#				newvalue = eval(parse(text=paste0("apply(x, MARGIN = 2, FUN = metric,",ArgString.i,")")))} else
#			newvalue = apply(x, MARGIN = 2, FUN = metric) #...
#				values = c(values, newvalue)
#			}
#			if (column == 1) {
#				resultingtable = data.frame(Value = values, row.names = metricsNames)
#			}
#			else {
#				nextcolumn = data.frame(Value = values, row.names = metricsNames)
#				resultingtable = cbind(resultingtable, nextcolumn)
#			}
#		}
#		names(Arg.mat) <- metrics
#		colnames(resultingtable) = columnnames
#		return(list(resultingtable=resultingtable,
#						Arg.mat=Arg.mat))
#	}
#	
##	table.Arbitrary.m()
#
##	generating the table	
#	res <- table.Arbitrary.m(...)
##	show printout	
#	if(verbose){
#		cat("###################################","\n")
#		cat("metrics:\n")
#		print(metrics)
#		cat("###################################","\n")
#		cat("metricsNames:\n")
#		print(metricsNames)
#		cat("###################################","\n")
#		cat("metricsOptArg:\n")
#		cat("Attention: for more than one element in args, \n only the first one will be used","\n")
#		print(res$Arg.mat)
#		cat("###################################","\n")
#		cat("table:\n")
#		print(res$resultingtable)
#	}
#	
#}


#table.Performance.input.shiny(metrics=c("ES"))
table.Performance.input.shiny <- function(metrics=NULL,metricsNames=NULL, verbose=FALSE,...){
	# FUNCTION: 47-1 different metrics
		
#	extract metric functions' arguments
	ArgFormals <- lapply(metrics,function(x)formals(x))
	ArgNames <- lapply(ArgFormals,function(x)names(x))
	ArgString.temp <- unique(unlist(ArgNames))
	ArgString <- sort(ArgString.temp[-which(ArgString.temp%in%c("R","x","..."))])	
#	ArgNames.use <- lapply(ArgNames,function(x)ArgString.temp[ArgString.temp%in%x])
#	ArgValue <- lapply(ArgNames,function(x)ArgString[ArgString%in%x])
	
	metrics.vec <- data.frame(
			metrics=metrics,
			include=rep(0,length(metrics)),
			metricsNames=metrics,
			stringsAsFactors=FALSE)
	
	
#	loop through each metric and input the default values of args
	for (i in paste0("arg_",ArgString))
		eval(parse(text=paste0("metrics.vec$",i,"<- '#'")))
	
	
	for (i in 1:length(metrics)){
#		i=1
		ArgFormals.i <- ArgFormals[[i]]
		ArgNames.use.i <- names(ArgFormals.i)
		
		for (ii in ArgString){
#		ii=ArgString[1]
			if(any(ArgNames.use.i%in%ii)){
				temp <- ArgFormals.i[which(ArgNames.use.i==ii)]
				temp <- ifelse(class(temp[[1]])%in%c("call","NULL"),as.character(temp),temp) 
				metrics.vec[i,paste0("arg_",ii)] <- temp
			}  
		}
	}
#	promote the order of pre-specified metric
	if(length(metrics)>0){
		metrics.vec$include[match(metrics,metrics.vec$metrics)] <- 1
		if(is.null(metricsNames)) metricsNames=metrics 
		metrics.vec$metricsNames[match(metrics,metrics.vec$metrics)] <- metricsNames
		metrics.vec <- metrics.vec[order(metrics.vec$include,decreasing=T),]
	}
#	open data editor	
#	metrics.vec <- fix(metrics.vec) #allow user to change the input
	
#   process the selected metrics and args	
	metrics.choose <- subset(metrics.vec,include==1)
	if(nrow(metrics.choose)==0) stop("please specify as least one metric")
	colnames(metrics.choose) <- gsub("arg_","",colnames(metrics.choose))
	metrics <- as.character(metrics.choose$metrics)
	metricsNames <-  as.character(metrics.choose$metricsNames)
#	metricsOptArg <- as.list(apply(metrics.choose[,-c(1:3)],1,function(x){
	##						x <- metrics.choose[1,-c(1:3)]
#						x[is.na(x)] <- "NA"
#						names(x)[x!='#']
#					}
#	))
#	metrics.choose[,-c(1:3),drop=FALSE]
	metricsOptArgVal <- 
			lapply(1:nrow(metrics.choose[,-c(1:3),drop=FALSE]),function(x){
#						x=2
						xx <- metrics.choose[x,-c(1:3),drop=FALSE]
						xx[is.na(xx)] <- "NA"
						xy <- as.vector(xx[xx!='#'])
						names(xy) <-  names(xx)[xx!='#']
						xy})
	names(metricsOptArgVal) <- metrics
	return(metricsOptArgVal)
}
	
#	names(metricsOptArg) <- metrics
	
#   functions to call each metric function with user input args	




table.Performance.output.shiny <- function(R,metricsOptArgVal, metrics=NULL,metricsNames=NULL, verbose=FALSE,...){
	
#	metrics=names(metricsOptArgVal)
	if(is.null(metricsNames))
		metricsNames=metrics
	table.Arbitrary.m <- function(...){
		y = checkData(R, method = "zoo")
		columns = ncol(y)
		rows = nrow(y)
		columnnames = colnames(y)
		rownames = rownames(y)
		Arg.mat <- list()
		for (column in 1:columns) {
#			 column=1
			x = as.matrix(y[, column])
			values = vector("numeric", 0)
			for (metric in metrics) {
#			 metric=metrics[1]
				ArgString.i <- paste(names(metricsOptArgVal[[metric]]),metricsOptArgVal[[metric]],sep=" = ")
#				ArgString.i[1] <- "p=0.9"
				Arg.mat[[metric]] <- ArgString.i
#			newvalue = apply(x, MARGIN = 2, FUN = metric)
				ArgString.i <- paste(ArgString.i,collapse =", ")
				if(length(ArgString.i)>0 & nchar(ArgString.i)>0)
					newvalue = eval(parse(text=paste0("apply(x, MARGIN = 2, FUN = metric,",ArgString.i,")"))) else
					newvalue = apply(x, MARGIN = 2, FUN = metric) #...
				values = c(values, newvalue)
			}
			if (column == 1) {
				resultingtable = data.frame(Value = values, row.names = metricsNames)
			}
			else {
				nextcolumn = data.frame(Value = values, row.names = metricsNames)
				resultingtable = cbind(resultingtable, nextcolumn)
			}
		}
		names(Arg.mat) <- metrics
		colnames(resultingtable) = columnnames
		rownames(resultingtable) = metricsNames
		return(list(resultingtable=resultingtable,
						Arg.mat=Arg.mat))
	}
	
#	table.Arbitrary.m()
	
#	generating the table	
	res <- table.Arbitrary.m(...)
#	show printout	
	if(verbose){
		cat("###################################","\n")
		cat("metrics:\n")
		print(metrics)
		cat("###################################","\n")
		cat("metricsNames:\n")
		print(metricsNames)
		cat("###################################","\n")
		cat("metricsOptArg:\n")
		cat("Attention: for more than one element in args, \n only the first one will be used","\n")
		print(res$Arg.mat)
		cat("###################################","\n")
		cat("table:\n")
		print(res$resultingtable)
	}
	res$resultingtable
	
}
###################################Example###################################
library(PerformanceAnalytics)
data(edhec)

# Example 1: start with NULL specification
#res <- table.Performance(edhec,verbose=T)
# Example 2: start with Var and ES
#res <- table.Performance(edhec,metrics=c("VaR", "ES"),metricsNames=c("Modified VaR","Modified Expected Shortfall"),verbose=T)

#
#
#a1 <- table.Performance.input.shiny("ES")
#
#table.Performance.output.shiny(mydata,a1,"ES")

