#' Generate general performance table for returns 
#' 
#' Main function to produce summary table. user can 
#' choose a set of metrics and corresponding optional arguments, 
#' modify metricsnames in output table, all together in data editor 
#' window from R console. For example, to include SharpeRatio, 
#' inside the data editor window, locate the row with SharpeRatio, 
#' and change "include" column from 0 to 1 to include the metric, and then 
#' change its optional arguments on the right that not with "#" sign. 
#' "#" sign simply means the argument (column index) is not defined 
#' for this metric (row index). 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param metrics a character vector of input metrics, use table.Performance.pool() to see 
#' all the condicate metrics
#' @param metricsNames options argument to specify metricsNames, default is NULL, 
#' the same as the metrics
#' @param interactive logical, default is TRUE, optional argument to trigger data editor window
#' @param arg.list optional argument to specify input optional argument for each metric, uses 
#' only interactive=FALSE
#' @param digits optional argument to specify the significant digits in printed table, default is 4
#' @param latex logical, default is FALSE, optional arguemnt to output latex code
#' @param exportFun logical, default is FALSE, optional argument to export function, see details
#' @details use \code{table.Performance.pool} to check available metrics. recoded SharpeRatio.
#' Both interactive and fixed input on metric set and optional arguments. Output latex code for resulting table. Export function that uses the same metrics and optional argument from interactive input. 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{table.Performance.pool}},\code{\link{table.Performance.pool.cran}},\code{\link{table.Arbitrary}}
#' @keywords table metrics performance measure
#' @examples
#' 
#' library(PerformanceAnalytics,lib="C:/R/R-3.1.0/library_forge")
#' data(edhec)
#' 
#' # Example 1: start with NULL specification
#' res <- table.Performance(R=edhec,verbose=T, interactive=TRUE)
#'
#' # Example 2: start with Var and ES
#' res.ex2 <- table.Performance(edhec,metrics=c("VaR", "ES"), 
#' metricsNames=c("Modified VaR","Modified Expected Shortfall"),verbose=T)
#' 
#' # Example 3: Non-interactive
#' arg.list <- list(
#' 		ES=list(method=c("modified","test"),
#' 				p=0.9),
#' 		VaR=list(method=c("gaussian"))
#' )
#' res.ex3 <- table.Performance(R=edhec,metrics=c("VaR", "ES"), interactive=FALSE,
#'  arg.list=arg.list, verbose=T, digits=4)
#' 
#' # Example 4: Latex code 
#' arg.list <- list(
#' 		ES=list(method=c("modified"),
#' 				p=0.9),
#' 		VaR=list(method=c("gaussian"))
#' )
# 
#' res.ex4 <- table.Performance(R=edhec,metrics=c("VaR", "ES"), interactive=FALSE, 
#' arg.list=arg.list, verbose=T, digits=4, latex=TRUE)
#'
#' # Example 5: Export function 
#' res.ex5 <- table.Performance(R=edhec,metrics=c("VaR", "ES"), interactive=TRUE, verbose=T,
#' digits=4, latex=FALSE, exportFun="myfun1", flag.pre.arg.list=FALSE)
#' myfun1(R=edhec)  
#' # myfun1 uses res.ex5's metrics and optional arguments 
#' args(myfun1)
#' @export
table.Performance <-
		function(R,metrics=NULL,metricsNames=NULL, verbose=FALSE, interactive=TRUE, arg.list=NULL, digits=4, latex=FALSE, exportFun=NULL, flag.pre.arg.list=FALSE,...){
	# FUNCTION: 47-1 different metrics
	pool <- table.Performance.pool()
	
#	extract metric functions' arguments
	ArgFormals <- lapply(pool,function(x)formals(x))
	ArgNames <- lapply(ArgFormals,function(x)names(x))
	ArgString.temp <- unique(unlist(ArgNames))
	ArgString <- sort(ArgString.temp[-which(ArgString.temp%in%c("R","x","..."))])	
	
	metrics.vec <- data.frame(
			metrics=pool,
			include=rep(0,length(pool)),
			metricsNames=pool,
			stringsAsFactors=FALSE)
	
	
#	loop through each metric and input the default values of args
	for (i in paste0("arg_",ArgString))
		eval(parse(text=paste0("metrics.vec$",i,"<- '#'")))
	
	
	for (i in 1:length(pool)){
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
		if(is.null(metrics.vec$metricsNames))
			metrics.vec$metricsNames[match(metrics,metrics.vec$metrics)] <- metricsNames
		metrics.vec <- metrics.vec[order(metrics.vec$include,decreasing=T),]
	}
#	open data editor	
	if(interactive & !is.null(arg.list))
		stop("Error: either uses interactive method or user input ArgList, not both")
	# TODO: some improvement can be made here
	
	if(interactive){
		metrics.vec <- fix(metrics.vec) #allow user to change the input
	} 
#   process the selected metrics and args	
	metrics.choose <- subset(metrics.vec,include==1)
	if(nrow(metrics.choose)==0) stop("please specify as least one metric")
	colnames(metrics.choose) <- gsub("arg_","",colnames(metrics.choose))
	metrics <- as.character(metrics.choose$metrics)
	metricsNames <-  as.character(metrics.choose$metricsNames)
	metricsOptArgVal <- 
			lapply(1:nrow(metrics.choose[,-c(1:3),drop=FALSE]),function(x){
#						x=2
						xx <- metrics.choose[x,-c(1:3),drop=FALSE]
						xx[is.na(xx)] <- "NA"
						xy <- as.vector(xx[xx!='#'])
						names(xy) <-  names(xx)[xx!='#']
						xy})
	
	
	names(metricsOptArgVal) <- metrics
	
	
	if(!is.null(arg.list)){
		if(!is.list(arg.list)) stop("Input argument arg.list should be a list")
		if(length(setdiff(names(arg.list),names(metricsOptArgVal)))!=0) 
			stop(paste("Mismatch: input argument arg.list for",paste(names(arg.list),collapse=","), ",  but input metrics are",paste(names(metricsOptArgVal),collapse=",")))
		if(!all(unlist(lapply(arg.list,names)) %in% unlist(lapply(metricsOptArgVal,names))))
			stop("Input argument arg.list doesn't match with argument metrics")
		metricsOptArgVal <- NULL
		
		# Extract the first argument and if it is NULL, replace with _NULL_
		arg.list <- lapply(arg.list,function(x){
					lapply(x,function(xx){
								t1 <- xx[1]
								if (is.null(t1))
									t1 <-"_NULL_"	
								t1
							})
					
				})
		
		
		# Convert arg.list to metricsOptArgVal
		metricsOptArgVal <- lapply(arg.list,function(x){
					t1 <- unlist(x)
					t2 <- suppressWarnings(sapply(t1,function(xx){
										if(is.na(as.numeric(xx)))
											paste0("'",xx,"'")
										else {xx}
									}))
#			 expected warning when checking wheter input is numeric convertable or not
					names(t2) <- names(x)
					t2
				})
		
		#  replace _NULL_ with NULL
		metricsOptArgVal <- lapply(metricsOptArgVal,function(x){
					lapply(x,function(xx){
								t1 <- xx[1]
								if (t1=="'_NULL_'")
									t1 <-"NULL"	
								t1
							})
					
				})
		
	} 
	
	if(flag.pre.arg.list & !is.null(arg.list)){
		cat("###################################","\n")
		cat("use previous stored optional argument\n")
		cat("###################################","\n")
	}
	
	
#   functions to call each metric function with user input args	
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
#			 metric=metrics[3]
				ArgString.i <- paste(names(metricsOptArgVal[[metric]]),metricsOptArgVal[[metric]],sep=" = ")
				
				ArgString.i <- paste(ArgString.i,collapse =", ")
				
				Arg.mat[[metric]] <- ArgString.i
				
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
		return(list(resultingtable=round(resultingtable,digits),
						Arg.mat=Arg.mat))
	}
	
#	generating the table	
	res <- table.Arbitrary.m()
#	res <- table.Arbitrary.m(...)
#	show printout	
	
	# convert Arg.mat to arg.list
	arg.list.string <- paste("list(",paste(metrics,"=",paste("list(",res$Arg.mat,")"),collapse=","),")")
	# save arg.list to arg.list.p
	eval(parse(text=paste("arg.list.p <- ",arg.list.string)))
	# take the first argument
	res$Arg.List <- lapply(arg.list.p,function(x){
				lapply(x,function(xx){
							t1 <- xx[1]
							if (is.null(t1))
								t1 <-"NULL"	
							t1
						})
				
			})
	
	res$Arg.mat <- NULL
	if(verbose){
		cat("###################################","\n")
		cat("metrics:\n")
		print(metrics)
		cat("###################################","\n")
		cat("metricsNames:\n")
		print(metricsNames)
		cat("###################################","\n")
		cat("arg.list:\n")
		cat("Attention: for more than one element in args, \n only the first one will be used","\n")
		print(res$Arg.List)
		cat("###################################","\n")
		cat("table:\n")
		print(res$resultingtable)
	}
	
	if(latex){
		require(xtable)
		cat("###################################","\n")
		cat("Latex code:\n")
		print(xtable(res$resultingtable,digits=digits,...))
	}
	
	if(!is.null(exportFun)){
		if(!is.character(exportFun))
			warning("exportFun is mis-specified")
		else{
			cat("###################################","\n")
			cat(paste0("Exporting function ",exportFun,"\n"))
			cat("###################################","\n")
			
			string1 <- paste(exportFun,"<<-","function(R,metrics=",'c(',paste(paste0("'",metrics,"'"),collapse=','),')',", metricsNames=",'c(',paste(paste0("'",metricsNames,"'"),collapse=','),')', ", verbose=FALSE, interactive=FALSE, arg.list=",arg.list.string,", digits=4, latex = FALSE, exportFun=NULL,...)")
			
			string2 <- paste("{table.Performance(R=R,metrics=metrics,metricsNames=metricsNames,verbose=verbose,interactive=interactive,arg.list=arg.list,digits=digits,latex=latex,exportFun=exportFun,flag.pre.arg.list=TRUE,...)}")
			
			
			eval(parse(text=paste0(string1,string2)))
			
			cat(paste0(string1,string2),"\n")
			cat("###################################","\n")
			
			metricsOptArgVal.export <<- metricsOptArgVal
			
		}
		
	}
	
	return(res)
}



