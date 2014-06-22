# TODO: Add comment
# 
# Author: KirkLi
###############################################################################


library(shiny)

source("table.Performance.R")
source("chooser.R")
count <- 0
# Define server logic for random distribution application
shinyServer(function(input, output) {
			
			
			inputTextarea <- function(inputId,  label="",value="", nrows=10, ncols=10) {
				tagList(
						singleton(tags$head(tags$script(src = "textarea.js"))),
						tags$label(label, `for` = inputId),
						tags$textarea(id = inputId,
								class = "inputtextarea",
								rows = nrows,
								cols = ncols,
								as.character(value))
				)
			}
			
			output$text <- renderPrint({
#						if(input$goButton1==0) {
						if(is.null(input$file1) & input$dataset)
							cat("Please load data.\n")
#						if (input$dataset==F)
#							cat("Example csv file: \n")
						else{
							if(!is.null(input$file1))cat("first 10 rows of the return data: \n")}
#						}
#						else{    
						
#						}
					}
			)
			
#			output$contents <- renderDataTable({
##						if(input$goButton1==0) {
##							test <- read.csv("crsp.short.6.csv")
##							head(test)
##						}
##						else{
#						if(input$dataset==F){
#							mydata <- mydata.raw <- read.csv("crsp.short.6.csv")
#							rownames(mydata) <- mydata[,1]
#							colnames(mydata.raw)[1] <- "date"
#							mydata <- mydata[,-1]
#							mydata <<- mydata
#							date <- mydata.raw[,1]
#							cbind(date,round(mydata,2))
#						}else{
#							if(!is.null(input$file1)){
#								inFile1 <- input$file1
#								mydata <- mydata.raw <- read.csv(
#										inFile1$datapath, 
#										header=input$header1, 
#										sep=input$sep1, 
#										quote=input$quote1)
#								rownames(mydata) <- mydata[,1]
#								colnames(mydata.raw)[1] <- "date"
#								mydata <- mydata[,-1]
#								mydata <<- mydata
#								date <- mydata.raw[,1]
#								cbind(date,round(mydata,2))
#							}
#							
#						}
##						}
#					})
			
			output$summary <-  renderDataTable({
#						if(input$goButton1==0) {
#							test <- read.csv("crsp.short.6.csv")
#							head(test)
#						}
#						else{
						if(input$dataset==F){
							mydata <- mydata.raw <- read.csv("crsp.short.6.csv")
							rownames(mydata) <- mydata[,1]
							colnames(mydata.raw)[1] <- "date"
							mydata <- mydata[,-1]
							mydata <<- mydata
							date <- mydata.raw[,1]
							cbind(date,round(mydata,2))
						}else{
							if(!is.null(input$file1)){
								inFile1 <- input$file1
								mydata <- mydata.raw <- read.csv(
										inFile1$datapath, 
										header=input$header1, 
										sep=input$sep1, 
										quote=input$quote1)
								rownames(mydata) <- mydata[,1]
								colnames(mydata.raw)[1] <- "date"
								mydata <- mydata[,-1]
								mydata <<- mydata
								date <- mydata.raw[,1]
								cbind(date,round(mydata,2))}
							
						}
#						}
					})
			
			metric.list <- reactive({
						if(length(input$mychooser$right)>=1)
							table.Performance.input.shiny(metrics=input$mychooser$right)
						else return()
					})
#		
			nmetric<- reactive({
						length(metric.list())
					})
#		
			
#			metric.list <-function()table.Performance.input.shiny(metrics="BernardoLedoitRatio")
#			
#			metric.list()
#			
			
			output$selection <- renderPrint(
#					nrows <<- length(input$mychooser$right)
					input$mychooser$right
			)
			
#			
			##			metric.list <- function() table.Performance.input.shiny(metrics="ES")
			##			metric.list()
			
			
			ct=1:50
			eval(parse(text=paste0("output$para.",ct," <- renderUI({
											if(length(input$mychooser$right)>= ",ct," ){
											count <- ",ct,"
											inputId = eval(parse(text=paste0('\"para.',count,'\"')))
											label= eval(parse(text=paste0('paste0(names(metric.list())[',count,'],\":\")')))
											value= eval(parse(text=paste0('paste0(names(metric.list()[[',count,']]),\"=\",metric.list()[[',count,']],collapse=\"\n\")')))
											if(nchar(value)>=2 & length(value)>0) # colum sign 
											inputTextarea(inputId,label,value,nrow=5,ncol=10)
											else return()
											
											}
											else return()
											})")))
			
			
			eval(parse(text=paste0("metric.list.m.",ct,"<- reactive({if(length(input$mychooser$right)>=",ct,"){if(length(input$para.",ct,")>0){
											l1 <- unlist(strsplit(input$para.",ct,",'\n'))
											l1 <- strsplit(l1,'=')
											temp <- metric.list()[[",ct,"]]
											temp[unlist(lapply(l1,'[[',1))]	<- 					unlist(lapply(l1,'[[',2))} else return()
											temp
											} 
											else{return()}
											})")))
			
			##			
			
			output$result <-renderDataTable({
						metrics <- input$mychooser$right
						if(length(input$mychooser$right)>0){
							metricsOptArgVal <-list()
							string.use <- 
									paste0("list(",paste0("metric.list.m.",1:50,"()",collapse=","),")")
							metricsOptArgVal <- eval(parse(text=string.use						
									))
							
							names(metricsOptArgVal) <- metrics
							res <<- table.Performance.output.shiny(R=mydata,metricsOptArgVal= metricsOptArgVal,metrics=metrics,metricsNames=NULL)
							cbind(metrics,res) } else return()
					})
			
			
#			output$diag2 <- renderPrint({
#						metrics <- input$mychooser$right
#						string.use <- paste0("list(",paste0("metric.list.m.",1:50,"()",collapse=","),")")
#						metricsOptArgVal <- eval(parse(text=string.use						
#						))
#						
#							names(metricsOptArgVal) <- metrics
#							metricsOptArgVal
#					}
#			)
			
			
			output$downloadData <- downloadHandler(
					# This function returns a string which tells the client
					# browser what name to use when saving the file.
					filename = function() {
						paste0("PerformanceMetricTable_",Sys.Date(),".",input$filetype)
					},
					
					# This function should write data to a file given to it by
					# the argument 'file'.
					content = function(file) {
						sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
						# Write to a file specified by the 'file' argument
						write.table(res, file, sep = sep,
								row.names = TRUE,col.names=NA)
					})
			
			
		})


