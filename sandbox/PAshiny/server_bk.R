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
						if(is.null(input$file1))
							cat("Please load data.\n")
							if (input$dataset==F)
							cat("Example csv file: \n")
						else{
						if(!is.null(input$file1))cat("first 10 rows of the return data: \n")}
#						}
#						else{    
							
#						}
			}
	)
			
			output$contents <- renderTable({
#						if(input$goButton1==0) {
#							test <- read.csv("crsp.short.6.csv")
#							head(test)
#						}
#						else{
							if(input$dataset==F){
								mydata1 <<- read.csv("crsp.short.6.csv")
								rownames(mydata1) <- mydata1[,1]
								mydata <<- mydata1[,-1]
								head(mydata)
							}else{
								if(!is.null(input$file1)){
								inFile1 <- input$file1
								mydata <- read.csv(
										inFile1$datapath, 
										header=input$header1, 
										sep=input$sep1, 
										quote=input$quote1)
								rownames(mydata) <- mydata[,1]
								mydata <<- mydata[,-1]
								head(mydata)}
						
							}
#						}
					})
			
			output$summary <-  renderTable({mydata})
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
		
			
			output$para.1 <- renderUI({
						if(length(input$mychooser$right)>=1){
							count <- 1
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
							inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})
			
			
			output$para.2 <- renderUI({
						if(length(input$mychooser$right)>=2){
							count <- 2
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
							inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
	
			
			output$para.3 <- renderUI({
						if(length(input$mychooser$right)>=3){
							count <- 3
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
							inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		

			output$para.4 <- renderUI({
						if(length(input$mychooser$right)>=4){
							count <- 4
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
							inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			output$para.5 <- renderUI({
						if(length(input$mychooser$right)>= 5 ){
							count <- 5
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
							inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			
			output$para.6 <- renderUI({
						if(length(input$mychooser$right)>= 6 ){
							count <- 6
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			output$para.7 <- renderUI({
						if(length(input$mychooser$right)>= 7 ){
							count <- 7
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			output$para.8 <- renderUI({
						if(length(input$mychooser$right)>= 8 ){
							count <- 8
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			output$para.9 <- renderUI({
						if(length(input$mychooser$right)>= 9 ){
							count <- 9
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			output$para.10 <- renderUI({
						if(length(input$mychooser$right)>= 10 ){
							count <- 10
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			output$para.11 <- renderUI({
						if(length(input$mychooser$right)>= 11 ){
							count <- 11
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			output$para.12 <- renderUI({
						if(length(input$mychooser$right)>= 12 ){
							count <- 12
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			output$para.13 <- renderUI({
						if(length(input$mychooser$right)>= 13 ){
							count <- 13
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			output$para.14 <- renderUI({
						if(length(input$mychooser$right)>= 14 ){
							count <- 14
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			output$para.15 <- renderUI({
						if(length(input$mychooser$right)>= 15 ){
							count <- 15
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
			output$para.16 <- renderUI({
						if(length(input$mychooser$right)>= 16 ){
							count <- 16
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			output$para.17 <- renderUI({
						if(length(input$mychooser$right)>= 17 ){
							count <- 17
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			output$para.18 <- renderUI({
						if(length(input$mychooser$right)>= 18){
							count <- 18
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			output$para.19 <- renderUI({
						if(length(input$mychooser$right)>= 19 ){
							count <- 19
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			output$para.20 <- renderUI({
						if(length(input$mychooser$right)>= 20 ){
							count <- 20
							inputId = eval(parse(text=paste0("'para.",count,"'")))
							label= eval(parse(text=paste0("paste0(names(metric.list())[",count,"],':')")))
							value= eval(parse(text=paste0("paste0(names(metric.list()[[",count,"]]),':',metric.list()[[",count,"]],collapse='\n')")))
							if(nchar(value)>=2 & length(value)>0) # colum sign 
								inputTextarea(inputId,label,value,nrow=10,ncol=10)
							else return()
							
						}
						else return()
					})		
			
#			metric.list <- function() table.Performance.input.shiny(metrics="ES")
#			metric.list()
			metric.list.m.1 <- reactive({
					if(length(input$mychooser$right)>=1){
					if(length(input$para.1)>0){
					l1 <- unlist(strsplit(input$para.1,"\n"))
					l1 <- strsplit(l1,":")
					temp <- metric.list()[[1]]
					temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
					temp
				} 
				else{return()}
					})
			
			
			metric.list.m.2 <- reactive({
						if(length(input$mychooser$right)>=2){
							if(length(input$para.2)>0){
								l1 <- unlist(strsplit(input$para.2,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[2]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})
			
			metric.list.m.3 <- reactive({
						if(length(input$mychooser$right)>=3){
							if( length(input$para.3)>0){
								l1 <- unlist(strsplit(input$para.3,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[3]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			
			metric.list.m.4 <- reactive({
						if(length(input$mychooser$right)>=4){
							if(length(input$para.4)>0){
								l1 <- unlist(strsplit(input$para.4,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[4]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			
			metric.list.m.5 <- reactive({
						if(length(input$mychooser$right)>=5){
							if(length(input$para.5)>0){
								l1 <- unlist(strsplit(input$para.5,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[5]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			
			metric.list.m.6 <- reactive({
						if(length(input$mychooser$right)>= 6){
							if(length(input$para.6)>0){
								l1 <- unlist(strsplit(input$para.6,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 6 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.7 <- reactive({
						if(length(input$mychooser$right)>= 7){
							if(length(input$para.7)>0){
								l1 <- unlist(strsplit(input$para.7,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 7 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.8 <- reactive({
						if(length(input$mychooser$right)>= 8){
							if(length(input$para.8)>0){
								l1 <- unlist(strsplit(input$para.8,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 8 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.9 <- reactive({
						if(length(input$mychooser$right)>= 9){
							if(length(input$para.9)>0){
								l1 <- unlist(strsplit(input$para.9,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 9 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			
			metric.list.m.10 <- reactive({
						if(length(input$mychooser$right)>= 10){
							if(length(input$para.10)>0){
								l1 <- unlist(strsplit(input$para.10,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 10 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})					
			metric.list.m.11 <- reactive({
								if(length(input$mychooser$right)>= 11){
									if(length(input$para.11)>0){
										l1 <- unlist(strsplit(input$para.11,"\n"))
										l1 <- strsplit(l1,":")
										temp <- metric.list()[[ 11 ]]
										temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
									temp} else{return()}
							})		
			metric.list.m.12 <- reactive({
						if(length(input$mychooser$right)>= 12){
							if(length(input$para.12)>0){
								l1 <- unlist(strsplit(input$para.12,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 12 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.13 <- reactive({
						if(length(input$mychooser$right)>= 13){
							if(length(input$para.13)>0){
								l1 <- unlist(strsplit(input$para.13,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 13 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.14 <- reactive({
						if(length(input$mychooser$right)>= 14){
							if(length(input$para.14)>0){
								l1 <- unlist(strsplit(input$para.14,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 14 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.15 <- reactive({
						if(length(input$mychooser$right)>= 15){
							if(length(input$para.15)>0){
								l1 <- unlist(strsplit(input$para.15,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[15]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.16 <- reactive({
						if(length(input$mychooser$right)>= 16){
							if(length(input$para.16)>0){
								l1 <- unlist(strsplit(input$para.16,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 16 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.17 <- reactive({
						if(length(input$mychooser$right)>= 17){
							if(length(input$para.17)>0){
								l1 <- unlist(strsplit(input$para.17,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 17 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.18 <- reactive({
						if(length(input$mychooser$right)>= 18){
							if(length(input$para.18)>0){
								l1 <- unlist(strsplit(input$para.18,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 18 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.19 <- reactive({
						if(length(input$mychooser$right)>= 19){
							if(length(input$para.19)>0){
								l1 <- unlist(strsplit(input$para.19,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 19 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.20 <- reactive({
						if(length(input$mychooser$right)>= 20){
							if(length(input$para.20)>0){
								l1 <- unlist(strsplit(input$para.20,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 20 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			
			metric.list.m.21 <- reactive({
						if(length(input$mychooser$right)>= 21){
							if(length(input$para.21)>0){
								l1 <- unlist(strsplit(input$para.21,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[21]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.22 <- reactive({
						if(length(input$mychooser$right)>= 22){
							if(length(input$para.22)>0){
								l1 <- unlist(strsplit(input$para.22,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 22 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.23 <- reactive({
						if(length(input$mychooser$right)>= 23){
							if(length(input$para.23)>0){
								l1 <- unlist(strsplit(input$para.23,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 23 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.24 <- reactive({
						if(length(input$mychooser$right)>= 24){
							if(length(input$para.24)>0){
								l1 <- unlist(strsplit(input$para.24,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 24 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.25 <- reactive({
						if(length(input$mychooser$right)>= 25){
							if(length(input$para.25)>0){
								l1 <- unlist(strsplit(input$para.25,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 25 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.26 <- reactive({
						if(length(input$mychooser$right)>= 26){
							if(length(input$para.26)>0){
								l1 <- unlist(strsplit(input$para.26,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 26 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})		
			metric.list.m.26 <- reactive({
						if(length(input$mychooser$right)>= 26){
							if(length(input$para.26)>0){
								l1 <- unlist(strsplit(input$para.26,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 26 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.27 <- reactive({
						if(length(input$mychooser$right)>= 27){
							if(length(input$para.27)>0){
								l1 <- unlist(strsplit(input$para.27,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 27 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.28 <- reactive({
						if(length(input$mychooser$right)>= 28){
							if(length(input$para.28)>0){
								l1 <- unlist(strsplit(input$para.28,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 28 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.29 <- reactive({
						if(length(input$mychooser$right)>= 29){
							if(length(input$para.29)>0){
								l1 <- unlist(strsplit(input$para.29,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 29 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.30 <- reactive({
						if(length(input$mychooser$right)>= 30){
							if(length(input$para.26)>0){
								l1 <- unlist(strsplit(input$para.30,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 30 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.31 <- reactive({
						if(length(input$mychooser$right)>= 31){
							if(length(input$para.26)>0){
								l1 <- unlist(strsplit(input$para.31,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 31 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.32 <- reactive({
						if(length(input$mychooser$right)>= 32){
							if(length(input$para.32)>0){
								l1 <- unlist(strsplit(input$para.32,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 32 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.33 <- reactive({
						if(length(input$mychooser$right)>= 33){
							if(length(input$para.33)>0){
								l1 <- unlist(strsplit(input$para.33,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 33 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.34 <- reactive({
						if(length(input$mychooser$right)>= 34){
							if(length(input$para.34)>0){
								l1 <- unlist(strsplit(input$para.34,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 34 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.35 <- reactive({
						if(length(input$mychooser$right)>= 35){
							if(length(input$para.35)>0){
								l1 <- unlist(strsplit(input$para.35,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 35 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			
			metric.list.m.36 <- reactive({
						if(length(input$mychooser$right)>= 36){
							if(length(input$para.36)>0){
								l1 <- unlist(strsplit(input$para.36,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 36 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.37 <- reactive({
						if(length(input$mychooser$right)>= 37){
							if(length(input$para.37)>0){
								l1 <- unlist(strsplit(input$para.37,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 37 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			metric.list.m.50 <- reactive({
						if(length(input$mychooser$right)>= 50){
							if(length(input$para.50)>0){
								l1 <- unlist(strsplit(input$para.50,"\n"))
								l1 <- strsplit(l1,":")
								temp <- metric.list()[[ 50 ]]
								temp[unlist(lapply(l1,"[[",1))]	<- 					unlist(lapply(l1,"[[",2))} else return()
							temp} else{return()}
					})	
			
			
#			
#			output$diag1 <- renderPrint({
#						length(input$para.1)>0}
#			)
##			
#					output$diag2 <- renderPrint({
#							metricsOptArgVal <- list(
#									metric.list.m.1(),
#									metric.list.m.2(),
#									metric.list.m.3(),
#									metric.list.m.4(),
#									metric.list.m.5(),
#									metric.list.m.6(),
#									metric.list.m.7(),
#									metric.list.m.8(),
#									metric.list.m.9(),
#									metric.list.m.10(),
#									metric.list.m.11(),
#									metric.list.m.12(),
#									metric.list.m.13(),
#									metric.list.m.14(),
#									metric.list.m.15(),
#									metric.list.m.16(),
#									metric.list.m.17(),
#									metric.list.m.18(),
#									metric.list.m.19(),
#									metric.list.m.20()
#							)
#								names(metricsOptArgVal) <-input$mychooser$right
#								metricsOptArgVal}
#							)
#							
							
					output$result <-renderTable({
					metrics <- input$mychooser$right
					if(length(input$mychooser$right)>0){
					metricsOptArgVal <- list(
							metric.list.m.1(),
							metric.list.m.2(),
							metric.list.m.3(),
							metric.list.m.4(),
							metric.list.m.5(),
							metric.list.m.6(),
							metric.list.m.7(),
							metric.list.m.8(),
							metric.list.m.9(),
							metric.list.m.10(),
							metric.list.m.11(),
							metric.list.m.12(),
							metric.list.m.13(),
							metric.list.m.14(),
							metric.list.m.15(),
							metric.list.m.16(),
							metric.list.m.17(),
							metric.list.m.18(),
							metric.list.m.19(),
							metric.list.m.20()
							)
					names(metricsOptArgVal) <- metrics
#					save(metricsOptArgVal,file="tt.RData")
#					table.Performance.output.shiny(R=mydata,metricsOptArgVal= table.Performance.input.shiny("ES"),metrics=input$mychooser$right,metricsNames=NULL)
			table.Performance.output.shiny(R=mydata,metricsOptArgVal= metricsOptArgVal,metrics=metrics,metricsNames=NULL)} else return()
				})
		
		
		})
		
