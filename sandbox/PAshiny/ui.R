# TODO: Add comment
# 
# Author: KirkLi
###############################################################################

library(shiny)
library(shinyIncubator)

source("table.Performance.R")
source("chooser.R")
# Define UI for random distribution application 



shinyUI(fluidPage(
				
				titlePanel("Performance Metric"),
				fluidRow(
				column(4, wellPanel(
						h3("Input Data"),
						checkboxInput("dataset", "Load user dataset? Otherwise use example dataset that can be found in Summary Tab"),
						textOutput('text'),
						conditionalPanel(
										condition = "input.dataset == true",
										fileInput('file1', 'Choose Data File in CSV Format',
												accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
										checkboxInput('header1', 'Header', TRUE),
										radioButtons('sep1', 'Separator',
												c(Comma=',',
														Semicolon=';',
														Tab='t'),
												'Comma'),
										radioButtons('quote1', 'Quote',
												c(None='',
														'Double Quote'='"',
														'Single Quote'="'"),
												'Double Quote'),
										br()		
								),
#						actionButton("goButton1", "Go!"),
								
#     p("Otherwise just click 'Go' and load your own data"),
#						dataTableOutput('contents'),
						tags$hr(),
								h3("Select Metrics"),
								chooserInput("mychooser", "Available metric", "Selected metric",table.Performance.pool(), c(), size = 50, multiple = TRUE
								)
#						verbatimTextOutput("diag1"),
#						,verbatimTextOutput("diag2")
#						verbatimTextOutput("aaa")
						
						
)),
			
			column(3,wellPanel(
							h3("Modify Optional Arguments"),
							uiOutput("para.1"),uiOutput("para.2"),uiOutput("para.3"),uiOutput("para.4"),uiOutput("para.5"),uiOutput("para.6"),uiOutput("para.7"),uiOutput("para.8"),uiOutput("para.9"),uiOutput("para.10"),uiOutput("para.11"),uiOutput("para.12"),uiOutput("para.13"),uiOutput("para.14"),uiOutput("para.15"),uiOutput("para.16"),uiOutput("para.17"),uiOutput("para.18"),uiOutput("para.19"),uiOutput("para.20"),uiOutput("para.21"),uiOutput("para.22"),uiOutput("para.23"),uiOutput("para.24"),uiOutput("para.25"),uiOutput("para.26"),uiOutput("para.27"),uiOutput("para.28"),uiOutput("para.29"),uiOutput("para.30"),uiOutput("para.31"),uiOutput("para.32"),uiOutput("para.33"),uiOutput("para.34"),uiOutput("para.35"),uiOutput("para.36"),uiOutput("para.37"),uiOutput("para.38"),uiOutput("para.39"),uiOutput("para.40"),uiOutput("para.41"),uiOutput("para.42"),uiOutput("para.43"),uiOutput("para.44"),uiOutput("para.45"),uiOutput("para.46"),uiOutput("para.47"),uiOutput("para.48"),uiOutput("para.49"),uiOutput("para.50")
							)),

#				titlePanel("Tabsets"),
						
						# Sidebar with controls to select the random distribution type
						# and number of observations to generate. Note the use of the br()
						# element to introduce extra vertical spacing
						# Show a tabset that includes a plot, summary, and table view
						# of the generated distribution
				
			column(5, wellPanel(
							h3("Result"),
								tabsetPanel(type = "tabs", 
										tabPanel("Table", 
												h3("Performance Table"),
												dataTableOutput('result'),
												radioButtons("filetype", "File type:",
														choices = c("csv", "tsv")),
												downloadButton('downloadData', 'Download')), 
										tabPanel("Summary", dataTableOutput("summary"))
								)
						)))))
