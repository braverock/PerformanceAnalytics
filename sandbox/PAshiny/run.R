# TODO: Add comment
# 
# Author: kirkli
###############################################################################


rm(list=ls())
# install.packages('devtools')
# install.packages("shiny")
# devtools::install_github('shiny-incubator', 'rstudio')
# install.packages("Rglpk")
# setwd("C:/Dropbox/doug/MVO")
if(!"devtools" %in% rownames(installed.packages())) {install.packages('devtools')}
# In linux, if error occurs, check R-Curl. 
if(!"shiny" %in% rownames(installed.packages())) {install.packages('shiny')}
if(!"shinyIncubator" %in% rownames(installed.packages())) {devtools::install_github("shiny-incubator", "rstudio")}
if(!"Rglpk" %in% rownames(installed.packages())) {install.packages('Rglpk')}
if(!"xts" %in% rownames(installed.packages())) {install.packages('xts')}
if(!"corpcor" %in% rownames(installed.packages())) {install.packages('corpcor')}
if(!"quadprog" %in% rownames(installed.packages())) {install.packages('quadprog')}

library(shinyIncubator)
library(shiny)
library(quadprog)
library(Rglpk)
library(xts)

# ##
# load("crsp.short.Rdata")
# 
# allcap.ts <- merge(merge(smallcap.ts,midcap.ts),largecap.ts)
# stock.names.list <- c(names(smallcap.ts)[1:2],names(midcap.ts)[1:2],names(largecap.ts)[1:2])
# files <- list(data=allcap.ts[,stock.names.list],group=rep(c(1,2,3),each=2))
# save(files,file="crsp.short.6.Rdata")
# 
# load("crsp.short.6.Rdata")
# files$data
# write.csv(as.data.frame(files$data),file="crsp.short.6.csv",row.names=TRUE)
# write.csv(files$group,file="crsp.short.6.group.csv",row.names=FALSE)
#test <- read.csv("crsp.short.6.csv")
#test2 <- read.csv("crsp.short.6.group.csv")
# 
# head(test)
# head(test2)

result.folder <<- getwd()
code.folder <<- getwd()
setwd(code.folder)
runApp()











