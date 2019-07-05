source("C:/Users/yz_ze/Documents/GitHub/PerformanceAnalytics/R/chart.TimeSeries.base.r")
source("C:/Users/yz_ze/Documents/GitHub/PerformanceAnalytics/R/chart.TimeSeries.r")

# library(PerformanceAnalytics)
library(xts)
library(zoo)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(dygraphs)

#easy test & package validation
data <- read.csv('C:/Users/yz_ze/Box/2019 Spring - ChicagoAlphaModelingLLC/Week11/expandData.csv')
data <- xts(data[1:48,-1], order.by = as.Date(as.character(data[1:48,1]), format = "%m/%d/%Y"))
 
y = data.frame(date=index(data),coredata(data))
 y <- y %>%
   gather(key = "variable", value = "value", -date)

# y = data.frame(date=index(data),coredata(data))
# 
# 
# nline = ncol(y)
# plot <- plot_ly(y, mode = 'lines')
# 
# for(i in 2:nline){
#   plot <- add_trace(plot,
#                     x = y[["date"]],
#                     y = y[[i]],
#                     mode = 'lines')
# }

chart.TimeSeries.multi_engine(data,plot_engine = "googlevis",
                              main="Demo",
                              xlab="date")
