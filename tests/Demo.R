source("D:/GSOC/PerformanceAnalytics/R/chart.TimeSeries.base.R")
source("D:/GSOC/PerformanceAnalytics/R/chart.TimeSeries.R")

library(PerformanceAnalytics)
library(xts)
library(zoo)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)

#easy test & package validation
data <- read.csv('C:/Users/yz_ze/Box/2019 Spring - ChicagoAlphaModelingLLC/Week11/expandData.csv')
data <- xts(data[1:48,-1], order.by = as.Date(as.character(data[1:48,1]), format = "%m/%d/%Y"))

y = data.frame(date=index(data),coredata(data))
y <- y %>%
  gather(key = "variable", value = "value", -date)

plot <- plot_ly(y,y=~value,x=~date, mode = 'lines')

return(plot)


chart.TimeSeries.multi_engine(data,plot_engine = "plotly")


