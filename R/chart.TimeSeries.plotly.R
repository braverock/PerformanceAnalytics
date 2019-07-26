#' @rdname chart.TimeSeries
#' 

chart.TimeSeries.plotly <-
  function(R,main){
    R = checkData(R, method="xts")
    
    columns = ncol(R)
    rows = nrow(R)
    columnnames = colnames(R)
    
    date = index(R)
    
    R = checkData(R, method="data.frame")
    plot <- plot_ly(R, mode = 'lines')%>%
      layout(title = main)
    
    for(i in 1:columns){
      plot <- add_trace(plot,
                        y = R[[i]],
                        x = date,
                        mode = 'lines',
                        name=columnnames[i])
    }
    
    return(plot)
  }