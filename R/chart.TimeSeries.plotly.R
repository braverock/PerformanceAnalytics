#' @rdname chart.TimeSeries
#' 

chart.TimeSeries.plotly <-
  function(R){
    R = checkData(R, method="data.frame")
    
    columns = ncol(R)
    rows = nrow(R)
    columnnames = colnames(R)
    
    plot <- plot_ly(R, mode = 'lines')
    
    for(i in 1:nline){
      plot <- add_trace(plot,
                        R[[i]],
                        name = colnames[i],
                        mode = 'lines')
    }
    
    return(plot)
  }