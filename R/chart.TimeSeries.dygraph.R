#' @rdname chart.TimeSeries
#' 

chart.TimeSeries.dygraph <-
  function(R){
    y = checkData(R,method='xts')
    plot <- dygraph(y)
    
    return(plot)
  }