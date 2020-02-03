#' @rdname chart.TimeSeries
#' 

chart.TimeSeries.dygraph <-
  function(R){
    y = checkData(R,method='xts')
    plot <- dygraphs::dygraph(y)
    
    return(plot)
  }