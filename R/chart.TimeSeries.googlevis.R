#' @rdname chart.TimeSeries
#' 

chart.TimeSeries.googlevis <-
  function(R,
           xlab,ylab,
           main){
    y = data.frame(date=index(R),coredata(R))
    
    plot <- gvisLineChart(y,
                          options=list(
                            gvis.editor="Edit",
                            xvar=xlab,
                            yvar=ylab,
                            title=main)
    )
    
    plot(plot)
  }