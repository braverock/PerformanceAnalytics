#' @rdname chart.TimeSeries
#'

chart.TimeSeries.dygraph <-
  function(R, type = "l") {
    y <- checkData(R, method = "xts")
    plot <- dygraphs::dygraph(y)

    if (type == "h" || type == "bar") {
      plot <- dygraphs::dyBarChart(plot)
    }

    return(plot)
  }
