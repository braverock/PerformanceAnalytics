#' @rdname chart.TimeSeries
#'

chart.TimeSeries.dygraph <-
  function(R, type = "l", ylog = FALSE) {
    y <- checkData(R, method = "xts")
    plot <- dygraphs::dygraph(y)

    if (type == "h" || type == "bar") {
      plot <- dygraphs::dyBarChart(plot)
    }

    if (ylog) {
      plot <- dygraphs::dyAxis(plot, name = "y", logscale = TRUE)
    }

    return(plot)
  }
