#' @rdname chart.TimeSeries
#'

chart.TimeSeries.googlevis <-
  function(R,
           xlab, ylab,
           main,
           type = "l") {
    y <- data.frame(date = index(R), coredata(R))

    options_list <- list(
      gvis.editor = "Edit",
      xvar = xlab,
      yvar = ylab,
      title = main
    )

    if (type == "h" || type == "bar") {
      plot <- googleVis::gvisColumnChart(y, options = options_list)
    } else {
      plot <- googleVis::gvisLineChart(y, options = options_list)
    }

    return(plot)
  }
