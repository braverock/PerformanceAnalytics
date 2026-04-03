#' @rdname chart.TimeSeries
#'

chart.TimeSeries.googlevis <-
  function(R,
           xlab, ylab,
           main,
           type = "l",
           ylog = FALSE) {
    y <- data.frame(date = index(R), coredata(R))

    options_list <- list(
      gvis.editor = "Edit",
      xvar = xlab,
      yvar = ylab,
      title = main
    )

    if (ylog) {
      options_list$vAxis <- "{logScale:true}"
    }

    if (type == "h" || type == "bar") {
      plot <- googleVis::gvisColumnChart(y, options = options_list)
    } else {
      plot <- googleVis::gvisLineChart(y, options = options_list)
    }

    return(plot)
  }
