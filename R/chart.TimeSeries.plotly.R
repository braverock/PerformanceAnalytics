#' @rdname chart.TimeSeries
#'

chart.TimeSeries.plotly <-
  function(R,
           main,
           type = "l",
           ylog = FALSE,
           ...) {
    R <- checkData(R, method = "xts")
    columns <- ncol(R)
    rows <- nrow(R)
    columnnames <- colnames(R)

    date <- index(R)

    R <- checkData(R, method = "data.frame")

    pl_mode <- if (type == "h" || type == "bar") "none" else "lines"
    pl_type <- if (type == "h" || type == "bar") "bar" else "scatter"

    plot <- plotly::layout(plotly::plot_ly(R, type = pl_type, mode = pl_mode), title = main)

    if (ylog) {
      plot <- plotly::layout(plot, yaxis = list(type = "log"))
    }


    for (i in 1:columns) {
      plot <- plotly::add_trace(plot,
        y = R[[i]],
        x = date,
        type = pl_type,
        mode = pl_mode,
        name = columnnames[i]
      )
    }

    return(plot)
  }
