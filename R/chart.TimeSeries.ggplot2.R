#' @rdname chart.TimeSeries
#'

chart.TimeSeries.ggplot2 <-
  function(R,
           auto.grid,
           xaxis, yaxis,
           yaxis.right,
           type,
           lty,
           lwd,
           las,
           main,
           ylab,
           xlab,
           date.format.in,
           date.format,
           xlim,
           ylim,
           element.color,
           event.lines,
           event.labels,
           period.areas,
           event.color,
           period.color,
           colorset,
           pch,
           legend.loc,
           ylog,
           cex.axis,
           cex.legend,
           cex.lab,
           cex.labels,
           cex.main,
           major.ticks,
           minor.ticks,
           grid.color,
           grid.lty,
           xaxis.labels,
           plot.engine,
           yaxis.pct) {
    R <- checkData(R, method = "xts")

    # Stack columns as rows and add security role
    R <- data.frame(date = index(R), coredata(R))
    R <- data.frame(R[1], utils::stack(R[2:ncol(R)]))

    col_names <- colnames(R)
    col_names[2] <- "returns"
    col_names[3] <- "security"

    colnames(R) <- col_names

    # adjust y-axis units if requested
    if (yaxis.pct) {
      R[["returns"]] <- R[["returns"]] * 100
    }

    # drop missing observations to avoid geom_line warnings
    R <- stats::na.omit(R)

    if (nrow(R) == 0) {
      stop("No observations available for plotting after removing missing values.")
    }
    # Plotting
    p <- ggplot2::ggplot(R, ggplot2::aes(x = date, y = returns, color = security))

    if (type == "h" || type == "bar") {
      # Use stat="identity" to plot the literal return values as bars, mapped to fill rather than color
      p <- p + ggplot2::geom_bar(stat = "identity", position = "dodge", ggplot2::aes(fill = security))
    } else {
      p <- p + ggplot2::geom_line(linewidth = lwd)
    }

    p <- p + ggplot2::ggtitle(main)

    if (ylog) {
      p <- p + ggplot2::scale_y_continuous(trans = "log10")
    }

    if (!is.null(date.format)) {
      if (inherits(date, "POSIXt")) {
        p <- p + ggplot2::scale_x_datetime(date_labels = date.format)
      } else {
        p <- p + ggplot2::scale_x_date(date_labels = date.format)
      }
    }

    if (mode(colorset) == "character") {
      p <- p + ggplot2::scale_color_brewer(palette = colorset)
    }

    # format xlim and ylim
    if (!is.null(xlim[1])) { # is.na or is.null?
      p <- p + ggplot2::xlim(xlim)
    }
    if (!is.null(ylim[1])) {
      p <- p + ggplot2::ylim(ylim)
    }

    # draw lines and add title
    # grid line format
    p <- p + ggplot2::theme(
      panel.grid = ggplot2::element_line(
        colour = grid.color,
        linetype = grid.lty
      )
    )

    # set legend position
    p <- p + ggplot2::theme(legend.position = legend.loc)
    p <- p + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)


    return(p)
  }
