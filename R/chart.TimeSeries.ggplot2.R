#' @rdname chart.TimeSeries
#'

chart.TimeSeries.ggplot2<-
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
            yaxis.pct)

{

    R <- checkData(R, method='xts')

    # Stack columns as rows and add security role
    R <- data.frame(date=index(R), coredata(R))
    R <- data.frame(R[1], utils::stack(R[2:ncol(R)]))

    col_names = colnames(R)
    col_names[2] = "returns"
    col_names[3] = "security"

    colnames(R) = col_names

    returns <- R[,"returns"]
    date <- R[,"date"]
    security <- R[,"security"]

    # adjust of yaxis if in percentage
    if(yaxis.pct)
      returns = returns * 100
    # Plotting
    p <- ggplot2::ggplot(R, ggplot2::aes(x = date, y = returns, color = security)) +
                               ggplot2::geom_line(size = lwd) +
                               ggplot2::ggtitle(main)



    if(mode(colorset) == "character"){
      p <- p + ggplot2::scale_color_brewer(palette=colorset)
    }

    # format xlim and ylim
    if(!is.null(xlim[1])) # is.na or is.null?
      p <- p + ggplot2::xlim(xlim)
    if(!is.null(ylim[1])){
      p <- p + ggplot2::ylim(ylim)
    }

    #draw lines and add title
    # grid line format
    p <- p + ggplot2::theme(
                    panel.grid = ggplot2::element_line(colour = grid.color,
                                                        linetype = grid.lty)
                        )

    # set legend position
    p <-p + ggplot2::theme(legend.position = legend.loc)
    p <- p + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)


    return(p)
  }
