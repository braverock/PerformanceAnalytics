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
    
    require(ggplot2) 
  
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
    plot <- ggplot2::ggplot(R, ggplot2::aes(x = date, y = returns, color = security)) +
                               ggplot2::geom_line(size = lwd) +
                               ggplot2::ggtitle(main)



    # format xlim and ylim
    if(!is.null(xlim[1])) # is.na or is.null?
      plot+xlim(xlim)
    if(!is.null(ylim[1])){
      plot+ylim(ylim)
    }

    #draw lines and add title
    # grid line format
    plot + ggplot2::theme(
                    panel.grid = ggplot2::element_line(colour = grid.color,
                                                        linetype = grid.lty)
                        )

    # set legend position
    plot + ggplot2::theme(legend.position = legend.loc)
    plot + xlab(xlab) +ylab(ylab)


    return(plot)
  }