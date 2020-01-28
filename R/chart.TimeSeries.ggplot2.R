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
    y = checkData(R,method='xts')
  
    # Extract basic info
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    
    y = data.frame(date=index(y),coredata(y))
    
    # Stack columns as rows and add variable role
    y = data.frame(y[1], utils::stack(y[2:ncol(y)]))
    col_names = colnames(y)
    
    col_names[2] = "value"
    col_names[3] = "variable"
    
    colnames(y) = col_names

    value = y[,"value"]
    date = y[,"date"]
    variable = y[,"variable"]
    
    # adjust of yaxis if in percentage
    if(yaxis.pct)
      value = value * 100
    # Plotting
    plot <- ggplot2::ggplot(y, ggplot2::aes(x = date, y = value)) +
      ggplot2::geom_line(ggplot2::aes(color = variable), size = lwd) +
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