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
            plot_engine,
            yaxis.pct)

{
    y = checkData(R,method='xts')
  
    # Extract basic info
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    
    y = data.frame(date=index(y),coredata(y))
    
    # Stack columns as rows and add variable role
    y = data.frame(y[1], stack(y[2:ncol(y)]))
    col_names = colnames(y)
    
    col_names[2] = "value"
    col_names[3] = "variable"
    
    colnames(y) = col_names

    # Plotting
    plot <- ggplot(y, aes(x = date, y = value)) +
      geom_line(aes(color = variable), size = lwd) +
      ggtitle(main)

    # adjust of yaxis if in percentage
    if(yaxis.pct)
      y = y * 100

    # format xlim and ylim
    if(!is.null(xlim[1])) # is.na or is.null?
      plot+xlim(xlim)
    if(!is.null(ylim[1])){
      plot+ylim(ylim)
    }

    #draw lines and add title
    # grid line format
    plot + theme(
      panel.grid = element_line(colour = grid.color,
                                linetype = grid.lty)
    )

    # set legend position
    plot + theme(legend.position = legend.loc)
    plot + xlab(xlab) +ylab(ylab)


    return(plot)
  }