#' @rdname chart.TimeSeries
#' 

chart.TimeSeries.builtin <-
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
           yaxis.pct){
    
    y = checkData(R,method='xts')
    
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    
    
    # Date standarization if format is not specified
    if (is.null(date.format)){
      freq = periodicity(y)
      yr_eq <- ifelse(format(index(first(y)),format="%Y")==format(index(last(y)),format="%Y"),TRUE,FALSE)
      switch(freq$scale,
             seconds = { date.format = "%H:%M"},
             minute = { date.format = "%H:%M"},
             hourly = {date.format = "%d %H"},
             daily = {if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"},
             weekly = {if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"},
             monthly = {if (yr_eq) date.format = "%b" else date.format = "%b %y"},
             quarterly = {if (yr_eq) date.format = "%b" else date.format = "%b %y"},
             yearly = {date.format = "%Y"}
      )
    }
    
    
    # Needed for finding aligned dates for event lines and period areas
    rownames = as.Date(time(y))
    rownames = format(strptime(rownames,format = date.format.in), date.format)
    
    # If the Y-axis is ln
    logaxis = ""
    if(ylog) {
      logaxis = "y"
    }
    
    if(yaxis.pct)
      y = y * 100
    
    if(is.null(xlim[1])) # is.na or is.null?
      xlim = c(1,rows)
    if(is.null(ylim[1])){
      ylim = as.numeric(range(y, na.rm=TRUE))
    }
    
    # Check for y axis
    if(yaxis)
      yaxis.left = TRUE
    else
      yaxis.left = FALSE
    
    # Add the other titles
    if(is.null(main))
      main=columnnames[1]
    
    p <- plot.xts(x = y, 
                  y = NULL, 
                  col = colorset, 
                  main = main, 
                  ylim = ylim, 
                  yaxis.left = yaxis.left, 
                  yaxis.right = yaxis.right, 
                  grid.col = grid.color, 
                  legend.loc = NULL)
    
    if(!is.null(event.lines)) {
      
      event.ind = NULL
      for(event in 1:length(event.lines)){
        event.ind = c(event.ind, grep(event.lines[event], rownames))
      }
      number.event.labels = ((length(event.labels)-length(event.ind) + 1):length(event.labels))
      
      # Draw any areas in the background
      if(!is.null(period.areas)) {
        # build a list of specific dates to find from xts ranges given
        period.dat = lapply(period.areas,function(x,y) c(first(index(y[x])),last(index(y[x]))),y=y)
        period.ind = NULL
        # add event lines
        # get bold event labels
        opar <- par(font = 1)
        par(font = 2)
        p$Env$period.color <- period.color
        p <- addEventLines(xts(event.labels[number.event.labels], time(y)[event.ind]), 
                           srt = 90, offset = 1.2, pos = 2, lty = 2)
        for(period in 1:length(period.dat)){
          if(!is.na(period.dat[[period]][1]))
            p <- addPolygon(xts(matrix(c(min(y), max(y), min(y), max(y)), ncol = 2, byrow = TRUE), 
                                period.dat[[period]]), on = 1, col = period.color)
        }
        par(opar)
      }
    }
    
    # Draw a solid reference line at zero
    p$Env$element.color <- element.color
    p <- addSeries(xts(rep(0, rows), time(y)), col = element.color, on = 1)
    
    # Expand the attributes to #columns if fewer values are passed in
    # (e.g., only one), to allow the user to pass in line, type, or
    # symbol variations.
    if(length(lwd) < columns)
      lwd = rep(lwd,columns)
    if(length(lty) < columns)
      lty = rep(lty,columns)
    if(length(pch) < columns)
      pch = rep(pch,columns)
    
    if(!is.null(legend.loc)) {
      if(!hasArg(legend.names))
        legend.names <- columnnames
      # add legend
      p$Env$cex.legend <- cex.legend
      p <- addLegend(legend.loc, legend.names, 
                     lty = lty, lwd = lwd, cex = cex.legend)
    }
    
    return(p)
  }