#' @rdname chart.TimeSeries
chart.TimeSeries.base <-
function (R, 
          auto.grid=TRUE, 
          xaxis = TRUE, 
          yaxis = TRUE, 
          yaxis.right = FALSE, 
          type = "l", 
          lty = 1, 
          lwd = 2, 
          las = par("las"),
          main = NULL, 
          ylab=NULL, 
          xlab="", 
          date.format.in="%Y-%m-%d", 
          date.format = NULL, 
          xlim = NULL, 
          ylim = NULL, 
          element.color="darkgray", 
          event.lines = NULL, 
          event.labels = NULL, 
          period.areas = NULL, 
          event.color = "darkgray", 
          period.color = "aliceblue", colorset = (1:12), 
          pch = (1:12), 
          legend.loc = NULL, 
          ylog = FALSE, 
          cex.axis=0.8, 
          cex.legend = 0.8, 
          cex.lab = 1, 
          cex.labels = 0.8, 
          cex.main = 1, 
          major.ticks='auto', 
          minor.ticks=TRUE, 
          grid.color="lightgray", 
          grid.lty="dotted", 
          xaxis.labels = NULL,
          yaxis.pct = FALSE, ...)
{ # @author Peter Carl, Brian Peterson

    # DESCRIPTION:
    # Draws a line chart and labels the x-axis with the appropriate dates.
    # This is really a "primitive", since it constructs the elements of a plot
    # to provide lines for each column of data provided.  

    # Inputs:
    # R = assumes that data is a regular time series, not irregular.  Can take
    # any type of object, whether a matrix, data frame, or timeSeries.
    # date.format: allows passing of a date format for the xaxis
    # legend.loc = use this to locate the legend, e.g., "topright"
    # colorset = use the name of any of the palattes above
    # reference.grid = if true, draws a grid aligned with the points on the
    #    x and y axes.
    # xaxis = if true, draws the x axis.
    # event.lines = if not null, will draw vertical lines indicating that an
    #    event happened during that time period.  event.lines should be a list
    #    of dates (e.g., c("09/03","05/06")) formatted the same as date.format.
    #    This function matches the re-formatted row names (dates) with the
    #    events.list, so to get a match the formatting needs to be correct.
    # event.labels = if not null and event.lines is not null, this will apply
    #    labels to the vertical lines drawn.

    # All other inputs are the same as "plot" and are principally included
    # so that some sensible defaults could be set.

    # Output:
    # Draws a timeseries graph of type "line" with some sensible defaults.

    # FUNCTION:

    y = checkData(R)

    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)

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

    time.scale = periodicity(y)$scale
    ep = axTicksByTime(y,major.ticks, format.labels = date.format)

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

    # par("usr"): A vector of the form 'c(x1, x2, y1, y2)' giving the extremes
    #           of the user coordinates of the plotting region.  When a
    #           logarithmic scale is in use (i.e., 'par("xlog")' is true, see
    #           below), then the x-limits will be '10 ^ par("usr")[1:2]'.
    #           Similarly for the y-axis.

    if(yaxis)
      yaxis.left = TRUE
    else
      yaxis.left = FALSE
    
    # Add the other titles
    if(is.null(main))
      main=columnnames[1]
    
    p <- plot.xts(x = y, 
                  y = NULL, 
                  ..., 
                  col = colorset, 
                  type = type, 
                  lty = lty, 
                  lwd = lwd, 
                  main = main, 
                  ylim = ylim, 
                  yaxis.left = yaxis.left, 
                  yaxis.right = yaxis.right, 
                  major.ticks = major.ticks, 
                  minor.ticks = minor.ticks, 
                  grid.ticks.lty = grid.lty, 
                  grid.col = grid.color, 
                  legend.loc = NULL, 
                  pch = pch)
    
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
                           srt = 90, offset = 1.2, pos = 2, lty = 2, ...)
        for(period in 1:length(period.dat)){
          if(!is.na(period.dat[[period]][1]))
            p <- addPolygon(xts(matrix(c(min(y), max(y), min(y), max(y)), ncol = 2, byrow = TRUE), 
                                period.dat[[period]]), on = 1, col = period.color, ...)
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
                     lty = lty, lwd = lwd, cex = cex.legend, ...)
    }
    
    return(p)
}


# New plotting engine function
chart.TimeSeries.multi_engine.base <-
  function(passon_list){
    
    # Unpack the passed on data from main function
    y = passon_list[[1]]
    
    auto.grid = passon_list[[2]]
    xaxis = passon_list[[3]]
    yaxis = passon_list[[4]]
    yaxis.right = passon_list[[5]]
    type = passon_list[[6]]
    lty = passon_list[[7]]
    lwd = passon_list[[8]]
    las = passon_list[[9]]
    main = passon_list[[10]]
    ylab = passon_list[[11]]
    xlab = passon_list[[12]] 
    date.format.in = passon_list[[13]] 
    date.format = passon_list[[14]]
    xlim = passon_list[[15]]
    ylim = passon_list[[16]]
    element.color = passon_list[[17]] 
    event.lines = passon_list[[18]]
    event.labels = passon_list[[19]]
    period.areas = passon_list[[20]]
    
    event.color = passon_list[[21]]
    period.color = passon_list[[22]]
    colorset = passon_list[[23]]
    
    pch = passon_list[[24]]
    legend.loc = passon_list[[25]] 
    ylog = passon_list[[26]] 
    cex.axis = passon_list[[27]]
    cex.legend = passon_list[[28]] 
    cex.lab = passon_list[[29]]
    cex.labels = passon_list[[30]]
    cex.main = passon_list[[31]]
    
    major.ticks = passon_list[[32]]
    minor.ticks = passon_list[[33]]
    grid.color = passon_list[[34]] 
    grid.lty = passon_list[[35]]
    xaxis.labels = passon_list[[36]]
    plot_engine = passon_list[[37]]
    yaxis.pct = passon_list[[38]]
    
    
    if(plot_engine == "ggplot"){
      plot <- ggplot(y, aes(x = date, y = value)) + 
        geom_line(aes(color = variable), size = 1) +
        ggtitle(main)
    
      return(plot)
    }
    
    
    if(plot_engine == "dyplotGraph"){
      
      print("in function")
      
      columns = ncol(y)
      rows = nrow(y)
      columnnames = colnames(y)
      
      print("parameters extracted")
      
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

      print("date formatted")

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
                             srt = 90, offset = 1.2, pos = 2, lty = 2, ...)
          for(period in 1:length(period.dat)){
            if(!is.na(period.dat[[period]][1]))
              p <- addPolygon(xts(matrix(c(min(y), max(y), min(y), max(y)), ncol = 2, byrow = TRUE), 
                                  period.dat[[period]]), on = 1, col = period.color, ...)
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
                       lty = lty, lwd = lwd, cex = cex.legend, ...)
      }
      
      return(p)
    }
  }

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2018 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.TimeSeries.R 3579 2018-01-07 13:01:25Z braverock $
#
###############################################################################
