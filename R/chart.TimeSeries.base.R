#' @rdname chart.TimeSeries


# New plotting engine function
chart.TimeSeries.base <-
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
           yaxis.pct,
           ...){
    
    #Switch to check for plot engine and direct to respective sub-functions
    switch(plot.engine,
           default = {
             plot = 
             chart.TimeSeries.builtin(R=R,
                                      auto.grid=auto.grid, 
                                      xaxis=xaxis, yaxis=yaxis, 
                                      yaxis.right=yaxis.right, 
                                      type=type, 
                                      lty=lty, 
                                      lwd=lwd, 
                                      las=las,
                                      main=main, 
                                      ylab=ylab, 
                                      xlab=xlab, 
                                      date.format.in=date.format.in, 
                                      date.format=date.format, 
                                      xlim=xlim, 
                                      ylim=ylim, 
                                      element.color=element.color, 
                                      event.lines=event.lines, 
                                      event.labels=event.labels, 
                                      period.areas=period.areas, 
                                      event.color=event.color, 
                                      period.color=period.color, 
                                      colorset=colorset, 
                                      pch=pch, 
                                      legend.loc=legend.loc, 
                                      ylog=ylog, 
                                      cex.axis=cex.axis, 
                                      cex.legend=cex.legend, 
                                      cex.lab=cex.lab, 
                                      cex.labels=cex.labels, 
                                      cex.main=cex.main, 
                                      major.ticks=major.ticks, 
                                      minor.ticks=minor.ticks, 
                                      grid.color=grid.color, 
                                      grid.lty=grid.lty, 
                                      xaxis.labels=xaxis.labels,
                                      yaxis.pct=yaxis.pct)
           },
           ggplot = {
             plot = 
               chart.TimeSeries.ggplot(R=R,
                                        auto.grid=auto.grid, 
                                        xaxis=xaxis, yaxis=yaxis, 
                                        yaxis.right=yaxis.right, 
                                        type=type, 
                                        lty=lty, 
                                        lwd=lwd, 
                                        las=las,
                                        main=main, 
                                        ylab=ylab, 
                                        xlab=xlab, 
                                        date.format.in=date.format.in, 
                                        date.format=date.format, 
                                        xlim=xlim, 
                                        ylim=ylim, 
                                        element.color=element.color, 
                                        event.lines=event.lines, 
                                        event.labels=event.labels, 
                                        period.areas=period.areas, 
                                        event.color=event.color, 
                                        period.color=period.color, 
                                        colorset=colorset, 
                                        pch=pch, 
                                        legend.loc=legend.loc, 
                                        ylog=ylog, 
                                        cex.axis=cex.axis, 
                                        cex.legend=cex.legend, 
                                        cex.lab=cex.lab, 
                                        cex.labels=cex.labels, 
                                        cex.main=cex.main, 
                                        major.ticks=major.ticks, 
                                        minor.ticks=minor.ticks, 
                                        grid.color=grid.color, 
                                        grid.lty=grid.lty, 
                                        xaxis.labels=xaxis.labels,
                                        yaxis.pct=yaxis.pct)
           },
           plotly = {
             plot = chart.TimeSeries.plotly(R=R,
                                            main=main)
           },
           googlevis = {
             chart.TimeSeries.googlevis(R=R,
                                        xlab=xlab,
                                        ylab=ylab,
                                        main=main)
           },
           dygraph = {
             plot = chart.TimeSeries.dygraph(R=R)
           }
    )
    
    #End Switch
    
    return(plot)
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
