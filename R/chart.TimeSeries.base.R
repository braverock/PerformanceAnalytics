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
           plot_engine,
           yaxis.pct,
           ...){
    
    # Unpack the passed on data from main function
    # R = passon_list[[1]]
    # 
    # auto.grid = passon_list[[2]]
    # xaxis = passon_list[[3]]
    # yaxis = passon_list[[4]]
    # yaxis.right = passon_list[[5]]
    # type = passon_list[[6]]
    # lty = passon_list[[7]]
    # lwd = passon_list[[8]]
    # las = passon_list[[9]]
    # main = passon_list[[10]]
    # ylab = passon_list[[11]]
    # xlab = passon_list[[12]] 
    # date.format.in = passon_list[[13]] 
    # date.format = passon_list[[14]]
    # xlim = passon_list[[15]]
    # ylim = passon_list[[16]]
    # element.color = passon_list[[17]] 
    # event.lines = passon_list[[18]]
    # event.labels = passon_list[[19]]
    # period.areas = passon_list[[20]]
    # 
    # event.color = passon_list[[21]]
    # period.color = passon_list[[22]]
    # colorset = passon_list[[23]]
    # 
    # pch = passon_list[[24]]
    # legend.loc = passon_list[[25]] 
    # ylog = passon_list[[26]] 
    # cex.axis = passon_list[[27]]
    # cex.legend = passon_list[[28]] 
    # cex.lab = passon_list[[29]]
    # cex.labels = passon_list[[30]]
    # cex.main = passon_list[[31]]
    # 
    # major.ticks = passon_list[[32]]
    # minor.ticks = passon_list[[33]]
    # grid.color = passon_list[[34]] 
    # grid.lty = passon_list[[35]]
    # xaxis.labels = passon_list[[36]]
    # plot_engine = passon_list[[37]]
    # yaxis.pct = passon_list[[38]]
    
    #Switch to check for plot engine and direct to respective sub-functions
    
    switch(plot_engine,
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
