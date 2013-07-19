chart.REDD<-function(R,rf,h, geometric = TRUE,legend.loc = NULL, colorset = (1:12),...)
{
#DESCRIPTION:
#A function to create the chart for the rolling economic drawdown
#
  # calculates the Rolling Economic Drawdown(REDD) for
  # a return series.To calculate the rolling economic drawdown cumulative 
  # return and rolling economic max is calculated for each point. The risk 
  # free return(rf) and the lookback period(h) is taken as the input.
 

    rolldrawdown = rollDrawdown(R,geometric = TRUE,weights = NULL,rf,h)
    chart.TimeSeries(rolldrawdown, colorset = colorset, legend.loc = legend.loc, ...)
}


