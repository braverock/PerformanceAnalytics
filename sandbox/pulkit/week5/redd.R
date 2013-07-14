#'@title Calculate the Rolling Economic Drawdown
#'
#'@description
#'\code{rollDrawdown} calculates the Rolling Economic Drawdown(REDD) for
#' a return series.To calculate the rolling economic drawdown cumulative 
#' return and rolling economic max is calculated for each point. The risk 
#' free return(rf) and the lookback period(h) is taken as the input. 
#'
#'@param R an xts, vector, matrix, data frame, timeseries, or zoo object of asset return.
#'@param weights portfolio weighting vector, default NULL
#'@param geometric utilize geometric chaining (TRUE) or  simple/arithmetic chaining(FALSE)
#'to aggregate returns, default is TRUE
#'@param rf risk free rate can be vector such as government security rate of return
#'@param h lookback period 
#'@param \dots any other passthru variable
#'@references Yang, Z. George and Zhong, Liang, Optimal Portfolio Strategy to 
#'Control Maximum Drawdown - The Case of Risk Based Dynamic Asset Allocation (February 25, 2012)
#' @export
rollDrawdown<-function(R,Rf,h, geometric = TRUE, weights = NULL,...)
{
  
  # DESCRIPTION:
  # calculates the Rolling Economic Drawdown(REDD) for
  # a return series.To calculate the rolling economic drawdown cumulative 
  # return and rolling economic max is calculated for each point. The risk 
  # free return(rf) and the lookback period(h) is taken as the input.
  
  # FUNCTION:
    x = checkData(R)
    columns = ncol(x)
    rowx = nrow(x)
    columnnames = colnames(x)
    rf = checkData(rf)
    rowr = nrow(rf)
    if(rowr != 1 ){
        if(rowr != rowx){
            warning("The number of rows of the returns and the risk free rate do not match")
        }
    }
    REDD<-function(x,geometric){
        if(geometric)
            Return.cumulative = cumprod(1+x)
        else Return.cumulative = 1 + cumsum(x)
        l = length(Return.cumulative)
        REM = max(Return.cumulative*(1+rf)^(l-c(1:l)))
        result = 1 - Return.cumulative[l]/REM
    }

    for(column in 1:columns){
        column.drawdown <- apply.rolling(x[,column],width = h, FUN = REDD, geometric = geometric)
        if(column == 1)
            rolldrawdown = column.drawdown
        else rolldrawdown = merge(rolldrawdown, column.drawdown) 
    }
    colnames(rolldrawdown) = columnnames
    rolldrawdown = reclass(rolldrawdown, x)
    return(rolldrawdown)
}
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










  


