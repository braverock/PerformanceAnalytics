#' Conditional Drawdown
#' 
#' A new one-parameter family of risk measures called Conditional Drawdown (CDD) has
#'been proposed. These measures of risk are functionals of the portfolio drawdown (underwater) curve considered in active portfolio management. For some value of the tolerance
#' parameter ??, in the case of a single sample path, drawdown functional is de???ned as
#'the mean of the worst (1 ??? ??) ??? 100% drawdowns. The CDD measure generalizes the
#'notion of the drawdown functional to a multi-scenario case and can be considered as a
#'generalization of deviation measure to a dynamic case. The CDD measure includes the
#'Maximal Drawdown and Average Drawdown as its limiting cases. 
#' 
#' @param Ra return vector of the portfolio
#' @param Rb return vector of the benchmark asset
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @author Peter Carl
#' @references DRAWDOWN MEASURE IN PORTFOLIO OPTIMIZATION,\emph{International Journal of Theoretical and Applied Finance}
#' ,Fall 1994, 49-58.Vol. 8, No. 1 (2005) 13-58
#' @keywords Conditional Drawdown models
#' @examples
#' 
#'     data(managers)
#'     ActivePremium(managers[, "HAM1", drop=FALSE], managers[, "SP500 TR", drop=FALSE])
#'     ActivePremium(managers[,1,drop=FALSE], managers[,8,drop=FALSE]) 
#'     ActivePremium(managers[,1:6], managers[,8,drop=FALSE]) 
#'     ActivePremium(managers[,1:6], managers[,8:7,drop=FALSE])
#' @rdname ActivePremium
#' @aliases ActivePremium, ActiveReturn
#' @export 

CDrawdown <-
  function (R,p=0.90, ...)
  {
    y = checkData(R, method = "xts")
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)
    
    for(column in 1:columns) {
      x = y[,column]
      drawdown = findDrawdowns(x)
      threshold= ES(x,p)[1]
      total = length(drawdown$return)
      num = length(drawdown$return[drawdown$return>threshold])
      cva1= (((num/total)-p)/(1-p))*threshold
      cva2=sum(drawdown$return)/((1-p)*total)
      z = c((cva1+cva2))
      znames = c("Conditional Drawdown at Risk")
      if(column == 1) {
        resultingtable = data.frame(Value = z, row.names = znames)
      }
      else {
        nextcolumn = data.frame(Value = z, row.names = znames)
        resultingtable = cbind(resultingtable, nextcolumn)
      }
      
    }
    colnames(resultingtable) = columnnames
    #ans = base::round(resultingtable, digits)
    #ans
    resultingtable
  }