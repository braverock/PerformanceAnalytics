#' Active Premium or Active Return
#' 
#' The return on an investment's annualized return minus the benchmark's
#' annualized return.
#' 
#' Active Premium = Investment's annualized return - Benchmark's annualized
#' return
#' 
#' Also commonly referred to as 'active return'.
#' 
#' @param Ra return vector of the portfolio
#' @param Rb return vector of the benchmark asset
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @author Peter Carl
#' @seealso \code{\link{InformationRatio}} \code{\link{TrackingError}}
#' \code{\link{Return.annualized}}
#' @references Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio
#' Management},Fall 1994, 49-58.
#' @keywords ts multivariate distribution models
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