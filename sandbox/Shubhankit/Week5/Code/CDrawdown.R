CDrawdown <-
  function (R,p=0.95, ...)
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