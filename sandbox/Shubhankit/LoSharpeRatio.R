LoSharpeRatio<-
  function(R = NULL,Rf=0.,q = 0., ...)
  {
columns = 1
columnnames = NULL
#Error handling if R is not NULL
if(!is.null(R)){
  x = checkData(R)
  columns = ncol(x)
  n = nrow(x)
  
  if(q==0){
    stop("AutoCorrelation Coefficient Should be greater than 0")
    
  }
  else{
    # A potfolio is constructed by applying the weights
    
    count = q
    x=edhec
    columns = ncol(x)
    columnnames = colnames(x)
    
    # Calculate AutoCorrelation Coefficient
    for(column in 1:columns) { # for each asset passed in as R
      y = checkData(edhec[,column], method="vector", na.rm = TRUE)
      
      acflag6 = acf(y,plot=FALSE,lag.max=6)[[1]][2:7]
      LjungBox =  Box.test(y,type="Ljung-Box",lag=q)
      values = c(acflag6, LjungBox$p.value)
      # values = base::round(as.numeric(values),digits)
      
      if(column == 1) {
        result.df = data.frame(Value = values)
        colnames(result.df) = columnnames[column]
      }
      else {
        nextcol = data.frame(Value = values)
        colnames(nextcol) = columnnames[column]
        result.df = cbind(result.df, nextcol)
      }
    }
    # Calculate Neta's
    for(column in 1:columns) {
      sum = 0
      z = checkData(edhec[,column], method="vector", na.rm = TRUE)
    for(q in 1:(q-1) )
    {
      sum = sum + (count-q)*result.df[column,q]
    
    }
      
      netaq = count/(sqrt(count+2*sum))
      if(column == 1) {
        netacol = data.frame(Value = netaq)
        colnames(netacol) = columnnames[column]
      }
      else {
          nextcol = data.frame(Value = netaq)
        colnames(nextcol) = columnnames[column]
        netacol = cbind(netacol, nextcol)
      }
      
    }
    shrp = SharpeRatio(x, Rf, FUN="VaR" , method="gaussian")
    results = Shrp*netacol
    colnames(results) = colnames(x)
    return(results)
  }
  }  
}