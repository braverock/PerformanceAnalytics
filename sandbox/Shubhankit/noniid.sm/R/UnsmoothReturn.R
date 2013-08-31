UnSmoothReturn<-
  function(R = NULL,q,  ...)
  {
    columns = 1
    columnnames = NULL
    #Error handling if R is not NULL
    if(!is.null(R)){
      x = checkData(R)
      columns = ncol(x)
      n = nrow(x)
      count = q
      columns = ncol(x)
      columnnames = colnames(x)
      
      # Calculate AutoCorrelation Coefficient
      for(column in 1:columns) { # for each asset passed in as R
        y = checkData(R[,column], method="vector", na.rm = TRUE)
        
        acflag6 = acf(y,plot=FALSE,lag.max=6)[[1]][2:7]
        values = sum(acflag6*acflag6)/(sum(acflag6)*sum(acflag6))
        
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
      return(result.df[1:q,]*R)  # Unsmooth Return
      
    }  
  }