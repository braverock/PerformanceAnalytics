GLMSmoothIndex<-
  function(R = NULL, ...)
  {
    columns = 1
    columnnames = NULL
    #Error handling if R is not NULL
    if(!is.null(R)){
      x = checkData(R)
      columns = ncol(x)
      n = nrow(x)
      count = q
        x=edhec
        columns = ncol(x)
        columnnames = colnames(x)
        
        # Calculate AutoCorrelation Coefficient
        for(column in 1:columns) { # for each asset passed in as R
          y = checkData(x[,column], method="vector", na.rm = TRUE)
          sum = sum(abs(acf(y,plot=FALSE,lag.max=6)[[1]][2:7]));
          acflag6 = acf(y,plot=FALSE,lag.max=6)[[1]][2:7]/sum;
          values = sum(acflag6*acflag6)
          
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
        return(result.df)
      
    }  
    
    
    ###############################################################################
    # R (http://r-project.org/) Econometrics for Performance and Risk Analysis
    #
    # Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
    #
    # This R package is distributed under the terms of the GNU Public License (GPL)
    # for full details see the file COPYING
    #
    # $Id: Return.Geltner.R 2163 2012-07-16 00:30:19Z braverock $
    #
    ###############################################################################
    
  }