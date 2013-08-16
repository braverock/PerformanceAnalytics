#' @title
#' Table for displaying the Mximum Drawdown and the Time under Water
#'
#' @description
#' \code{table.Penance} Displays the table showing mean,Standard Deviation , phi, sigma , MaxDD,time at which MaxDD occurs, MaxTuW and the penance.
#'  
#' @param R Returns
#' @param confidence the confidence interval
#' 
#' @reference Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).

table.Penance<-function(R,confidence){
  # DESCRIPTION:
  # Maximum Drawdown and Time under Water considering first-order serial correlation
  # 
  # Input:
  # R log returns 
  # 
  # Output:
  # Creates a Table showing mean stdDev phi sigma MaxDD t* MaxTuW and Penance
  #
  # Function:
  x = checkData(R)
  columns = ncol(x) 
  columnnames = colnames(x)
  rownames = c("mean","stdDev","phi","sigma","MaxDD(in %)","t*","MaxTuW","Penance")
  for(column in 1:columns){
    phi = cov(x[,column][-1],x[,column][-length(x[,column])])/(cov(x[,column][-length(x[,column])]))
    sigma_infinity = StdDev(x[,column])
    sigma = sigma_infinity*((1-phi^2)^0.5)
    column_MinQ<-c(mean(x[,column]),sigma_infinity,phi,sigma)
    column_MinQ <- c(column_MinQ,get_minq(x[,column],confidence))
    column_TuW = get_TuW(x[,column],confidence)
    v = c(column_MinQ,column_TuW,column_MinQ[5]/column_TuW)
    if(column == 1){
      result = data.frame(Value = v, row.names = rownames)
    }
    else{
      nextcolumn = data.frame(Value = v,row.names = rownames)
      result = cbind(result,nextcolumn)
    }
  }
  colnames(result) = columnnames
  result
}
